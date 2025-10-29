module Sexp = Sexplib.Sexp

module RuleParser = struct
  type rule =
    { lhs : Sexp.t
    ; rhs : Sexp.t
    ; cond : Sexp.t option
    }

  type expr_pat =
    | Var of string
    | Arg of string
    | Node of string
    | Ty of string
    | Op of string
    | Concrete of string * string (* e.g., (concrete Ty_list l) *)
    | Concrete_list of
        string * expr_pat list (* e.g., (concrete Ty_list [ ... ]) *)
    | App of string * expr_pat list (* e.g., (unop _ty Not x) *)

  type rule_pat =
    { lhs : expr_pat
    ; rhs : expr_pat
    ; cond : expr_pat option
    }

  let preprocess (s : string) : string =
    let buf = Buffer.create (String.length s) in
    String.iter
      (fun c ->
        match c with
        | '[' -> Buffer.add_string buf "(list "
        | ']' -> Buffer.add_char buf ')'
        | ',' -> Buffer.add_char buf ' '
        | _ -> Buffer.add_char buf c )
      s;
    Buffer.contents buf

  (* Parse a single top-level sexp into a rule *)
  let parse_rule (sexp : Sexp.t) : rule =
    match sexp with
    | Sexp.List [ lhs; Sexp.Atom "==>"; rhs ] -> { lhs; rhs; cond = None }
    | Sexp.List [ lhs; Sexp.Atom "==>"; rhs; Sexp.Atom "when"; cond ] ->
      { lhs; rhs; cond = Some cond }
    | _ ->
      let s = Sexp.to_string sexp in
      Fmt.failwith
        "Invalid rule shape (expected '(lhs ==> rhs)' or '(lhs ==> rhs when \
         cond)'): %s"
        s

  let remove_comments (s : string) : string =
    let lines = String.split_on_char '\n' s in
    let is_comment line = String.trim line |> String.starts_with ~prefix:";" in
    String.concat "\n" (List.filter (fun line -> not (is_comment line)) lines)

  let parse_rules_from_file (filename : string) : rule list =
    let raw = Bos.OS.File.read (Fpath.v filename) in
    let content =
      match raw with
      | Ok c -> c
      | Error (`Msg e) -> Fmt.failwith "Error reading rules file: %s" e
    in
    let preprocessed = preprocess content in
    let no_comments = remove_comments preprocessed in
    let lexbuf = Lexing.from_string no_comments in
    let sexps = Sexplib.Sexp.scan_sexps lexbuf in
    List.map parse_rule sexps

  let rec encode_args (args : Sexp.t) : expr_pat =
    match args with
    | Sexp.Atom v -> Arg v
    | Sexp.List (Sexp.Atom "list" :: rest) ->
      Concrete_list ("Ty_list", List.map encode_args rest)
    | Sexp.List (Sexp.Atom v :: args) -> App (v, List.map encode_args args)
    | _ -> Fmt.failwith "encode_args: Unsupported argument pattern"

  let rec sexp_to_expr_pat (sexp : Sexp.t) ~(inner : int) : expr_pat =
    match sexp with
    | Sexp.Atom v when inner <= 0 -> Node v
    | Sexp.List (Sexp.Atom "Val" :: rest) when inner <= 0 ->
      Node
        (Fmt.str "Val (%s)" (String.concat " " (List.map Sexp.to_string rest)))
    | Sexp.Atom v -> Var v
    | Sexp.List [ Sexp.Atom v ] -> Var v
    | Sexp.List [ Sexp.Atom "concrete"; Sexp.Atom ty; Sexp.Atom var ] ->
      Concrete (ty, var)
    | Sexp.List
        [ Sexp.Atom "concrete"; Sexp.Atom ty; Sexp.List [ Sexp.Atom "list" ] ]
      ->
      Concrete (ty, "[]")
    | Sexp.List
        [ Sexp.Atom "concrete"
        ; Sexp.Atom ty
        ; Sexp.List (Sexp.Atom "list" :: rest)
        ] ->
      Concrete_list (ty, List.map (sexp_to_expr_pat ~inner:(inner - 1)) rest)
    | Sexp.List
        [ Sexp.Atom "concrete"
        ; Sexp.Atom ty
        ; Sexp.List (Sexp.Atom func :: vars)
        ] ->
      Concrete
        ( ty
        , Fmt.str "(%s %s)" func
            (String.concat " " (List.map Sexp.to_string vars)) )
    | Sexp.List (Sexp.Atom "list" :: rest) ->
      Concrete_list
        ("Ty_list", List.map (sexp_to_expr_pat ~inner:(inner + 1)) rest)
    | Sexp.List (Sexp.Atom "unop" :: [ Sexp.Atom ty; Sexp.Atom op; arg ]) ->
      App ("unop", [ Ty ty; Op op; (sexp_to_expr_pat ~inner:(inner - 1)) arg ])
    | Sexp.List (Sexp.Atom "cvtop" :: [ Sexp.Atom ty; Sexp.Atom op; arg ]) ->
      App ("cvtop", [ Ty ty; Op op; (sexp_to_expr_pat ~inner:(inner - 1)) arg ])
    | Sexp.List (Sexp.Atom "binop" :: [ Sexp.Atom ty; Sexp.Atom op; arg1; arg2 ])
      ->
      App
        ( "binop"
        , [ Ty ty
          ; Op op
          ; (sexp_to_expr_pat ~inner:(inner - 1)) arg1
          ; (sexp_to_expr_pat ~inner:(inner - 1)) arg2
          ] )
    | Sexp.List (Sexp.Atom "relop" :: [ Sexp.Atom ty; Sexp.Atom op; arg1; arg2 ])
      ->
      App
        ( "relop"
        , [ Ty ty
          ; Op op
          ; (sexp_to_expr_pat ~inner:(inner - 1)) arg1
          ; (sexp_to_expr_pat ~inner:(inner - 1)) arg2
          ] )
    | Sexp.List
        (Sexp.Atom "triop" :: [ Sexp.Atom ty; Sexp.Atom op; arg1; arg2; arg3 ])
      ->
      App
        ( "triop"
        , [ Ty ty
          ; Op op
          ; (sexp_to_expr_pat ~inner:(inner - 1)) arg1
          ; (sexp_to_expr_pat ~inner:(inner - 1)) arg2
          ; (sexp_to_expr_pat ~inner:(inner - 1)) arg3
          ] )
    | Sexp.List (Sexp.Atom "naryop" :: Sexp.Atom ty :: Sexp.Atom op :: args) ->
      App
        ( "naryop"
        , Ty ty :: Op op :: List.map (sexp_to_expr_pat ~inner:(inner - 1)) args
        )
    | Sexp.List (Sexp.Atom func :: args) -> App (func, List.map encode_args args)
    | Sexp.List _ ->
      Fmt.failwith "Unsupported S-expression pattern: %s" (Sexp.to_string sexp)

  let rec _pp_expr_pat (p : expr_pat) : string =
    match p with
    | Var v -> Fmt.str "Var(%s)" v
    | Concrete (ty, var) -> Fmt.str "(concrete %s %s)" ty var
    | Concrete_list (ty, vars) ->
      let vars_str = List.map _pp_expr_pat vars |> String.concat "; " in
      Fmt.str "(concrete_list %s [ %s ])" ty vars_str
    | Ty ty -> Fmt.str "Ty(%s)" ty
    | Op op -> Fmt.str "Op(%s)" op
    | Node n -> Fmt.str "Node(%s)" n
    | Arg v -> Fmt.str "Arg(%s)" v
    | App (f, args) ->
      let args_str = List.map _pp_expr_pat args |> String.concat " " in
      Fmt.str "App (%s, [%s])" f args_str

  let sexp_to_rule_pat (r : rule) : rule_pat =
    { lhs = sexp_to_expr_pat r.lhs ~inner:2
    ; rhs = sexp_to_expr_pat r.rhs ~inner:2
    ; cond = Option.map (fun c -> sexp_to_expr_pat c ~inner:2) r.cond
    }

  let extract_simpl_type (p : expr_pat) : string option =
    match p with App (op, _) -> Some op | _ -> None

  let rec expr_pat_to_ocaml_pattern ?(top_level = false) (p : expr_pat) =
    match p with
    | Var v -> v
    | Node v -> Fmt.str "{node = %s; _}" v
    | Arg v -> v
    | Ty ty -> ty
    | Op op -> op
    | Concrete (_ty, var) -> Fmt.str "List %s" var
    | Concrete_list (_ty, vars) ->
      let vars_str =
        List.map (expr_pat_to_ocaml_pattern ~top_level:false) vars
        |> String.concat "; "
      in
      Fmt.str "[ %s ]" vars_str
    | App (s, [ arg ]) -> Fmt.str "%s (%s)" s (expr_pat_to_ocaml_pattern arg)
    | App (f, args) -> begin
      match args with
      | Ty ty :: Op op :: rest_args ->
        let constr = String.capitalize_ascii f in
        let arg_pats =
          rest_args
          |> List.map (fun a -> expr_pat_to_ocaml_pattern ~top_level:false a)
        in
        let res =
          if top_level then Fmt.str "%s, %s" op (String.concat ", " arg_pats)
          else
            Fmt.str "%s (%s, %s, %s)" constr ty op (String.concat ", " arg_pats)
        in
        res
      | _ ->
        Fmt.failwith
          "expr_pat_to_ocaml_pattern: Operator-family application with too few \
           args: %s"
          f
    end

  let rec expr_pat_to_ocaml_expr (p : expr_pat) : string =
    match p with
    | Var v -> Fmt.str "make %s" v
    | Node v -> Fmt.str "make %s" v
    | Arg v -> v
    | Ty ty -> ty
    | Op op -> op
    | Concrete (_ty, var) -> Fmt.str "list (%s)" var
    | Concrete_list (_ty, vars) ->
      let vars_str =
        List.map expr_pat_to_ocaml_expr vars |> String.concat "; "
      in
      Fmt.str "[ make %s ]" vars_str
    | App (s, []) -> s
    | App ("Bitv", [ Arg bv ]) -> Fmt.str "Bitv (Bitvector.of_int8 %s)" bv
    | App ("Val", rest) ->
      Fmt.str "value (%s)"
        (String.concat " " (List.map expr_pat_to_ocaml_expr rest))
    | App (s, [ arg ]) -> Fmt.str "(%s %s)" s (expr_pat_to_ocaml_expr arg)
    | App (s, rest) when String.starts_with ~prefix:"Eval." s ->
      Fmt.str "(%s %s)" s
        (String.concat " " (List.map expr_pat_to_ocaml_expr rest))
    | App (s, rest) when String.starts_with ~prefix:"List." s ->
      Fmt.str "%s %s" s
        (String.concat " " (List.map expr_pat_to_ocaml_expr rest))
    | App (f, args) -> begin
      match args with
      | Ty ty :: Op op :: rest_args ->
        let constr =
          String.concat "" [ "simplify_"; String.uncapitalize_ascii f ]
        in
        let arg_pats =
          rest_args
          |> List.map (fun a -> Fmt.str "(%s)" (expr_pat_to_ocaml_expr a))
        in
        let res =
          Fmt.str "%s %s %s %s" constr ty op (String.concat " " arg_pats)
        in
        res
      | _ -> Fmt.failwith "Operator-family application with too few args: %s" f
    end

  let cond_to_ocaml (cond_opt : expr_pat option) : string =
    match cond_opt with
    | None -> ""
    | Some c -> Fmt.str " when %s" (expr_pat_to_ocaml_expr c)

  let generate_match_arm (r : rule_pat) : string * string =
    let simpl_type = Option.value (extract_simpl_type r.lhs) ~default:"" in
    let lhs = expr_pat_to_ocaml_pattern ~top_level:true r.lhs in
    let rhs = expr_pat_to_ocaml_expr r.rhs in
    let when_clause = cond_to_ocaml r.cond in
    (simpl_type, Fmt.str "  | %s%s -> %s" lhs when_clause rhs)

  let is_variable (s : string) : bool =
    if String.length s = 0 then false
    else
      let c = String.get s 0 in
      (c >= 'a' && c <= 'z') || c = '_'

  exception UnsupportedLeanTheorem of string

  let lean_ty_constructor (ty_str : string) : string =
    match ty_str with
    | "Ty_int" -> "Int"
    | "Ty_str" -> "String"
    | "Ty_bool" -> "Bool"
    | "Ty_bitv" -> "Bitvec"
    | "Int" -> "Int"
    | "String" -> "String"
    | "Bool" -> "Bool"
    | "Prop" -> "Prop"
    | "ty" -> "ty"
    | "_ty" -> "_ty"
    | str when String.starts_with ~prefix:"List " str -> str
    | _ ->
      Fmt.epr "Unsupported %s\n" ty_str;
      raise (UnsupportedLeanTheorem (Fmt.str "Type constructor: %s" ty_str))

  let is_prop_pat (p : expr_pat) : bool =
    match p with
    | App ("unop", [ _; Op "Not"; _ ]) -> true
    | App ("binop", [ _; Op "And"; _ ]) -> true
    | App ("binop", [ _; Op "Or"; _ ]) -> true
    | App ("relop", _) -> true
    | App ("Val", [ Arg "True" ]) -> true
    | App ("Val", [ Arg "False" ]) -> true
    | _ -> false

  type lean_type_info =
    { generics :
        (string, string list) Hashtbl.t
    }

  let rec find_generics_and_classes (info : lean_type_info) (p : expr_pat) :
    unit =
    let is_real_generic (ty : string) =
      is_variable ty && ty <> "_" && not (String.starts_with ~prefix:"_ty" ty)
    in
    match p with
    | Ty ty when is_real_generic ty ->
      if not (Hashtbl.mem info.generics ty) then Hashtbl.add info.generics ty []
    | App ("relop", [ Ty ty; Op op; arg1; arg2 ]) ->
      ( if is_variable ty && ty <> "_" then
          let classes =
            Hashtbl.find_opt info.generics ty |> Option.value ~default:[]
          in
          let with_class =
            match op with
            | "Lt" | "Le" | "Gt" | "Ge" ->
              if List.mem "LinearOrder" classes then classes
              else "LinearOrder" :: classes
            | "Eq" | "Neq" ->
              if List.mem "DecidableEq" classes then classes
              else "DecidableEq" :: classes
            | _ -> classes
          in
          Hashtbl.replace info.generics ty with_class );
      find_generics_and_classes info (Ty ty);
      find_generics_and_classes info (Op op);
      find_generics_and_classes info arg1;
      find_generics_and_classes info arg2
    | App ("binop", [ Ty ty; Op op; arg1; arg2 ]) ->
      ( if is_variable ty && ty <> "_" then
          let classes =
            Hashtbl.find_opt info.generics ty |> Option.value ~default:[]
          in
          let with_class =
            match op with
            | "Add" | "Sub" | "Mul" ->
              if List.mem "Ring" classes then classes else "Ring" :: classes
            | _ -> classes
          in
          Hashtbl.replace info.generics ty with_class );
      find_generics_and_classes info (Ty ty);
      find_generics_and_classes info (Op op);
      find_generics_and_classes info arg1;
      find_generics_and_classes info arg2
    | App (_, args) -> List.iter (find_generics_and_classes info) args
    | Concrete_list (_, args) -> List.iter (find_generics_and_classes info) args
    | _ -> ()

  type var_map = (string, string) Hashtbl.t

  let rec has_bitv (p : expr_pat) : bool =
    match p with
    | App ("Val", [ App ("Bitv", _) ]) -> true
    | App (_, args) -> List.exists has_bitv args
    | Concrete_list (_, args) -> List.exists has_bitv args
    | _ -> false

  let rec find_var_types (vars : var_map) (current_ty : string) (p : expr_pat) :
    unit =
    match p with
    | (Node v | Var v | Arg v) when is_variable v ->
      if not (Hashtbl.mem vars v) then Hashtbl.add vars v current_ty
    | Ty _ | Op _ -> ()
    | App ("unop", [ Ty ty; Op op; arg ]) ->
      let ty_name = if is_variable ty then ty else lean_ty_constructor ty in
      let arg_ty =
        match op with "Reverse" -> Fmt.str "List %s" ty_name | _ -> ty_name
      in
      let final_ty = if has_bitv arg then "Bitvec" else arg_ty in
      find_var_types vars final_ty arg
    | App ("binop", [ Ty ty; Op _; arg1; arg2 ])
    | App ("relop", [ Ty ty; Op _; arg1; arg2 ]) ->
      let ty_name = if is_variable ty then ty else lean_ty_constructor ty in
      let final_ty =
        if has_bitv arg1 || has_bitv arg2 then "Bitvec" else ty_name
      in
      find_var_types vars final_ty arg1;
      find_var_types vars final_ty arg2
    | App ("triop", [ Ty _; Op "Ite"; c; t; e ]) ->
      find_var_types vars "Prop" c;
      find_var_types vars current_ty t;
      find_var_types vars current_ty e
    | App ("triop", [ Ty ty; Op op; arg1; arg2; arg3 ]) ->
      let ty_name = if is_variable ty then ty else lean_ty_constructor ty in
      let final_ty =
        if has_bitv arg1 || has_bitv arg2 || has_bitv arg3 || op = "Extract"
        then "Bitvec"
        else ty_name
      in
      find_var_types vars final_ty arg1;
      find_var_types vars final_ty arg2;
      find_var_types vars final_ty arg3
    | App ("naryop", [ Ty ty; Op "Concat"; arg_list ]) ->
      let ty_name = if is_variable ty then ty else lean_ty_constructor ty in
      let list_ty = Fmt.str "List %s" ty_name in
      let rec find_vars (p : expr_pat) (expected_ty : string) : unit =
        match p with
        | Var v | Arg v ->
          if is_variable v then Hashtbl.replace vars v expected_ty
        | App ("naryop", [ Ty _; Op "Concat"; Var v ]) ->
          if is_variable v then Hashtbl.replace vars v list_ty
        | App ("List.append", [ arg1; arg2 ]) ->
          find_vars arg1 list_ty;
          find_vars arg2 list_ty
        | App ("List.cons", [ arg1; arg2 ]) ->
          find_vars arg1 ty_name;
          find_vars arg2 list_ty
        | Concrete_list (_, args) ->
          List.iter (fun arg -> find_vars arg ty_name) args
        | _ -> ()
      in
      find_vars arg_list list_ty
    | App ("Val", [ App ("Eval.unop", [ _; _; v_arg ]) ]) ->
      find_var_types vars current_ty v_arg
    | App ("Val", [ App ("Eval.binop", [ _; _; v1_arg; v2_arg ]) ]) ->
      find_var_types vars current_ty v1_arg;
      find_var_types vars current_ty v2_arg
    | App ("Val", [ App ("Eval.triop", [ _; _; v1_arg; v2_arg; v3_arg ]) ]) ->
      find_var_types vars current_ty v1_arg;
      find_var_types vars current_ty v2_arg;
      find_var_types vars current_ty v3_arg
    | App ("Val", [ App ("Eval.naryop", [ _; _; v_args ]) ]) ->
      find_var_types vars current_ty v_args
    | App ("Val", [ Arg "True" ]) | App ("Val", [ Arg "False" ]) -> ()
    | App ("Val", [ App ("Int", [ Arg n ]) ]) ->
      if is_variable n then Hashtbl.replace vars n "Int"
    | App ("Val", [ App ("Bitv", [ Arg bv ]) ]) ->
      if is_variable bv then Hashtbl.replace vars bv "Bitvec"
    | App ("Bitvector.eqz", [ Arg bv ]) -> Hashtbl.replace vars bv "Bitvec"
    | App ("Bitvector.eq_one", [ Arg bv ]) -> Hashtbl.replace vars bv "Bitvec"
    | App (_, args) -> List.iter (find_var_types vars current_ty) args
    | _ -> ()

  let rec expr_pat_to_lean_notation (p : expr_pat) : string =
    match p with
    | Node v when String.starts_with ~prefix:"Val (" v ->
      String.sub v 5 (String.length v - 6)
    | Var v | Node v | Arg v -> v
    | Ty _ | Op _ -> ""
    (* Unop *)
    | App ("unop", [ _; Op "Not"; arg ]) ->
      Fmt.str "¬%s" (expr_pat_to_lean_notation arg)
    | App ("unop", [ _; Op "Neg"; arg ]) ->
      Fmt.str "¬%s" (expr_pat_to_lean_notation arg)
    | App (_, [ _; Op "Reverse"; arg ]) ->
      Fmt.str "List.reverse (%s)" (expr_pat_to_lean_notation arg)
    | App ("List.append", [ arg1; Concrete_list (_, [ arg2 ]) ]) ->
      Fmt.str "(%s ++ [%s])"
        (expr_pat_to_lean_notation arg1)
        (expr_pat_to_lean_notation arg2)
    | App ("List.append", [ arg1; arg2 ]) ->
      Fmt.str "(%s ++ %s)"
        (expr_pat_to_lean_notation arg1)
        (expr_pat_to_lean_notation arg2)
    | App ("List.cons", [ arg1; arg2 ]) ->
      Fmt.str "(%s :: %s)"
        (expr_pat_to_lean_notation arg1)
        (expr_pat_to_lean_notation arg2)
    | App ("naryop", [ Ty _; Op "Concat"; Var v ]) ->
      Fmt.str "(String.join %s)" v
    | App ("naryop", [ Ty _; Op "Concat"; arg ]) ->
      Fmt.str "(String.join (%s))" (expr_pat_to_lean_notation arg)
    (* Binop *)
    | App ("binop", [ Ty ty; Op "And"; arg1; arg2 ]) ->
      if lean_ty_constructor ty = "Bitvec" then
        Fmt.str "(%s) &&& (%s)"
          (expr_pat_to_lean_notation arg1)
          (expr_pat_to_lean_notation arg2)
      else (* Assume Bool *)
        Fmt.str "(%s) ∧ (%s)"
          (expr_pat_to_lean_notation arg1)
          (expr_pat_to_lean_notation arg2)
    | App ("binop", [ Ty ty; Op "Or"; arg1; arg2 ]) ->
      if lean_ty_constructor ty = "Bitvec" then
        Fmt.str "(%s) ||| (%s)"
          (expr_pat_to_lean_notation arg1)
          (expr_pat_to_lean_notation arg2)
      else (* Assume Bool *)
        Fmt.str "(%s) ∨ (%s)"
          (expr_pat_to_lean_notation arg1)
          (expr_pat_to_lean_notation arg2)
    | App ("binop", [ _; Op "Add"; arg1; arg2 ]) ->
      Fmt.str "(%s) + (%s)"
        (expr_pat_to_lean_notation arg1)
        (expr_pat_to_lean_notation arg2)
    | App ("binop", [ _; Op "Sub"; arg1; arg2 ]) ->
      Fmt.str "(%s) - (%s)"
        (expr_pat_to_lean_notation arg1)
        (expr_pat_to_lean_notation arg2)
    | App ("binop", [ _; Op "Mul"; arg1; arg2 ]) ->
      Fmt.str "(%s) * (%s)"
        (expr_pat_to_lean_notation arg1)
        (expr_pat_to_lean_notation arg2)
    (* Relop *)
    | App ("relop", [ _; Op "Eq"; arg1; arg2 ]) ->
      Fmt.str "((%s) = (%s))"
        (expr_pat_to_lean_notation arg1)
        (expr_pat_to_lean_notation arg2)
    | App ("relop", [ _; Op "Ne"; arg1; arg2 ]) ->
      Fmt.str "((%s) ≠ (%s))"
        (expr_pat_to_lean_notation arg1)
        (expr_pat_to_lean_notation arg2)
    | App ("relop", [ _; Op "Lt"; arg1; arg2 ]) ->
      Fmt.str "((%s) < (%s))"
        (expr_pat_to_lean_notation arg1)
        (expr_pat_to_lean_notation arg2)
    | App ("relop", [ _; Op "Le"; arg1; arg2 ]) ->
      Fmt.str "((%s) ≤ (%s))"
        (expr_pat_to_lean_notation arg1)
        (expr_pat_to_lean_notation arg2)
    | App ("relop", [ _; Op "Gt"; arg1; arg2 ]) ->
      Fmt.str "((%s) > (%s))"
        (expr_pat_to_lean_notation arg1)
        (expr_pat_to_lean_notation arg2)
    | App ("relop", [ _; Op "Ge"; arg1; arg2 ]) ->
      Fmt.str "((%s) ≥ (%s))"
        (expr_pat_to_lean_notation arg1)
        (expr_pat_to_lean_notation arg2)
    (* Triop *)
    | App ("triop", [ _; Op "Ite"; c; t; e ]) ->
      Fmt.str "(if %s then %s else %s)"
        (expr_pat_to_lean_notation c)
        (expr_pat_to_lean_notation t)
        (expr_pat_to_lean_notation e)
    (* Bool values *)
    | App ("Val", [ Arg "True" ]) -> "True"
    | App ("Val", [ Arg "False" ]) -> "False"
    | App ("Val", [ App ("Int", [ Arg n ]) ]) -> n
    | App ("Val", [ App ("Bitv", [ Arg bv ]) ]) -> bv
    | App ("Val", [ App ("Eval.binop", [ _; Arg "Add"; arg1; arg2 ]) ]) ->
      Fmt.str "(%s) + (%s)"
        (expr_pat_to_lean_notation arg1)
        (expr_pat_to_lean_notation arg2)
    | App ("Val", [ App ("Eval.binop", [ _; Arg "Sub"; arg1; arg2 ]) ]) ->
      Fmt.str "(%s) - (%s)"
        (expr_pat_to_lean_notation arg1)
        (expr_pat_to_lean_notation arg2)
    | App ("Val", [ App ("Eval.binop", [ _; Arg "Mul"; arg1; arg2 ]) ]) ->
      Fmt.str "(%s) * (%s)"
        (expr_pat_to_lean_notation arg1)
        (expr_pat_to_lean_notation arg2)
    | App ("Val", [ Arg v ]) when is_variable v -> v
    | App ("Bitvector.eqz", [ Arg bv ]) -> Fmt.str "(%s = 0)" bv
    | App ("Bitvector.eq_one", [ Arg bv ]) -> Fmt.str "(%s = 1)" bv
    | Concrete_list (_, vars) ->
      let vars_str =
        List.map expr_pat_to_lean_notation vars |> String.concat ", "
      in
      Fmt.str "[%s]" vars_str
    | App (f, _) when String.starts_with ~prefix:"List." f ->
      raise (UnsupportedLeanTheorem (Fmt.str "OCaml List function: %s" f))
    | App (f, _) when String.starts_with ~prefix:"Bitvector." f ->
      raise (UnsupportedLeanTheorem (Fmt.str "OCaml Bitvector function: %s" f))
    | App (f, _) when String.starts_with ~prefix:"Eval." f ->
      raise (UnsupportedLeanTheorem (Fmt.str "OCaml Eval function: %s" f))
    | App (_, [ _; Op "Head"; _ ]) -> raise (UnsupportedLeanTheorem "Head op")
    | App (_, [ _; Op "Tail"; _ ]) -> raise (UnsupportedLeanTheorem "Tail op")
    | App (_, [ _; Op "Length"; _ ]) ->
      raise (UnsupportedLeanTheorem "Length op")
    | App (_, [ _; Op "At"; _; _ ]) -> raise (UnsupportedLeanTheorem "At op")
    | App (_, [ _; Op "List_cons"; _; _ ]) ->
      raise (UnsupportedLeanTheorem "List_cons op")
    | App (_, [ _; Op "List_append"; _; _ ]) ->
      raise (UnsupportedLeanTheorem "List_append op")
    | App (_, [ _; Op "Extract"; _; _; _ ]) ->
      raise (UnsupportedLeanTheorem "Extract op")
    | App (_, [ _; Op "Concat"; _ ]) ->
      raise (UnsupportedLeanTheorem "Concat op")
    | Concrete (ty, var) ->
      raise (UnsupportedLeanTheorem (Fmt.str "concrete pattern (%s %s)" ty var))
    | App (f, _) ->
      raise (UnsupportedLeanTheorem (Fmt.str "Unsupported App: %s" f))

  let generate_lean_theorem (r : rule_pat) (index : int) : string option =
    try
      let simpl_type =
        Option.value (extract_simpl_type r.lhs) ~default:"unknown"
      in
      let lemma_name = Fmt.str "simplification_%s_%06d" simpl_type index in

      let generics_tbl = Hashtbl.create 4 in
      find_generics_and_classes { generics = generics_tbl } r.lhs;
      find_generics_and_classes { generics = generics_tbl } r.rhs;
      Option.iter (find_generics_and_classes { generics = generics_tbl }) r.cond;

      let vars_tbl = Hashtbl.create 8 in
      find_var_types vars_tbl "ty" r.lhs;
      find_var_types vars_tbl "ty" r.rhs;
      Option.iter (find_var_types vars_tbl "ty") r.cond;

      let has_bitvec =
        Hashtbl.fold (fun _ ty acc -> acc || ty = "Bitvec") vars_tbl false
      in

      let generics_str =
        Hashtbl.to_seq generics_tbl
        |> List.of_seq
        |> List.map (fun (ty, classes) ->
             let class_str =
               List.map (fun c -> Fmt.str "[%s %s]" c ty) classes
               |> String.concat " "
             in
             Fmt.str "{%s : Type} %s" ty class_str )
        |> String.concat " "
      in
      let generics_str =
        if has_bitvec then generics_str ^ " {w : Nat}" else generics_str
      in

      let vars_tbl = Hashtbl.create 8 in
      find_var_types vars_tbl "ty" r.lhs;
      find_var_types vars_tbl "ty" r.rhs;
      Option.iter (find_var_types vars_tbl "ty") r.cond;

      let params_str =
        Hashtbl.to_seq vars_tbl
        (* Group by type *)
        |> List.of_seq
        |> List.sort (fun (_, t1) (_, t2) -> String.compare t1 t2)
        |> List.fold_left
             (fun acc (var, ty) ->
               match acc with
               | (current_ty, vars) :: rest when current_ty = ty ->
                 (current_ty, var :: vars) :: rest
               | _ -> (ty, [ var ]) :: acc )
             []
        |> List.rev_map (fun (ty, vars) ->
             let ty_str =
               if ty = "Bitvec" then "BitVec w"
               else if Hashtbl.mem generics_tbl ty then ty
               else lean_ty_constructor ty
             in
             Fmt.str "(%s : %s)" (String.concat " " (List.rev vars)) ty_str )
        |> String.concat " "
      in

      let decidable_constraints =
        Hashtbl.fold
          (fun var ty acc ->
            if ty = "Prop" then Fmt.str "[Decidable %s]" var :: acc else acc )
          vars_tbl []
        |> String.concat " "
      in

      let lhs_lean = expr_pat_to_lean_notation r.lhs in
      let rhs_lean = expr_pat_to_lean_notation r.rhs in

      let connector = if is_prop_pat r.lhs then "↔" else "=" in

      let statement =
        match r.cond with
        | None -> Fmt.str "%s %s %s" lhs_lean connector rhs_lean
        | Some c ->
          let cond_lean = expr_pat_to_lean_notation c in
          Fmt.str "%s →\n  %s %s %s" cond_lean lhs_lean connector rhs_lean
      in

      Some
        (Fmt.str "lemma %s %s %s %s :\n  %s := \nby sorry\n" lemma_name
           generics_str params_str decidable_constraints statement )
    with UnsupportedLeanTheorem _reason ->
      (* Fmt.epr "-- [Debug] Skipping rule %d: %s\n" index _reason; *)
      None

  let parse_and_generate_lean_theorems fname =
    try
      let rules = parse_rules_from_file fname in
      let rule_pats = List.map sexp_to_rule_pat rules in
      let all_results =
        List.mapi
          (fun i r -> (i + 1, generate_lean_theorem r (i + 1)))
          rule_pats
      in
      let generated_lemmas =
        List.filter_map (fun (_, opt) -> opt) all_results
      in
      generated_lemmas
    with e ->
      Fmt.failwith "Error parsing rules file '%s': %s\n" fname
        (Printexc.to_string e)

  let parse_and_generate fname =
    try
      let rules = parse_rules_from_file fname in
      let rule_pats = List.map sexp_to_rule_pat rules in
      List.map generate_match_arm rule_pats
    with e ->
      Fmt.failwith "Error parsing rules file '%s': %s\n" fname
        (Printexc.to_string e)
end

let generate_function op_type rules =
  let ty_name, raw_fallback, ending =
    match op_type with
    | "unop" -> ("let rec simplify_unop", "raw_unop ty op hte", "and")
    | "binop" -> ("simplify_binop", "raw_binop ty op hte1 hte2", "and")
    | "relop" -> ("simplify_relop", "raw_relop ty op hte1 hte2", "and")
    | "triop" -> ("simplify_triop", "raw_triop ty op hte1 hte2 hte3", "and")
    | "naryop" -> ("simplify_naryop", "raw_naryop ty op htes", "")
    | _ -> Fmt.failwith "Unsupported op_type: %s" op_type
  in
  let arg_pattern =
    match op_type with
    | "unop" -> "(ty : Ty.t) (op : Unop.t) (hte : Base_expr.t)"
    | "binop" ->
      "(ty : Ty.t) (op : Binop.t) (hte1 : Base_expr.t) (hte2 : Base_expr.t)"
    | "relop" ->
      "(ty : Ty.t) (op : Relop.t) (hte1 : Base_expr.t) (hte2 : Base_expr.t)"
    | "triop" ->
      "(ty : Ty.t) (op : Triop.t) (hte1 : Base_expr.t) (hte2 : Base_expr.t) \
       (hte3 : Base_expr.t)"
    | "naryop" -> "(ty : Ty.t) (op : Naryop.t) (htes : Base_expr.t list)"
    | _ -> Fmt.failwith "Invalid op_type"
  in
  let match_expr =
    match op_type with
    | "unop" -> "match op, view hte with"
    | "binop" -> "match op, view hte1, view hte2 with"
    | "relop" -> "match op, view hte1, view hte2 with"
    | "triop" -> "match op, view hte1, view hte2, view hte3 with"
    | "naryop" -> "match op, List.map view htes with"
    | _ -> Fmt.failwith "Invalid op_type"
  in
  Fmt.str "%s %s =\n  %s\n%s\n  | _ -> %s\n%s" ty_name arg_pattern match_expr
    (String.concat "\n" rules) raw_fallback ending

let () =
  let parsed_rules = RuleParser.parse_and_generate "rules.ssn" in
  let grouped =
    let types = [ "unop"; "binop"; "relop"; "triop"; "naryop" ] in
    List.map
      (fun ty ->
        let rules_for_ty =
          parsed_rules |> List.filter (fun (t, _) -> t = ty) |> List.map snd
        in
        (ty, rules_for_ty) )
      types
  in
  let generated =
    grouped
    |> List.map (fun (ty, rules) -> generate_function ty rules)
    |> String.concat "\n"
  in
  let content = "open Base_expr\nopen Ty\n\n" ^ generated ^ "\n" in
  let oc = open_out "simplifier.ml" in
  output_string oc content;
  close_out oc;

  let lean_theorems =
    RuleParser.parse_and_generate_lean_theorems "rules.ssn"
  in
  
  let lean_header =
    "import Mathlib\n"
    ^ (Fmt.str "-- Generated %d theorems from rules.ssn\n\n" (List.length lean_theorems))
  in
  let lean_body = String.concat "\n" lean_theorems in
  let lean_content = lean_header ^ lean_body in
  
  let oc_lean = open_out "simplifier_theorems.lean" in
  output_string oc_lean lean_content;
  close_out oc_lean