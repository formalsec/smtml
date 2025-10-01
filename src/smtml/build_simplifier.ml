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
        | ';' -> Buffer.add_char buf ' '
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

  let parse_rules_from_file (filename : string) : rule list =
    let raw = Bos.OS.File.read (Fpath.v filename) in
    let content =
      match raw with
      | Ok c -> c
      | Error (`Msg e) -> Fmt.failwith "Error reading rules file: %s" e
    in
    let preprocessed = preprocess content in
    let lexbuf = Lexing.from_string preprocessed in
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
    (* Fmt.epr "expr : %s\n" (_pp_expr_pat p); *)
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
        let constr = String.concat "" ["simplify_"; String.uncapitalize_ascii f] in
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
    | "binop" -> "(ty : Ty.t) (op : Binop.t) (hte1 : Base_expr.t) (hte2 : Base_expr.t)"
    | "relop" -> "(ty : Ty.t) (op : Relop.t) (hte1 : Base_expr.t) (hte2 : Base_expr.t)"
    | "triop" ->
      "(ty : Ty.t) (op : Triop.t) (hte1 : Base_expr.t) (hte2 : Base_expr.t) (hte3 : Base_expr.t)"
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
  let parsed_rules = RuleParser.parse_and_generate "rules.ssn"  in
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
  close_out oc
