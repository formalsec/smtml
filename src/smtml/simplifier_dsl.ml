open Simplifier_ast

module CodeGen = struct
  let get_lhs_var_level (p : Dsl_ast.lhs_pattern) : (string * int) list =
    let rec aux (p : Dsl_ast.lhs_pattern) (lvl : int) acc =
      match p with
      | LVar v -> (v, lvl) :: acc
      | LConstructor (_name, args) ->
        List.fold_left (fun a arg -> aux arg (lvl + 1) a) acc args
      | LList items -> List.fold_left (fun a item -> aux item lvl a) acc items
      | LInt _ -> acc
    in
    aux p 0 []

  let rec ast_lhs_to_ocaml_pattern (p : Dsl_ast.lhs_pattern) (lvl : int) :
    string =
    match p with
    | LVar v -> v
    | LList [] -> "[]"
    | LList items ->
      let vars_str =
        List.map (fun item -> ast_lhs_to_ocaml_pattern item (lvl + 1)) items
        |> String.concat "; "
      in
      Fmt.str "[ %s ]" vars_str
    | LInt i -> string_of_int i
    (* Value Patterns *)
    | LConstructor ("True", []) -> "Val (True)"
    | LConstructor ("False", []) -> "Val (False)"
    | LConstructor ("Int", [ arg ]) ->
      if lvl = 1 then
        Fmt.str "Val (Int (%s))" (ast_lhs_to_ocaml_pattern arg (lvl + 1))
      else
        Fmt.str "{node = Val (Int (%s)); _}"
          (ast_lhs_to_ocaml_pattern arg (lvl + 1))
    | LConstructor ("Bitv", [ arg ]) ->
      if lvl = 1 then
        Fmt.str "Val (Bitv %s)" (ast_lhs_to_ocaml_pattern arg (lvl + 1))
      else
        Fmt.str "{node = Val (Bitv %s); _}"
          (ast_lhs_to_ocaml_pattern arg (lvl + 1))
    | LConstructor ("Val", [ arg ]) ->
      if lvl = 1 then
        Fmt.str "Val (%s)" (ast_lhs_to_ocaml_pattern arg (lvl + 1))
      else
        Fmt.str "{node = Val (%s); _}" (ast_lhs_to_ocaml_pattern arg (lvl + 1))
    (* Operators Patterns *)
    | LConstructor ("List", [ LList [] ]) -> "List []"
    | LConstructor ("List", [ LList items ]) ->
      let vars_str =
        List.map (fun item -> ast_lhs_to_ocaml_pattern item (lvl + 1)) items
        |> String.concat "; "
      in
      Fmt.str "List [ %s ]" vars_str
    | LConstructor ("List", [ LVar v ]) -> Fmt.str "List %s" v
    | LConstructor ("Not", [ arg ]) ->
      Fmt.str "Unop (_, Not, %s)" (ast_lhs_to_ocaml_pattern arg (lvl + 1))
    | LConstructor ("Neg", [ arg ]) ->
      Fmt.str "Unop (_, Neg, %s)" (ast_lhs_to_ocaml_pattern arg (lvl + 1))
    | LConstructor ("Reverse", [ arg ]) ->
      Fmt.str "Unop (_, Reverse, %s)" (ast_lhs_to_ocaml_pattern arg (lvl + 1))
    | LConstructor ("Add", [ a1; a2 ]) ->
      Fmt.str "Binop (_, Add, %s, %s)"
        (ast_lhs_to_ocaml_pattern a1 (lvl + 1))
        (ast_lhs_to_ocaml_pattern a2 (lvl + 1))
    | LConstructor ("Sub", [ a1; a2 ]) ->
      Fmt.str "Binop (_, Sub, %s, %s)"
        (ast_lhs_to_ocaml_pattern a1 (lvl + 1))
        (ast_lhs_to_ocaml_pattern a2 (lvl + 1))
    | LConstructor ("Mul", [ a1; a2 ]) ->
      Fmt.str "Binop (_, Mul, %s, %s)"
        (ast_lhs_to_ocaml_pattern a1 (lvl + 1))
        (ast_lhs_to_ocaml_pattern a2 (lvl + 1))
    | LConstructor ("Ne", [ a1; a2 ]) ->
      Fmt.str "Relop (_, Ne, %s, %s)"
        (ast_lhs_to_ocaml_pattern a1 (lvl + 1))
        (ast_lhs_to_ocaml_pattern a2 (lvl + 1))
    | LConstructor ("Eq", [ a1; a2 ]) ->
      Fmt.str "Relop (_, Eq, %s, %s)"
        (ast_lhs_to_ocaml_pattern a1 (lvl + 1))
        (ast_lhs_to_ocaml_pattern a2 (lvl + 1))
    | LConstructor ("Lt", [ a1; a2 ]) ->
      Fmt.str "Relop (_, Lt, %s, %s)"
        (ast_lhs_to_ocaml_pattern a1 (lvl + 1))
        (ast_lhs_to_ocaml_pattern a2 (lvl + 1))
    | LConstructor ("Le", [ a1; a2 ]) ->
      Fmt.str "Relop (_, Le, %s, %s)"
        (ast_lhs_to_ocaml_pattern a1 (lvl + 1))
        (ast_lhs_to_ocaml_pattern a2 (lvl + 1))
    | LConstructor ("Gt", [ a1; a2 ]) ->
      Fmt.str "Relop (_, Gt, %s, %s)"
        (ast_lhs_to_ocaml_pattern a1 (lvl + 1))
        (ast_lhs_to_ocaml_pattern a2 (lvl + 1))
    | LConstructor ("Ge", [ a1; a2 ]) ->
      Fmt.str "Relop (_, Ge, %s, %s)"
        (ast_lhs_to_ocaml_pattern a1 (lvl + 1))
        (ast_lhs_to_ocaml_pattern a2 (lvl + 1))
    | LConstructor ("Ite", [ a1; a2; a3 ]) ->
      Fmt.str "Triop (_, Ite, %s, %s, %s)"
        (ast_lhs_to_ocaml_pattern a1 (lvl + 1))
        (ast_lhs_to_ocaml_pattern a2 (lvl + 1))
        (ast_lhs_to_ocaml_pattern a3 (lvl + 1))
    | LConstructor ("Concat", [ a1 ]) ->
      Fmt.str "Naryop (_, Concat, %s)" (ast_lhs_to_ocaml_pattern a1 (lvl + 1))
    | LConstructor (name, []) -> name
    | LConstructor (name, _args) ->
      Fmt.failwith "Unsupported nested LHS constructor: %s" name

  let ast_lhs_pattern_to_ocaml_expr (p : Dsl_ast.lhs_pattern) : string =
    match p with
    | LVar v -> v
    | LConstructor ("List", [ LVar v ]) -> Fmt.str "(make (List %s))" v
    | LConstructor (name, _args) ->
      Fmt.failwith
        "ast_lhs_pattern_to_ocaml_expr: Unsupported constructor '%s' in 'when' \
         clause"
        name
    | LList _ | LInt _ ->
      Fmt.failwith
        "ast_lhs_pattern_to_ocaml_expr: Unsupported pattern in 'when' clause"

  let op_type op =
    match op with
    | "Add" | "Sub" | "Mul" | "And" | "Or" | "At" | "List_cons" | "List_append"
      ->
      "binop"
    | "Eq" | "Ne" | "Lt" | "Le" | "Gt" | "Ge" -> "relop"
    | "Ite" -> "triop"
    | "Concat" -> "naryop"
    | "Extract" -> "extract"
    | "Not" | "Neg" | "Reverse" | "Head" | "Tail" | "Length" -> "unop"
    | _ -> Fmt.failwith "Unsupported evaluation of operator: %s\n" op

  let rec ast_rhs_to_ocaml_expr (e : Dsl_ast.rhs_expr)
    (vars : (string * int) list) : string =
    match e with
    | RVar v ->
      let lvl =
        try List.assoc v vars
        with Not_found -> Fmt.failwith "Unknown variable: %s" v
      in
      if lvl = 1 then Fmt.str "(make %s)" v else v
    | RInt i -> string_of_int i
    | RList l ->
      let items_str =
        List.map (fun item -> ast_rhs_to_ocaml_expr item vars) l
        |> String.concat "; "
      in
      Fmt.str "[ %s ]" items_str
    | RConstructor ("Eq", [ a1; a2 ]) ->
      Fmt.str "simplify_relop ty Eq (%s) (%s)"
        (ast_rhs_to_ocaml_expr a1 vars)
        (ast_rhs_to_ocaml_expr a2 vars)
    | RConstructor ("Ne", [ a1; a2 ]) ->
      Fmt.str "simplify_relop ty Ne (%s) (%s)"
        (ast_rhs_to_ocaml_expr a1 vars)
        (ast_rhs_to_ocaml_expr a2 vars)
    | RConstructor ("Lt", [ a1; a2 ]) ->
      Fmt.str "simplify_relop ty Lt (%s) (%s)"
        (ast_rhs_to_ocaml_expr a1 vars)
        (ast_rhs_to_ocaml_expr a2 vars)
    | RConstructor ("Le", [ a1; a2 ]) ->
      Fmt.str "simplify_relop ty Le (%s) (%s)"
        (ast_rhs_to_ocaml_expr a1 vars)
        (ast_rhs_to_ocaml_expr a2 vars)
    | RConstructor ("Add", [ a1; a2 ]) ->
      Fmt.str "simplify_binop ty Add (%s) (%s)"
        (ast_rhs_to_ocaml_expr a1 vars)
        (ast_rhs_to_ocaml_expr a2 vars)
    | RConstructor ("Sub", [ a1; a2 ]) ->
      Fmt.str "simplify_binop ty Sub (%s) (%s)"
        (ast_rhs_to_ocaml_expr a1 vars)
        (ast_rhs_to_ocaml_expr a2 vars)
    | RConstructor ("Mul", [ a1; a2 ]) ->
      Fmt.str "simplify_binop ty Mul (%s) (%s)"
        (ast_rhs_to_ocaml_expr a1 vars)
        (ast_rhs_to_ocaml_expr a2 vars)
    | RConstructor ("And", [ a1; a2 ]) ->
      Fmt.str "simplify_binop Ty_bool And (%s) (%s)"
        (ast_rhs_to_ocaml_expr a1 vars)
        (ast_rhs_to_ocaml_expr a2 vars)
    | RConstructor ("Ite", [ a1; a2; a3 ]) ->
      Fmt.str "simplify_triop ty Ite (%s) (%s) (%s)"
        (ast_rhs_to_ocaml_expr a1 vars)
        (ast_rhs_to_ocaml_expr a2 vars)
        (ast_rhs_to_ocaml_expr a3 vars)
    | RConstructor ("Concat", [ a1 ]) ->
      Fmt.str "simplify_naryop ty Concat (%s)" (ast_rhs_to_ocaml_expr a1 vars)
    | RConstructor ("List", [ arg ]) ->
      Fmt.str "list (%s)" (ast_rhs_to_ocaml_expr arg vars)
    | RConstructor ("Int", [ arg ]) ->
      Fmt.str "value (Int (%s))" (ast_rhs_to_ocaml_expr arg vars)
    | RConstructor ("Bitv", [ arg ]) ->
      Fmt.str "value (Bitv (%s))" (ast_rhs_to_ocaml_expr arg vars)
    | RFuncall ("eval", [ RConstructor (op_name, []); v1; v2 ]) ->
      let op_s = String.capitalize_ascii op_name in
      let op_t = op_type op_name in
      Fmt.str "value ((Eval.%s ty %s %s %s))" op_t op_s
        (ast_rhs_to_ocaml_expr v1 vars)
        (ast_rhs_to_ocaml_expr v2 vars)
    | RNamespacedFuncall ("List", "rev", [ arg ]) ->
      Fmt.str "List.rev %s" (ast_rhs_to_ocaml_expr arg vars)
    | RNamespacedFuncall ("List", "length", [ arg ]) ->
      Fmt.str "List.length %s" (ast_rhs_to_ocaml_expr arg vars)
    | RNamespacedFuncall ("List", "append", [ a1; a2 ]) ->
      Fmt.str "List.append %s %s"
        (ast_rhs_to_ocaml_expr a1 vars)
        (ast_rhs_to_ocaml_expr a2 vars)
    | RNamespacedFuncall ("List", "cons", [ a1; a2 ]) ->
      Fmt.str "List.cons %s %s"
        (ast_rhs_to_ocaml_expr a1 vars)
        (ast_rhs_to_ocaml_expr a2 vars)
    | RNamespacedFuncall ("List", "nth", [ lst; idx ]) ->
      Fmt.str "make (List.nth %s %s)"
        (ast_rhs_to_ocaml_expr lst vars)
        (ast_rhs_to_ocaml_expr idx vars)
    | RNamespacedFuncall (b, f, args) ->
      let args_str =
        List.map (fun a -> ast_rhs_to_ocaml_expr a vars) args
        |> String.concat " "
      in
      Fmt.str "%s.%s %s " b f args_str
    | _ -> Fmt.failwith "Unsupported RHS expression"

  let rec ast_cond_to_ocaml_expr (e : Dsl_ast.cond_expr)
    (vars : (string * int) list) : string =
    match e with
    | CVar s -> s
    | CPat (LVar v) -> v
    | CInt i -> string_of_int i
    | CApp ("concrete", [ CPat pat ]) ->
      Fmt.str "(not (is_symbolic %s))" (ast_lhs_pattern_to_ocaml_expr pat)
    | CApp ("size", [ arg ]) ->
      let lvl =
        match arg with
        | CPat (LVar v) -> (
          try List.assoc v vars
          with Not_found -> Fmt.failwith "Unknown variable in size(): %s" v )
        | _ -> Fmt.failwith "Unsupported argument to size(): must be a variable"
      in
      if lvl = 1 then
        Fmt.str "Ty.size (ty (make %s))" (ast_cond_to_ocaml_expr arg vars)
      else Fmt.str "Ty.size (ty %s)" (ast_cond_to_ocaml_expr arg vars)
    | CNamespacedApp ("Bitvector", "eqz", [ arg ]) ->
      Fmt.str "(Bitvector.eqz %s)" (ast_cond_to_ocaml_expr arg vars)
    | CNamespacedApp ("Bitvector", "eq_one", [ arg ]) ->
      Fmt.str "(Bitvector.eq_one %s)" (ast_cond_to_ocaml_expr arg vars)
    | CNamespacedApp (b, f, args) ->
      let args_str =
        List.map (fun a -> ast_cond_to_ocaml_expr a vars) args
        |> String.concat " "
      in
      Fmt.str "%s.%s %s " b f args_str
    | CInfix (a1, op, a2) ->
      Fmt.str "(%s %s %s)"
        (ast_cond_to_ocaml_expr a1 vars)
        op
        (ast_cond_to_ocaml_expr a2 vars)
    | CAnd (a1, a2) ->
      Fmt.str "(%s && %s)"
        (ast_cond_to_ocaml_expr a1 vars)
        (ast_cond_to_ocaml_expr a2 vars)
    | COr (a1, a2) ->
      Fmt.str "(%s || %s)"
        (ast_cond_to_ocaml_expr a1 vars)
        (ast_cond_to_ocaml_expr a2 vars)
    | CNot a1 -> Fmt.str "(not %s)" (ast_cond_to_ocaml_expr a1 vars)
    | _ -> Fmt.failwith "Unsupported condition expression"

  let generate_match_arm (r : Dsl_ast.rule) : string * string =
    let var_lvls = get_lhs_var_level r.lhs in

    let op_type, lhs_pattern_str =
      match r.lhs with
      (* Unops *)
      | LConstructor
          ( (("Not" | "Neg" | "Reverse" | "Head" | "Tail" | "Length") as op)
          , [ arg ] ) ->
        let op_constr = String.capitalize_ascii op in
        ("unop", Fmt.str "%s, %s" op_constr (ast_lhs_to_ocaml_pattern arg 1))
      (* Binops *)
      | LConstructor
          ( ( ( "Add" | "Sub" | "Mul" | "And" | "Or" | "At" | "List_cons"
              | "List_append" ) as op )
          , [ a1; a2 ] ) ->
        let op_constr = String.capitalize_ascii op in
        ( "binop"
        , Fmt.str "%s, %s, %s" op_constr
            (ast_lhs_to_ocaml_pattern a1 1)
            (ast_lhs_to_ocaml_pattern a2 1) )
      (* Relops *)
      | LConstructor
          ((("Eq" | "Ne" | "Lt" | "Le" | "Gt" | "Ge") as op), [ a1; a2 ]) ->
        let op_constr = String.capitalize_ascii op in
        ( "relop"
        , Fmt.str "Relop.%s, %s, %s" op_constr
            (ast_lhs_to_ocaml_pattern a1 1)
            (ast_lhs_to_ocaml_pattern a2 1) )
      (* Triops *)
      | LConstructor ("Ite", [ a1; a2; a3 ]) ->
        ( "triop"
        , Fmt.str "Ite, %s, %s, %s"
            (ast_lhs_to_ocaml_pattern a1 1)
            (ast_lhs_to_ocaml_pattern a2 1)
            (ast_lhs_to_ocaml_pattern a3 1) )
      | LConstructor ("Extract", [ a1; a2; a3 ]) ->
        ( "extract"
        , Fmt.str "%s, %s, %s"
            (ast_lhs_to_ocaml_pattern a1 1)
            (ast_lhs_to_ocaml_pattern a2 1)
            (ast_lhs_to_ocaml_pattern a3 1) )
      (* Naryops *)
      | LConstructor ("Concat", [ arg ]) ->
        ("naryop", Fmt.str "Concat, %s" (ast_lhs_to_ocaml_pattern arg 1))
      | _ -> Fmt.failwith "Unsupported LHS rule structure"
    in

    let when_clause =
      match r.cond with
      | None -> ""
      | Some c -> Fmt.str " when %s" (ast_cond_to_ocaml_expr c var_lvls)
    in

    let rhs_str = ast_rhs_to_ocaml_expr r.rhs var_lvls in

    (op_type, Fmt.str "  | %s%s -> %s" lhs_pattern_str when_clause rhs_str)

  let generate_function op_type rules =
    (* only add rec flag if rules list is non-empty *)
    let rec_flag = if List.length rules > 0 then "rec " else "" in
    let ty_name, raw_fallback, ending =
      match op_type with
      | "unop" ->
        (Fmt.str "let %ssimplify_unop" rec_flag, "raw_unop ty op hte", "and")
      | "binop" -> ("simplify_binop", "raw_binop ty op hte1 hte2", "and")
      | "relop" -> ("simplify_relop", "raw_relop ty op hte1 hte2", "and")
      | "triop" -> ("simplify_triop", "raw_triop ty op hte1 hte2 hte3", "and")
      | "extract" -> ("simplify_extract", "raw_extract hte ~high ~low", "and")
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
      | "extract" -> "(hte : Base_expr.t) (high : int) (low : int)"
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
      | "extract" -> "match view hte, high, low with"
      | _ -> Fmt.failwith "Invalid op_type"
    in
    Fmt.str "%s %s =\n  %s\n%s\n  | _ -> %s\n%s" ty_name arg_pattern match_expr
      (String.concat "\n" rules) raw_fallback ending
end

module LeanGen = struct
  open Dsl_ast

  exception UnsupportedLean of string

  let _lean_ty_constructor (ty_str : string) : string =
    match ty_str with
    | "Ty_int" -> "Int"
    | "Ty_str" -> "String"
    | "Ty_bool" -> "Bool"
    | "Ty_bitv" -> "Bitvec"
    | "Int" -> "Int"
    | "String" -> "String"
    | "Bool" -> "Bool"
    | "Prop" -> "Prop"
    | _ -> ty_str

  let is_variable (s : string) : bool =
    if String.length s = 0 then false
    else
      let c = String.get s 0 in
      (c >= 'a' && c <= 'z') || c = '_'

  let rec has_bitv (p : lhs_pattern) : bool =
    match p with
    | LVar _ -> false
    | LConstructor ("Bitv", _) -> true
    | LConstructor (_, args) -> List.exists has_bitv args
    | LList items -> List.exists has_bitv items
    | _ -> false

  let rec infer_var_types (vars : (string, string) Hashtbl.t)
    (current_ty : string) (p : lhs_pattern) : unit =
    let get_elem_ty ty =
      if String.starts_with ~prefix:"List " ty then
        String.sub ty 5 (String.length ty - 5)
      else "ty"
    in
    match p with
    | LVar "_" ->
      if not (Hashtbl.mem vars "_a") then Hashtbl.add vars "_a" current_ty
    | LVar v when is_variable v ->
      if not (Hashtbl.mem vars v) then Hashtbl.add vars v current_ty
    | LList items ->
      List.iter (infer_var_types vars (get_elem_ty current_ty)) items
    | LConstructor ("Bitv", [ arg ]) -> infer_var_types vars "BitVec" arg
    | LConstructor ("Int", [ arg ]) -> infer_var_types vars "Int" arg
    | LConstructor ("True", []) -> ()
    | LConstructor ("False", []) -> ()
    | LConstructor ("List", [ arg ]) ->
      infer_var_types vars (Fmt.str "List %s" current_ty) arg
    | LConstructor ("Ite", [ c; t; e ]) ->
      infer_var_types vars "Prop" c;
      infer_var_types vars current_ty t;
      infer_var_types vars current_ty e
    | LConstructor ("Extract", [ a1; a2; a3 ]) ->
      infer_var_types vars "BitVec" a1;
      infer_var_types vars "Int" a2;
      infer_var_types vars "Int" a3
    | LConstructor ("Not", [ arg ]) -> infer_var_types vars "Prop" arg
    | LConstructor (("Eq" | "Ne" | "Lt" | "Le" | "Gt" | "Ge"), [ a1; a2 ]) ->
      let arg_ty = if current_ty = "Prop" then "ty" else current_ty in
      let final_ty = if has_bitv a1 || has_bitv a2 then "BitVec" else arg_ty in
      infer_var_types vars final_ty a1;
      infer_var_types vars final_ty a2
    | LConstructor (("Add" | "Sub" | "Mul" | "Or" | "And"), [ a1; a2 ]) ->
      let final_ty =
        if has_bitv a1 || has_bitv a2 then "BitVec" else current_ty
      in
      infer_var_types vars final_ty a1;
      infer_var_types vars final_ty a2
    | LConstructor ("Reverse", [ arg ]) ->
      (* if current_ty already has List, then don't add it again *)
      if String.contains current_ty ' ' then infer_var_types vars current_ty arg
      else infer_var_types vars (Fmt.str "List %s" current_ty) arg
    | LConstructor ("Concat", [ arg1; arg2 ]) ->
      infer_var_types vars (Fmt.str "List %s" current_ty) arg1;
      infer_var_types vars (Fmt.str "List %s" current_ty) arg2
    | LConstructor ("Concat", [ arg ]) -> infer_var_types vars "List String" arg
    | LConstructor (_, args) -> List.iter (infer_var_types vars current_ty) args
    | LInt _ -> ()
    | _ -> ()

  let rec cond_to_lean (e : cond_expr) (vars_tbl : (string, string) Hashtbl.t) :
    string =
    match e with
    | CVar s -> s
    | CInt i -> string_of_int i
    | CPat (LVar v) -> v
    | CApp ("size", _) -> "w" (* w represents the bitvector width *)
    | CNamespacedApp ("Bitvector", "eqz", [ arg ]) ->
      Fmt.str "(%s = 0)" (cond_to_lean arg vars_tbl)
    | CNamespacedApp ("Bitvector", "eq_one", [ arg ]) ->
      Fmt.str "(%s = 1)" (cond_to_lean arg vars_tbl)
    | CInfix (a1, op, a2) ->
      let op_str = match op with "==" -> "=" | "!=" -> "≠" | _ -> op in
      Fmt.str "(%s %s %s)" (cond_to_lean a1 vars_tbl) op_str
        (cond_to_lean a2 vars_tbl)
    | CAnd (a1, a2) ->
      Fmt.str "(%s ∧ %s)" (cond_to_lean a1 vars_tbl) (cond_to_lean a2 vars_tbl)
    | COr (a1, a2) ->
      Fmt.str "(%s ∨ %s)" (cond_to_lean a1 vars_tbl) (cond_to_lean a2 vars_tbl)
    | CNot a -> Fmt.str "(¬%s)" (cond_to_lean a vars_tbl)
    | _ -> raise (UnsupportedLean "Unsupported condition expr")

  let rec to_lean (p : lhs_pattern) (vars_tbl : (string, string) Hashtbl.t) :
    string =
    let get_ty_of_arg p =
      (* Hashtbl.iter (fun k v -> Fmt.epr "%s -> %s\n" k v) vars_tbl; *)
      match p with
      | LVar v ->
        let v = if v = "_" then "_a" else v in
        Hashtbl.find_opt vars_tbl v |> Option.value ~default:"ty"
      | _ -> "ty"
    in
    match p with
    | LVar v -> if v = "_" then "_a" else v
    | LInt i -> string_of_int i
    | LConstructor ("True", []) -> "True"
    | LConstructor ("False", []) -> "False"
    | LConstructor ("Int", [ arg ]) -> to_lean arg vars_tbl
    | LConstructor ("Bitv", [ arg ]) -> to_lean arg vars_tbl
    | LConstructor ("Val", [ arg ]) -> to_lean arg vars_tbl
    | LConstructor ("List", [ arg ]) -> to_lean arg vars_tbl
    | LList items ->
      Fmt.str "[%s]"
        (List.map (fun i -> to_lean i vars_tbl) items |> String.concat ", ")
    | LConstructor ("Not", [ arg ]) -> Fmt.str "¬%s" (to_lean arg vars_tbl)
    | LConstructor ("Neg", [ arg ]) -> Fmt.str "¬%s" (to_lean arg vars_tbl)
    | LConstructor ("Add", [ a1; a2 ]) ->
      Fmt.str "(%s) + (%s)" (to_lean a1 vars_tbl) (to_lean a2 vars_tbl)
    | LConstructor ("Sub", [ a1; a2 ]) ->
      Fmt.str "(%s) - (%s)" (to_lean a1 vars_tbl) (to_lean a2 vars_tbl)
    | LConstructor ("Mul", [ a1; a2 ]) ->
      Fmt.str "(%s) * (%s)" (to_lean a1 vars_tbl) (to_lean a2 vars_tbl)
    | LConstructor ("And", [ a1; a2 ]) ->
      let ty1 = get_ty_of_arg a1 in
      let ty2 = get_ty_of_arg a2 in
      if ty1 = "BitVec" || ty2 = "BitVec" then
        Fmt.str "(%s) &&& (%s)" (to_lean a1 vars_tbl) (to_lean a2 vars_tbl)
      else Fmt.str "(%s) ∧ (%s)" (to_lean a1 vars_tbl) (to_lean a2 vars_tbl)
    | LConstructor ("Or", [ a1; a2 ]) ->
      let ty1 = get_ty_of_arg a1 in
      let ty2 = get_ty_of_arg a2 in
      if ty1 = "BitVec" || ty2 = "BitVec" then
        Fmt.str "(%s) ||| (%s)" (to_lean a1 vars_tbl) (to_lean a2 vars_tbl)
      else Fmt.str "(%s) ∨ (%s)" (to_lean a1 vars_tbl) (to_lean a2 vars_tbl)
    | LConstructor ("Eq", [ a1; a2 ]) ->
      Fmt.str "((%s) = (%s))" (to_lean a1 vars_tbl) (to_lean a2 vars_tbl)
    | LConstructor ("Ne", [ a1; a2 ]) ->
      Fmt.str "((%s) ≠ (%s))" (to_lean a1 vars_tbl) (to_lean a2 vars_tbl)
    | LConstructor ("Lt", [ a1; a2 ]) ->
      Fmt.str "((%s) < (%s))" (to_lean a1 vars_tbl) (to_lean a2 vars_tbl)
    | LConstructor ("Le", [ a1; a2 ]) ->
      Fmt.str "((%s) ≤ (%s))" (to_lean a1 vars_tbl) (to_lean a2 vars_tbl)
    | LConstructor ("Gt", [ a1; a2 ]) ->
      Fmt.str "((%s) > (%s))" (to_lean a1 vars_tbl) (to_lean a2 vars_tbl)
    | LConstructor ("Ge", [ a1; a2 ]) ->
      Fmt.str "((%s) ≥ (%s))" (to_lean a1 vars_tbl) (to_lean a2 vars_tbl)
    | LConstructor ("Ite", [ c; t; e ]) ->
      Fmt.str "(if %s then %s else %s)" (to_lean c vars_tbl)
        (to_lean t vars_tbl) (to_lean e vars_tbl)
    | LConstructor ("Reverse", [ arg ]) ->
      Fmt.str "List.reverse (%s)" (to_lean arg vars_tbl)
    | LConstructor ("Concat", [ arg ]) ->
      Fmt.str "(String.join %s)" (to_lean arg vars_tbl)
    | LConstructor (name, _) ->
      raise
        (UnsupportedLean (Fmt.str "to_lean: unsupported constructor %s" name))

  let rec rhs_to_lean (e : rhs_expr) (vars_tbl : (string, string) Hashtbl.t) :
    string =
    match e with
    | RVar v -> v
    | RInt i -> string_of_int i
    | RList items ->
      Fmt.str "[%s]"
        (List.map (fun i -> rhs_to_lean i vars_tbl) items |> String.concat ", ")
    | RConstructor ("List", [ arg ]) -> rhs_to_lean arg vars_tbl
    | RConstructor (name, args) ->
      let fake_lhs_args =
        List.map (fun a -> LVar (rhs_to_lean a vars_tbl)) args
      in
      to_lean (LConstructor (name, fake_lhs_args)) vars_tbl
    | RNamespacedFuncall ("List", "append", [ a1; a2 ]) ->
      Fmt.str "(%s ++ %s)" (rhs_to_lean a1 vars_tbl) (rhs_to_lean a2 vars_tbl)
    | RNamespacedFuncall ("List", "cons", [ a1; a2 ]) ->
      Fmt.str "(%s :: %s)" (rhs_to_lean a1 vars_tbl) (rhs_to_lean a2 vars_tbl)
    | RFuncall ("eval", [ RConstructor (op, []); v1; v2 ]) ->
      let op_map =
        match op with
        | "Add" -> "+"
        | "Sub" -> "-"
        | "Mul" -> "*"
        | _ -> raise (UnsupportedLean ("eval op " ^ op))
      in
      Fmt.str "(%s) %s (%s)" (rhs_to_lean v1 vars_tbl) op_map
        (rhs_to_lean v2 vars_tbl)
    | _ -> raise (UnsupportedLean "Unsupported RHS expression")

  let generate_theorem (r : rule) (idx : int) : string option =
    try
      let vars_tbl = Hashtbl.create 8 in
      infer_var_types vars_tbl "ty" r.lhs;
      let classes = ref [] in
      let rec detect_classes p =
        match p with
        | LConstructor (("Eq" | "Ne"), _) ->
          classes := "DecidableEq ty" :: !classes
        | LConstructor (("Add" | "Sub" | "Mul"), _) ->
          classes := "CommRing ty" :: !classes
        | LConstructor (("Lt" | "Le" | "Gt" | "Ge"), _) ->
          classes := "LinearOrder ty" :: !classes
        | LConstructor (_, args) -> List.iter detect_classes args
        | LList items -> List.iter detect_classes items
        | _ -> ()
      in
      detect_classes r.lhs;
      let has_bitvec =
        Hashtbl.fold (fun _ t acc -> acc || t = "BitVec") vars_tbl false
      in
      let generics =
        if has_bitvec then "{w : Nat} "
        else
          "{ty : Type} "
          ^ String.concat " "
              (List.map
                 (fun c -> "[" ^ c ^ "]")
                 (List.sort_uniq String.compare !classes) )
      in
      let params =
        Hashtbl.fold
          (fun v t acc ->
            let t_str = if t = "BitVec" then "BitVec w" else t in
            acc ^ Fmt.str "(%s : %s) " v t_str )
          vars_tbl ""
      in

      let decidable_constraints =
        Hashtbl.fold
          (fun var ty acc ->
            if ty = "Prop" then Fmt.str "[Decidable %s]" var :: acc else acc )
          vars_tbl []
        |> String.concat " "
      in

      let lhs_str = to_lean r.lhs vars_tbl in
      let rhs_str = rhs_to_lean r.rhs vars_tbl in
      let relation =
        match r.lhs with
        | LConstructor (("Eq" | "Ne" | "Lt" | "Le" | "Gt" | "Ge" | "Not"), _) ->
          "↔"
        | _ -> "="
      in
      let stmt =
        match r.cond with
        | None -> Fmt.str "%s %s %s" lhs_str relation rhs_str
        | Some cond ->
          Fmt.str "%s →\n %s %s %s"
            (cond_to_lean cond vars_tbl)
            lhs_str relation rhs_str
      in
      Some
        (Fmt.str "lemma simplification_%06d %s %s %s :\n  %s := \nby sorry\n"
           idx generics params decidable_constraints stmt )
    with UnsupportedLean _ | Failure _ -> None
end

let () =
  let in_file = "rules.dsl" in
  let out_ml = "simplifier.ml" in
  let out_lean = "simplifier_theorems.lean" in

  (* Fmt.epr "Parsing rules from %s...\n" in_file; *)
  let raw_content =
    match Bos.OS.File.read (Fpath.v in_file) with
    | Ok c -> c
    | Error (`Msg e) -> Fmt.failwith "Error reading rules file: %s" e
  in

  let lexbuf = Lexing.from_string raw_content in
  let dsl_rules : Simplifier_ast.Dsl_ast.rule list =
    try Simplifier_parser.rules Simplifier_lexer.token lexbuf
    with e ->
      let pos = lexbuf.Lexing.lex_curr_p in
      Fmt.failwith "Parse error at line %d, col %d: %s\nLexed token: '%s'"
        pos.Lexing.pos_lnum
        (pos.Lexing.pos_cnum - pos.Lexing.pos_bol)
        (Printexc.to_string e) (Lexing.lexeme lexbuf)
  in
  (* Fmt.epr "Found %d DSL rules.\n" (List.length dsl_rules); *)

  let parsed_rules = List.map CodeGen.generate_match_arm dsl_rules in

  (* Group the rules by op_type *)
  let grouped =
    let types = [ "unop"; "binop"; "relop"; "triop"; "extract"; "naryop" ] in
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
    |> List.map (fun (ty, rules) -> CodeGen.generate_function ty rules)
    |> String.concat "\n\n"
  in

  let content =
    "(* THIS FILE IS AUTOGENERATED - DO NOT EDIT *)\n\n" ^ "open Base_expr\n"
    ^ "open Ty\n" ^ "(* --- Generated Simplification Functions --- *)\n"
    ^ generated ^ "\n"
  in

  let oc = open_out out_ml in
  output_string oc content;
  close_out oc;

  (* Fmt.epr "Successfully generated %s\n" out_ml; *)

  (* Fmt.epr "Generating Lean theorems at %s...\n" out_lean; *)
  let lean_thms =
    List.mapi (fun i r -> LeanGen.generate_theorem r (i + 1)) dsl_rules
    |> List.filter_map (fun x -> x)
  in
  let lean_content =
    "-- THIS FILE IS AUTOGENERATED --\nimport Mathlib\n\n"
    ^ String.concat "\n" lean_thms
  in
  let oc_lean = open_out out_lean in
  output_string oc_lean lean_content;
  close_out oc_lean
(* Fmt.epr "Successfully generated %s\n" out_lean *)
