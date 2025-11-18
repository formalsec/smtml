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

let () =
  let in_file = "rules.dsl" in
  let out_ml = "simplifier.ml" in

  Fmt.epr "Parsing rules from %s...\n" in_file;
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
  Fmt.epr "Found %d DSL rules.\n" (List.length dsl_rules);

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

  Fmt.epr "Successfully generated %s\n" out_ml
