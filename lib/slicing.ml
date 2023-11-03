let find_dependents (expr : Expression.t) (exprs : Expression.t list) : Expression.t list * Expression.t list =
  let symbols = Expression.get_symbols [expr] in
  let rec aux remaining dependents independents known_symbols =
    match remaining with
    | [] -> (dependents, independents)
    | e :: rest ->
      let e_symbols = Expression.get_symbols [e] in
      if List.exists (fun s -> List.mem s e_symbols) known_symbols then
        aux (List.rev_append independents rest) (e :: dependents) [] (known_symbols @ e_symbols)
      else
        aux rest dependents (e :: independents) known_symbols
  in
  aux exprs [] [] symbols

let rec group_expressions (exprs : Expression.t list) : Expression.t list list =
  match exprs with
  | [] -> []
  | expr :: rest ->
    let dependents, independents = find_dependents expr rest in
    (expr :: dependents) :: group_expressions independents

let slice (exprs : Expression.t list) : Expression.t list list =
  group_expressions exprs
