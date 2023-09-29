
module SExpr = struct 
  type t = (String.t * Types.expr_type)

  let compare (s1, _) (s2, _) = String.compare s1 s2
end 

module SS = Set.Make(SExpr) 

let symbol_to_tuple (s : Symbol.t) : string * Types.expr_type = 
  (Symbol.to_string s, Symbol.type_of s)

let expr_vars (e : Expression.t) : SS.t = 
  SS.of_list (List.map symbol_to_tuple (Expression.get_symbols [e]))


let rec remaining_vars_aux (xs : SS.t) (yss : SS.t list) : SS.t = 
  let xs' 
    = List.fold_left
      (fun ac ys ->
        if (not (SS.disjoint ac ys))
          then SS.union ac ys 
          else ac    
      ) xs yss in 
  if SS.equal xs' xs 
    then xs' 
    else remaining_vars_aux xs' yss 

let remaining_vars (e : Expression.t) (es : Expression.t list) : SS.t = 
  remaining_vars_aux (expr_vars e) (List.map expr_vars es) 

let slice (e : Expression.t) (es : Expression.t list) : Expression.t list = 
  let xs = remaining_vars e es in 
  List.filter (fun e' -> not (SS.disjoint xs (expr_vars e'))) es


