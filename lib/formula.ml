open Base

type formula =
  | True
  | False
  | Not of formula
  | And of formula * formula
  | Or of formula * formula
  | Relop of Expression.t
  | Quantifier of
      at * (string * Types.expr_type) list * formula * Expression.t list list

and at = Forall | Exists

type t = formula

let ( && ) (f1 : t) (f2 : t) : t = And (f1, f2)
let ( || ) (f1 : t) (f2 : t) : t = Or (f1, f2)
let create () : t = True

let add_constraint ?(neg : bool = false) (e : Expression.t) (pc : t) : t =
  let cond =
    let c = Expression.to_relop (Expression.simplify e) in
    if neg then Option.map ~f:Expression.negate_relop c else c
  in
  match (cond, pc) with
  | None, _ -> pc
  | Some cond, True -> Relop cond
  | Some cond, _ -> And (Relop cond, pc)

let rec negate (f : t) : t =
  match f with
  | True -> False
  | False -> True
  | Not c -> c
  | And (c1, c2) -> Or (negate c1, negate c2)
  | Or (c1, c2) -> And (negate c1, negate c2)
  | Relop e -> Relop (Expression.negate_relop e)
  (*TODO: axiom negation*)
  | Quantifier (t, vars, body, patterns) -> Quantifier (t, vars, body, patterns)

let conjunct (conds : t list) : t =
  if List.is_empty conds then True
  else
    let rec loop (acc : t) = function
      | [] -> acc
      | h :: t -> loop (And (acc, h)) t
    in
    loop (List.hd_exn conds) (List.tl_exn conds)

let rec vars_list_to_str (l : (string * Types.expr_type) list) (acc : string) : string =
  match l with
  | (name, t) :: rest ->
      let acc = acc ^ name ^ " " ^ Types.string_of_type t in
      let acc = match rest with
      | []  -> "( " ^ acc ^ " )" 
      | _ -> acc ^ ", "
      in
      vars_list_to_str rest acc
  | [] -> acc

let patterns_to_string (patterns : Expression.t list list) : string =
  let rec aux (l : Expression.t list) (acc : string) : string =
    match l with 
    | e :: t -> 
      let acc = (acc ^ (Expression.to_string e) ^ ", ") in
      aux t acc
    | [] -> "pattern: ( " ^ acc ^ " ) "
  in let string_list = List.map ~f:(fun v -> aux v "") patterns in
  "(" ^ String.concat string_list ^ ")"
  


let rec to_string_aux (p : Expression.t -> string) (f : t) : string =
  match f with
  | True -> "True"
  | False -> "False"
  | Not c -> "(Not " ^ to_string_aux p c ^ ")"
  | And (c1, c2) ->
      let c1_str = to_string_aux p c1 and c2_str = to_string_aux p c2 in
      "(" ^ c1_str ^ " /\\ " ^ c2_str ^ ")"
  | Or (c1, c2) ->
      let c1_str = to_string_aux p c1 and c2_str = to_string_aux p c2 in
      "(" ^ c1_str ^ " \\/ " ^ c2_str ^ ")"
  | Relop e -> p e
  | Quantifier (at, vars, body, patterns) -> 
      let at' = 
      match at with
      | Forall -> "Forall"
      | Exists -> "Exists"
    in
    let vars = vars_list_to_str vars "" in
    let body' = to_string_aux p body in
    let patterns = patterns_to_string patterns in
    at' ^ "\n" ^ vars ^ "\n" ^ body' ^ "\n" ^ patterns 
      

let to_string (f : t) : string = to_string_aux Expression.to_string f
let pp_to_string (f : t) : string = to_string_aux Expression.pp_to_string f

let rec length (e : t) : int =
  match e with
  | True | False | Relop _ -> 1
  | Not c -> 1 + length c
  | And (c1, c2) -> 1 + length c1 + length c2
  | Or (c1, c2) -> 1 + length c1 + length c2
  | Quantifier (_, _, body, _) -> length body

let to_formulas (pc : Expression.t list) : t list =
  List.map ~f:(fun e -> Relop e) pc

let to_formula (pc : Expression.t list) : t = conjunct (to_formulas pc)



let rec get_symbols (e : t) : (string * Types.expr_type) list =
  let symbols =
    match e with
    | True | False -> []
    | Not c -> get_symbols c
    | And (c1, c2) -> get_symbols c1 @ get_symbols c2
    | Or (c1, c2) -> get_symbols c1 @ get_symbols c2
    | Relop e -> Expression.get_symbols e
    | Quantifier (_, vars, _, _) -> vars
  in
  let equal (x1, _) (x2, _) = String.equal x1 x2 in
  List.fold symbols ~init:[]
    ~f:(fun accum x -> if List.mem accum x ~equal then accum else x :: accum)