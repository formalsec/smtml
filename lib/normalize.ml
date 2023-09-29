open Types
open Expression

let new_var (i : int) (t : expr_type) : string =
  "__x" ^ (string_of_int i) ^ "_" ^ string_of_type t

let rename_var (tbl : (string, string) Hashtbl.t) (x : string) (t : expr_type): string = 
  match Hashtbl.find_opt tbl x with 
  | Some x' -> x' 
  | None -> 
      let i = Hashtbl.length tbl in 
      let x' = new_var i t in 
      Hashtbl.add tbl x x';
      x' 


let normalize_relop (op : relop) (e1 : Expression.t) (e2: Expression.t) : Expression.t = 
  match op with 
  | Types.Int iop -> 
    (match iop with 
    | Gt -> 
      (* e1 > e2 -> e2 < e1 -> -e1 + e2 + 1 <= 0 *)
      IntInfix.(((neg e1) + e2 + (const 1)) <= (const 0))
    | Lt -> 
      (* e1 < e2 -> e1 - e2 < 0 -> e1 - e2 + 1 <= 0 *)
      IntInfix.((e1 - e2 + (const 1)) <= (const 0))
    | Ge -> 
      (* e1 >= e2 -> e1 - e2 <= 0 *)
      let e' = Expression.Binop(Int Sub, e1, e2) in
      Relop (Int Le, e', Val (Int 0));
    | _ -> Relop (op, e1, e2);
      (* Le, Eq, Ne -> stay the same *)
    ) 
  | Types.Bool _ -> 
      assert false 
  | _ -> Relop (op, e1, e2)


let rec normalize_aux (tbl : (string, string) Hashtbl.t) (e : Expression.t) : Expression.t = 
  let f = normalize_aux tbl in 
  match e with 
  | Val v -> Val v
  | SymPtr (t, e) -> SymPtr (t, f e)
  | Binop (op, e1, e2) -> Binop (op, f e1, f e2)
  | Unop  (op, e) -> Unop (op, f e)
  | Relop (op, e1, e2) -> normalize_relop op (f e1) (f e2) 
  | Cvtop (op, e) -> Cvtop (op, f e) 
  (* | Symbol (t, x) -> Symbol (t, rename_var tbl x t) *)
  | Triop (op, e1, e2, e3) -> Triop (op, f e1, f e2, f e3)
  | Symbol (t) -> Symbol (t)
  | Extract (e, t1, t2) -> Extract (f e, t1, t2)
  | Concat (e1, e2) -> Concat (f e1, f e2)
  | _ -> assert false (* tirar *)

let normalize (e : Expression.t) : Expression.t = 
  let tbl = Hashtbl.create 0 in 
  normalize_aux tbl e