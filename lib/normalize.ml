open Types
open Expression
open Infixes

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
  | Types.Real rop ->
    (match rop with 
    | Gt -> 
      RealInfix.(((neg e1) + e2 + (const 1.0)) <= (const 0.0))
    | Lt -> 
      RealInfix.((e1 - e2 + (const 1.0)) <= (const 0.0))
    | Ge -> 
      let e' = Expression.Binop(Real Sub, e1, e2) in
      Relop (Real Le, e', Val (Real 0.0));
    | _ -> Relop (op, e1, e2);
    )
  | Types.I32 i32op ->
    (match i32op with
    | GtS -> 
      I32Infix.(((neg e1) + e2 + (const 1)) <=+ (const 0)) 
    | GtU -> 
      I32Infix.(((neg e1) + e2 + (const 1)) <= (const 0)) 
    | LtS ->
      I32Infix.((e1 - e2 + (const 1)) <=+ (const 0))
    | LtU ->
      I32Infix.((e1 - e2 + (const 1)) <= (const 0))
    | GeS ->
      let e' = Expression.Binop(I32 Sub, e1, e2) in
      Relop (I32 LeS, e', I32Infix.(const 0));
    | GeU ->
      let e' = Expression.Binop(I32 Sub, e1, e2) in
      Relop (I32 LeU, e', I32Infix.(const 0));
    | _ -> Relop (op, e1, e2);
    )
  | Types.I64 i64op ->
    (match i64op with
    | GtS -> 
      I64Infix.(((neg e1) + e2 + (const 1)) <=+ (const 0)) 
    | GtU -> 
      I64Infix.(((neg e1) + e2 + (const 1)) <= (const 0)) 
    | LtS ->
      I64Infix.((e1 - e2 + (const 1)) <=+ (const 0))
    | LtU ->
      I64Infix.((e1 - e2 + (const 1)) <= (const 0))
    | GeS ->
      let e' = Expression.Binop(I64 Sub, e1, e2) in
      Relop (I64 LeS, e', I64Infix.(const 0));
    | GeU ->
      let e' = Expression.Binop(I64 Sub, e1, e2) in
      Relop (I64 LeU, e', I64Infix.(const 0));
    | _ -> Relop (op, e1, e2);
    )
  | Types.F32 f32op ->
    (match f32op with
    | Gt -> 
      F32Infix.(((neg e1) + e2 + (const 1.0)) <= (const 0.0))
    | Lt -> 
      F32Infix.((e1 - e2 + (const 1.0)) <= (const 0.0))
    | Ge -> 
      let e' = Expression.Binop(F32 Sub, e1, e2) in
      Relop (F32 Le, e', F32Infix.(const 0.0));
    | _ -> Relop (op, e1, e2);
    )
  | Types.F64 f64op ->
    (match f64op with
    | Gt -> 
      F64Infix.(((neg e1) + e2 + (const 1.0)) <= (const 0.0))
    | Lt -> 
      F64Infix.((e1 - e2 + (const 1.0)) <= (const 0.0))
    | Ge -> 
      let e' = Expression.Binop(F64 Sub, e1, e2) in
      Relop (F64 Le, e', F64Infix.(const 0.0));
    | _ -> Relop (op, e1, e2);
    )
  | _ -> Relop (op, e1, e2)



let rec normalize_aux (tbl : (string, string) Hashtbl.t) (e : Expression.t) : Expression.t = 
  let f = normalize_aux tbl in 
  match e with 
  | Val v -> Val v
  | SymPtr (t, e) -> SymPtr (t, f e)
  | Binop (op, e1, e2) -> Binop (op, f e1, f e2)
  | Unop  (op, e) -> Unop (op, f e)
  | Relop (op, e1, e2) -> normalize_relop op (f e1) (f e2)
  (* | Relop (op, e1, e2) -> Relop (op, f e1, f e2) *)
  | Cvtop (op, e) -> Cvtop (op, f e) 
  | Triop (op, e1, e2, e3) -> Triop (op, f e1, f e2, f e3)
  | Symbol (t) -> Symbol (Symbol.mk_symbol (Symbol.type_of t) (rename_var tbl (Symbol.to_string t) (Symbol.type_of t)))
  | Extract (e, t1, t2) -> Extract (f e, t1, t2)
  | Concat (e1, e2) -> Concat (f e1, f e2)
  | Quantifier (qt, vars, e, es) -> Quantifier (qt, vars, f e, es) (* TODO: es? *)

let normalize (es : Expression.t list) : Expression.t list = 
  let tbl = Hashtbl.create 0 in 
  List.map (normalize_aux tbl) es
  