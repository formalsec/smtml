open Ty

exception InvalidRelop

type qt =
  | Forall
  | Exists

type expr =
  | Val of Value.t
  | Ptr of int32 * expr
  | Unop of unop * expr
  | Binop of binop * expr * expr
  | Relop of relop * expr * expr
  | Cvtop of cvtop * expr
  | Triop of triop * expr * expr * expr
  | Symbol of Symbol.t
  | Extract of expr * int * int
  | Concat of expr * expr
  | Quantifier of qt * Symbol.t list * expr * expr list list

let concretize_ptr (e : expr) : Num.t option =
  (* TODO: this should work with symbolic pointers *)
  (* would probably introduce Memory Objects here *)
  match e with
  | Val (Num n) -> Some n
  | Ptr (base, Val (Num (I32 offset))) -> Some (I32 (Int32.add base offset))
  | _ -> None

let concretize_base_ptr (e : expr) : int32 option =
  match e with Ptr (base, _) -> Some base | _ -> None

let to_bool (e : expr) : expr option =
  match e with
  | Val _ | Ptr _ -> None
  | (Relop _ as e') | Cvtop (I32 OfBool, e') -> Some e'
  | _ -> Some (Cvtop (I32 ToBool, e))
