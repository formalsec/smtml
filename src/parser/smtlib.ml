open Dolmen
module Loc = Std.Loc

let pp_loc fmt = function
  | None -> ()
  | Some loc -> Fmt.pf fmt "%a: " Loc.print_compact loc

module Term = struct
  type t = Expr.t

  let const ?loc (id : Symbol.t) : t =
    match (Symbol.namespace id, Symbol.name id) with
    | Sort, Simple name -> (
      match name with
      | "Int" -> Expr.symbol { id with ty = Ty_int }
      | "Real" -> Expr.symbol { id with ty = Ty_real }
      | "Bool" -> Expr.symbol { id with ty = Ty_bool }
      | _ -> Fmt.failwith "Could not parse sort: %a" Symbol.pp id )
    | Term, Simple name -> (
      match name with
      | "true" -> Expr.value True
      | "false" -> Expr.value False
      | _ -> Expr.symbol id )
    | Attr, _ ->
      Fmt.failwith "%acould not parse attr: %a" pp_loc loc Symbol.pp id
    | Var, _ -> Fmt.failwith "%acould not parse var: %a" pp_loc loc Symbol.pp id

  let str ?loc:_ (x : string) = Expr.value (Str x)

  let int ?loc (x : string) =
    match int_of_string x with
    | Some x -> Expr.value (Int x)
    | None -> Fmt.failwith "%aInvalid int" pp_loc loc

  let real ?loc (x : string) =
    match float_of_string x with
    | Some x -> Expr.value (Real x)
    | None -> Fmt.failwith "%aInvalid real" pp_loc loc

  let hexa ?loc:_ (_ : string) = assert false

  let binary ?loc:_ (_ : string) = assert false

  let colon ?loc:_ (_ : t) (_ : t) : t = assert false

  let apply ?loc (id : t) (args : t list) : t =
    match Expr.view id with
    | Symbol { namespace = Term; name = Simple name; _ } -> (
      match (name, args) with
      | "-", [ a ] -> Expr.unop Ty_none Neg a
      | "not", [ a ] -> Expr.unop Ty_bool Not a
      | "and", [ a; b ] -> Expr.binop Ty_bool And a b
      | "or", [ a; b ] -> Expr.binop Ty_bool Or a b
      | "xor", [ a; b ] -> Expr.binop Ty_bool Xor a b
      | "+", [ a; b ] -> Expr.binop Ty_none Add a b
      | "-", [ a; b ] -> Expr.binop Ty_none Sub a b
      | "*", [ a; b ] -> Expr.binop Ty_none Mul a b
      | "/", [ a; b ] -> Expr.binop Ty_none Div a b
      | "mod", [ a; b ] -> Expr.binop Ty_none Rem a b
      | "ite", [ a; b; c ] -> Expr.triop Ty_bool Ite a b c
      | "=", [ a; b ] -> Expr.relop Ty_bool Eq a b
      | "distinct", [ a; b ] -> Expr.relop Ty_bool Ne a b
      | ">", [ a; b ] -> Expr.relop Ty_none Gt a b
      | ">=", [ a; b ] -> Expr.relop Ty_none Ge a b
      | "<", [ a; b ] -> Expr.relop Ty_none Lt a b
      | "<=", [ a; b ] -> Expr.relop Ty_none Le a b
      | "to_real", [ a ] -> Expr.cvtop Ty_real Reinterpret_int a
      | "to_int", [ a ] -> Expr.cvtop Ty_int Reinterpret_float a
      | _ -> Fmt.failwith "%acould not parse term app: %s" pp_loc loc name )
    | Symbol id ->
      Fmt.failwith "%acould not parse app: %a" pp_loc loc Symbol.pp id
    | _ ->
      (* Ids can only be symbols. Any other expr here is super wrong *)
      assert false

  let letand ?loc:_ = assert false

  let forall ?loc:_ = assert false

  let exists ?loc:_ = assert false

  let match_ ?loc:_ = assert false

  let sexpr ?loc:_ = assert false

  let annot ?loc:_ = assert false
end

module Statement = struct
  open Ast

  type t = Ast.t

  let reset ?loc:_ () = Reset

  let exit ?loc:_ () = Exit

  let push ?loc:_ n = Push n

  let pop ?loc:_ n = Pop n

  let reset_assertions ?loc:_ () = Reset_assertions

  let type_decl ?loc:_ = assert false

  let type_def ?loc:_ = assert false

  let datatypes ?loc:_ = assert false

  let fun_decl ?loc id ts1 ts2 return_sort =
    match (id, ts1, ts2, Expr.view return_sort) with
    | id, [], [], Symbol sort -> Declare_const { id; sort }
    | _ ->
      Fmt.failwith "%afun_decl %a (%a) (%a) %a" pp_loc loc Symbol.pp id
        (Fmt.list Expr.pp) ts1 (Fmt.list Expr.pp) ts2 Expr.pp return_sort

  let fun_def ?loc:_ = assert false

  let funs_def_rec ?loc:_ _ = assert false

  let assert_ ?loc:_ term = Assert term

  let get_assertions ?loc:_ () = Get_assertions

  let check_sat ?loc:_ terms = Check_sat terms

  let get_model ?loc:_ () = Get_model

  let get_value ?loc:_ terms = Get_value terms

  let get_assignment ?loc:_ () = Get_assignment

  let get_proof ?loc:_ () = assert false

  let get_unsat_core ?loc:_ () = assert false

  let get_unsat_assumptions ?loc:_ () = assert false

  let get_info ?loc:_ info = Get_info info

  let get_option ?loc:_ opt = Get_option opt

  let echo ?loc:_ x = Echo x

  let set_info ?loc:_ term = Set_info term

  let set_option ?loc:_ term = Set_option term

  let set_logic ?loc:_ _logic =
    (* TODO: Ty.logic_of_string *)
    (* Set_logic (Ty.logic_of_string logic) *)
    assert false
end

include Dolmen.Smtlib2.Script.Latest.Make (Loc) (Symbol) (Term) (Statement)
