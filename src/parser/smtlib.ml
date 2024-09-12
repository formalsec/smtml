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
      | "String" -> Expr.symbol { id with ty = Ty_str }
      | _ -> Fmt.failwith "Could not parse sort: %a" Symbol.pp id )
    | Sort, Indexed { basename; indices } -> (
      match (basename, indices) with
      | "BitVec", [ n ] -> (
        match int_of_string n with
        | Some n -> Expr.symbol { id with ty = Ty_bitv n }
        | None -> Fmt.failwith "Invalid bitvector size" )
      | _ ->
        Fmt.failwith "%acould not parse indexed sort:%a %a@." pp_loc loc
          Fmt.string basename
          (Fmt.parens (Fmt.list ~sep:Fmt.sp Fmt.string))
          indices )
    | Term, Simple name -> (
      match name with
      | "true" -> Expr.value True
      | "false" -> Expr.value False
      | _ -> Expr.symbol id )
    | Term, Indexed { basename = base; indices } -> (
      match String.(sub base 0 2, sub base 2 (length base - 2), indices) with
      | "bv", str, [ "8" ] ->
        Expr.value (Num (I8 (Option.get (int_of_string str))))
      | "bv", str, [ "32" ] -> Expr.value (Num (I32 (Int32.of_string str)))
      | "bv", str, [ "64" ] -> Expr.value (Num (I64 (Int64.of_string str)))
      | _ ->
        Fmt.failwith "%acould not parse indexed term: %a %a" pp_loc loc
          Fmt.string base
          (Fmt.parens (Fmt.list ~sep:Fmt.sp Fmt.string))
          indices )
    | Attr, Simple _ -> Expr.symbol id
    | Attr, Indexed _ -> assert false
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

  let colon ?loc (symbol : t) (term : t) : t =
    match Expr.view symbol with
    | Symbol s ->
      (* Hack: var bindings are 1 argument lambdas *)
      Expr.app s [ term ]
    | _ ->
      Fmt.failwith "%acould not parse colon: %a %a" pp_loc loc Expr.pp symbol
        Expr.pp term

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
      | "str.len", [ a ] -> Expr.unop Ty_str Length a
      | "str.at", [ a; b ] -> Expr.binop Ty_str At a b
      | "str.prefixof", [ a; b ] -> Expr.binop Ty_str String_prefix a b
      | "str.suffixof", [ a; b ] -> Expr.binop Ty_str String_suffix a b
      | "str.contains", [ a; b ] -> Expr.binop Ty_str String_contains a b
      | "str.substr", [ a; b; c ] -> Expr.triop Ty_str String_extract a b c
      | "str.indexof", [ a; b; c ] -> Expr.triop Ty_str String_index a b c
      | "str.replace", [ a; b; c ] -> Expr.triop Ty_str String_replace a b c
      | "str.++", n -> Expr.naryop Ty_str Concat n
      | "str.<", [ a; b ] -> Expr.relop Ty_str Lt a b
      | "str.<=", [ a; b ] -> Expr.relop Ty_str Le a b
      | "str.to_code", [ a ] -> Expr.cvtop Ty_str String_to_code a
      | "str.from_code", [ a ] -> Expr.cvtop Ty_str String_from_code a
      | "str.to_int", [ a ] -> Expr.cvtop Ty_str String_to_int a
      | "str.from_int", [ a ] -> Expr.cvtop Ty_str String_from_int a
      | "bvnot", [ a ] -> Expr.unop Ty_none Not a
      | "bvneg", [ a ] -> Expr.unop Ty_none Neg a
      | "bvand", [ a; b ] -> Expr.binop Ty_none And a b
      | "bvor", [ a; b ] -> Expr.binop Ty_none Or a b
      | "bvadd", [ a; b ] -> Expr.binop Ty_none Add a b
      | "bvmul", [ a; b ] -> Expr.binop Ty_none Mul a b
      | "bvudiv", [ a; b ] -> Expr.binop Ty_none DivU a b
      | "bvurem", [ a; b ] -> Expr.binop Ty_none RemU a b
      | "bvshl", [ a; b ] -> Expr.binop Ty_none Shl a b
      | "bvlshr", [ a; b ] -> Expr.binop Ty_none ShrL a b
      | "bvult", [ a; b ] -> Expr.relop Ty_none LtU a b
      | _ -> Fmt.failwith "%acould not parse term app: %s" pp_loc loc name )
    | Symbol ({ name = Simple _; namespace = Attr; _ } as attr) ->
      Expr.app attr args
    | Symbol id ->
      Fmt.failwith "%acould not parse app: %a" pp_loc loc Symbol.pp id
    | _ ->
      (* Ids can only be symbols. Any other expr here is super wrong *)
      assert false

  let letand ?loc:_ (vars : t list) (expr : t) : t = Expr.let_in vars expr

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
    (* FIXME: TODO Ty.logic_of_string *)
    (* Set_logic (Ty.logic_of_string logic) *)
    Set_logic ALL
end

include Dolmen.Smtlib2.Script.Latest.Make (Loc) (Symbol) (Term) (Statement)
