open Dolmen
module Loc = Std.Loc

let custom_sorts = Hashtbl.create 10

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
      | "Float32" -> Expr.symbol { id with ty = Ty_fp 32 }
      | "Float64" -> Expr.symbol { id with ty = Ty_fp 64 }
      | _ -> (
        try Expr.symbol { id with ty = Hashtbl.find custom_sorts name }
        with Not_found ->
          Fmt.failwith "%acould not find sort: %a" pp_loc loc Symbol.pp id ) )
    | Sort, Indexed { basename; indices } -> (
      match (basename, indices) with
      | "BitVec", [ n ] -> (
        match int_of_string n with
        | Some n -> Expr.symbol { id with ty = Ty_bitv n }
        | None -> Fmt.failwith "Invalid bitvector size" )
      | "FloatingPoint", [ e; s ] -> (
        match (int_of_string e, int_of_string s) with
        | Some e, Some s -> Expr.symbol { id with ty = Ty_fp (e + s) }
        | _ -> Fmt.failwith "Invalid floating point size" )
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
      | "bv", str, [ "32" ] ->
        let int = Option.get (int_of_string str) in
        Expr.value (Num (I32 (Int32.of_int (int land 0xffffffff))))
      | "bv", str, [ "64" ] -> Expr.value (Num (I64 (Int64.of_string str)))
      | _ -> Expr.symbol id )
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

  let binary ?loc:_ (b : string) =
    let set (s : string) (i : int) (n : char) =
      let bs = Bytes.of_string s in
      Bytes.set bs i n;
      Bytes.to_string bs
    in
    let bv = set b 0 '0' in
    Expr.value (Str bv)

  let colon ?loc (symbol : t) (term : t) : t =
    match Expr.view symbol with
    | Symbol s ->
      (* Hack: var bindings are 1 argument lambdas *)
      Expr.app s [ term ]
    | _ ->
      Fmt.failwith "%acould not parse colon: %a %a" pp_loc loc Expr.pp symbol
        Expr.pp term

  let combine_to_int64 sign_bit exponent_bit mantissa_bit =
    let sign = Int64.of_string sign_bit in
    let exponent = Int64.of_string exponent_bit in
    let mantissa = Int64.of_string mantissa_bit in
    let sign_shifted = Int64.shift_left sign 63 in
    let exponent_shifted = Int64.shift_left exponent 52 in
    Int64.logor sign_shifted (Int64.logor exponent_shifted mantissa)

  let combine_to_int32 sign_bit exponent_bit mantissa_bit =
    let sign = Int32.of_string sign_bit in
    let exponent = Int32.of_string exponent_bit in
    let mantissa = Int32.of_string mantissa_bit in
    let sign_shifted = Int32.shift_left sign 31 in
    let exponent_shifted = Int32.shift_left exponent 23 in
    Int32.logor sign_shifted (Int32.logor exponent_shifted mantissa)

  let apply ?loc (id : t) (args : t list) : t =
    match Expr.view id with
    | Symbol { namespace = Term; name = Simple name; _ } -> (
      match (name, args) with
      | "-", [ a ] -> Expr.unop' Ty_none Neg a
      | "not", [ a ] -> Expr.unop' Ty_bool Not a
      | "and", [ a; b ] -> Expr.binop' Ty_bool And a b
      | "and", ts -> Expr.naryop' Ty_bool Logand ts
      | "or", [ a; b ] -> Expr.binop' Ty_bool Or a b
      | "or", ts -> Expr.naryop' Ty_bool Logor ts
      | "xor", [ a; b ] -> Expr.binop' Ty_bool Xor a b
      | "+", [ a; b ] -> Expr.binop' Ty_none Add a b
      | "+", hd :: tl ->
        List.fold_left (fun acc hd -> Expr.binop' Ty_none Add acc hd) hd tl
      | "-", [ a; b ] -> Expr.binop' Ty_none Sub a b
      | "*", [ a; b ] -> Expr.binop' Ty_none Mul a b
      | "/", [ a; b ] -> Expr.binop' Ty_none Div a b
      | "mod", [ a; b ] -> Expr.binop' Ty_none Rem a b
      | "ite", [ a; b; c ] -> Expr.triop Ty_bool Ite a b c
      | "=", [ a; b ] -> Expr.relop' Ty_bool Eq a b
      | "distinct", [ a; b ] -> Expr.relop' Ty_bool Ne a b
      | ">", [ a; b ] -> Expr.relop' Ty_none Gt a b
      | ">=", [ a; b ] -> Expr.relop' Ty_none Ge a b
      | "<", [ a; b ] -> Expr.relop' Ty_none Lt a b
      | "<=", [ a; b ] -> Expr.relop' Ty_none Le a b
      | "to_real", [ a ] -> Expr.cvtop' Ty_real Reinterpret_int a
      | "to_int", [ a ] -> Expr.cvtop' Ty_int Reinterpret_float a
      | "str.len", [ a ] -> Expr.unop' Ty_str Length a
      | "str.at", [ a; b ] -> Expr.binop' Ty_str At a b
      | "str.prefixof", [ a; b ] -> Expr.binop' Ty_str String_prefix a b
      | "str.suffixof", [ a; b ] -> Expr.binop' Ty_str String_suffix a b
      | "str.contains", [ a; b ] -> Expr.binop' Ty_str String_contains a b
      | "str.in_re", [ a; b ] -> Expr.binop' Ty_str String_in_re a b
      | "str.substr", [ a; b; c ] -> Expr.triop Ty_str String_extract a b c
      | "str.indexof", [ a; b; c ] -> Expr.triop Ty_str String_index a b c
      | "str.replace", [ a; b; c ] -> Expr.triop Ty_str String_replace a b c
      | "str.++", n -> Expr.naryop' Ty_str Concat n
      | "str.<", [ a; b ] -> Expr.relop' Ty_str Lt a b
      | "str.<=", [ a; b ] -> Expr.relop' Ty_str Le a b
      | "str.to_code", [ a ] -> Expr.cvtop' Ty_str String_to_code a
      | "str.from_code", [ a ] -> Expr.cvtop' Ty_str String_from_code a
      | "str.to_int", [ a ] -> Expr.cvtop' Ty_str String_to_int a
      | "str.from_int", [ a ] -> Expr.cvtop' Ty_str String_from_int a
      | "str.to_re", [ a ] -> Expr.cvtop' Ty_str String_to_re a
      | "re.*", [ a ] -> Expr.unop' Ty_regexp Regexp_star a
      | "re.+", [ a ] -> Expr.unop' Ty_regexp Regexp_plus a
      | "re.opt", [ a ] -> Expr.unop' Ty_regexp Regexp_opt a
      | "re.comp", [ a ] -> Expr.unop' Ty_regexp Regexp_comp a
      | "re.range", [ a; b ] -> Expr.binop' Ty_regexp Regexp_range a b
      | "re.union", n -> Expr.naryop' Ty_regexp Regexp_union n
      | "re.++", n -> Expr.naryop' Ty_regexp Concat n
      | "bvnot", [ a ] -> Expr.unop' Ty_none Not a
      | "bvneg", [ a ] -> Expr.unop' Ty_none Neg a
      | "bvand", [ a; b ] -> Expr.binop' Ty_none And a b
      | "bvor", [ a; b ] -> Expr.binop' Ty_none Or a b
      | "bvxor", [ a; b ] -> Expr.binop' Ty_none Xor a b
      | "bvadd", [ a; b ] -> Expr.binop' Ty_none Add a b
      | "bvsub", [ a; b ] -> Expr.binop' Ty_none Sub a b
      | "bvmul", [ a; b ] -> Expr.binop' Ty_none Mul a b
      | "bvudiv", [ a; b ] -> Expr.binop' Ty_none DivU a b
      | "bvurem", [ a; b ] -> Expr.binop' Ty_none RemU a b
      | "bvshl", [ a; b ] -> Expr.binop' Ty_none Shl a b
      | "bvlshr", [ a; b ] -> Expr.binop' Ty_none ShrL a b
      | "bvashr", [ a; b ] -> Expr.binop' Ty_none ShrA a b
      | "bvslt", [ a; b ] -> Expr.relop' Ty_none Lt a b
      | "bvult", [ a; b ] -> Expr.relop' Ty_none LtU a b
      | "bvsle", [ a; b ] -> Expr.relop' Ty_none Le a b
      | "bvule", [ a; b ] -> Expr.relop' Ty_none LeU a b
      | "bvsgt", [ a; b ] -> Expr.relop' Ty_none Gt a b
      | "bvugt", [ a; b ] -> Expr.relop' Ty_none GtU a b
      | "bvsge", [ a; b ] -> Expr.relop' Ty_none Ge a b
      | "bvuge", [ a; b ] -> Expr.relop' Ty_none GeU a b
      | "concat", [ a; b ] -> Expr.concat' a b
      | "fp", [ s; eb; i ] -> (
        match (Expr.view s, Expr.view eb, Expr.view i) with
        | Val (Str sign), Val (Str eb), Val (Str i) -> (
          match (String.length sign, String.length eb, String.length i) with
          (* 32 bit float -> sign = 1, eb = 8, i = 24 - 1 = 23  *)
          | 3, 10, 25 -> Expr.value (Num (F32 (combine_to_int32 sign eb i)))
          (* 64 bit float -> sign = 1, eb = 11, i = 53 - 1 = 52  *)
          | 3, 13, 54 -> Expr.value (Num (F64 (combine_to_int64 sign eb i)))
          | _ -> Fmt.failwith "%afp size not supported" pp_loc loc )
        | _ ->
          Fmt.failwith "%acould not parse fp: %a %a %a" pp_loc loc Expr.pp s
            Expr.pp eb Expr.pp i )
      | "fp.abs", [ a ] -> Expr.unop' Ty_none Abs a
      | "fp.neg", [ a ] -> Expr.unop' Ty_none Neg a
      | ( "fp.add"
        , [ { node = Symbol { name = Simple "roundNearestTiesToEven"; _ }; _ }
          ; a
          ; b
          ] ) ->
        Expr.binop' Ty_none Add a b
      | ( "fp.sub"
        , [ { node = Symbol { name = Simple "roundNearestTiesToEven"; _ }; _ }
          ; a
          ; b
          ] ) ->
        Expr.binop' Ty_none Sub a b
      | ( "fp.mul"
        , [ { node = Symbol { name = Simple "roundNearestTiesToEven"; _ }; _ }
          ; a
          ; b
          ] ) ->
        Expr.binop' Ty_none Mul a b
      | ( "fp.div"
        , [ { node = Symbol { name = Simple "roundNearestTiesToEven"; _ }; _ }
          ; a
          ; b
          ] ) ->
        Expr.binop' Ty_none Div a b
      | ( "fp.sqrt"
        , [ { node = Symbol { name = Simple "roundNearestTiesToEven"; _ }; _ }
          ; a
          ] ) ->
        Expr.unop' Ty_none Sqrt a
      | "fp.rem", [ a; b ] -> Expr.binop' Ty_none Rem a b
      | ( "fp.roundToIntegral"
        , [ { node = Symbol { name = Simple "roundNearestTiesToEven"; _ }; _ }
          ; a
          ] ) ->
        Expr.unop' Ty_none Nearest a
      | ( "fp.roundToIntegral"
        , [ { node = Symbol { name = Simple "roundTowardPositive"; _ }; _ }; a ]
        ) ->
        Expr.unop' Ty_none Ceil a
      | ( "fp.roundToIntegral"
        , [ { node = Symbol { name = Simple "roundTowardNegative"; _ }; _ }; a ]
        ) ->
        Expr.unop' Ty_none Floor a
      | ( "fp.roundToIntegral"
        , [ { node = Symbol { name = Simple "roundTowardZero"; _ }; _ }; a ] )
        ->
        Expr.unop' Ty_none Trunc a
      | "fp.min", [ a; b ] -> Expr.binop' Ty_none Min a b
      | "fp.max", [ a; b ] -> Expr.binop' Ty_none Max a b
      | "fp.leq", [ a; b ] -> Expr.relop' Ty_bool Le a b
      | "fp.lt", [ a; b ] -> Expr.relop' Ty_bool Lt a b
      | "fp.geq", [ a; b ] -> Expr.relop' Ty_bool Ge a b
      | "fp.gt", [ a; b ] -> Expr.relop' Ty_bool Gt a b
      | "fp.eq", [ a; b ] -> Expr.relop' Ty_bool Eq a b
      | _ -> Fmt.failwith "%acould not parse term app: %s" pp_loc loc name )
    | Symbol ({ name = Simple _; namespace = Attr; _ } as attr) ->
      Expr.app attr args
    | Symbol { name = Indexed { basename; indices }; _ } -> (
      match (basename, indices, args) with
      | "extract", [ h; l ], [ a ] ->
        let high = ((Option.get @@ int_of_string h) + 1) / 8 in
        let low = (Option.get @@ int_of_string l) / 8 in
        Expr.extract' a ~high ~low
      | "zero_extend", [ bits ], [ a ] ->
        let bits = Option.get @@ int_of_string bits in
        Expr.cvtop' Ty_none (Zero_extend bits) a
      | "re.loop", [ i1; i2 ], [ a ] ->
        let i1 = Option.get @@ int_of_string i1 in
        let i2 = Option.get @@ int_of_string i2 in
        Expr.unop' Ty_regexp (Regexp_loop (i1, i2)) a
      | _ ->
        Fmt.failwith "%acould not parse indexed app: %a" pp_loc loc Expr.pp id )
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

  let type_def ?loc:_ id _is t =
    let name =
      match Symbol.name id with Simple name -> name | _ -> assert false
    in
    Hashtbl.replace custom_sorts name (Expr.ty t);
    Echo ""

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

  let set_logic ?loc logic =
    let logic =
      Log.on_error ~level:Logs.Debug
        ~pp:(fun fmt (`Msg err) -> Fmt.pf fmt "%a%s. Using: ALL" pp_loc loc err)
        ~use:(fun _ -> Ty.ALL)
        (Ty.logic_of_string logic)
    in
    Set_logic logic
end

module Extension = struct
  let statement _ = None
end

include
  Dolmen.Smtlib2.Script.Latest.Make (Loc) (Symbol) (Term) (Statement)
    (Extension)
