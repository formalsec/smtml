(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by the Smtml programmers *)

open Dolmen
module Loc = Std.Loc

(* FIXME: Dangerous global reference *)
let custom_sorts = Hashtbl.create 10

let pp_loc fmt = function
  | None -> ()
  | Some loc -> Fmt.pf fmt "%a: " Loc.print_compact loc

let z_of_string_opt str =
  match Z.of_string str with
  | exception Invalid_argument _ -> None
  | z -> Some z

module Term = struct
  type t = Expr.t

  let fp_of_size f ebits sbits =
    match (ebits, sbits) with
    | "8", "24" -> Expr.value (Num (F32 (Int32.bits_of_float f)))
    | "11", "53" -> Expr.value (Num (F64 (Int64.bits_of_float f)))
    | _ ->
      Fmt.failwith "fp_of_size: unsupported %a (fp %a %a)" Fmt.float f
        Fmt.string ebits Fmt.string sbits

  let const ?loc (id : Symbol.t) : t =
    match (Symbol.namespace id, Symbol.name id) with
    | Sort, Simple name -> begin
      match name with
      | "Int" -> Expr.symbol { id with ty = Ty_int }
      | "Real" -> Expr.symbol { id with ty = Ty_real }
      | "Bool" -> Expr.symbol { id with ty = Ty_bool }
      | "String" -> Expr.symbol { id with ty = Ty_str }
      | "Float32" -> Expr.symbol { id with ty = Ty_fp 32 }
      | "Float64" -> Expr.symbol { id with ty = Ty_fp 64 }
      | "RoundingMode" -> Expr.symbol { id with ty = Ty_roundingMode }
      | "RegLan" -> Expr.symbol { id with ty = Ty_regexp }
      | _ -> begin
        match Hashtbl.find_opt custom_sorts name with
        | Some ty -> Expr.symbol { id with ty }
        | None ->
          Log.err (fun k ->
            k "%acould not find sort: %a" pp_loc loc Symbol.pp id );
          Expr.symbol id
      end
    end
    | Sort, Indexed { basename; indices } -> begin
      match (basename, indices) with
      | "BitVec", [ n ] -> (
        match int_of_string_opt n with
        | Some n -> Expr.symbol { id with ty = Ty_bitv n }
        | None -> Fmt.failwith "Invalid bitvector size" )
      | "FloatingPoint", [ e; s ] -> (
        match (int_of_string_opt e, int_of_string_opt s) with
        | Some e, Some s -> Expr.symbol { id with ty = Ty_fp (e + s) }
        | _ -> Fmt.failwith "Invalid floating point size" )
      | _ ->
        Fmt.failwith "%acould not parse indexed sort:%a %a@." pp_loc loc
          Fmt.string basename
          (Fmt.parens (Fmt.list ~sep:Fmt.sp Fmt.string))
          indices
    end
    | Term, Simple name -> begin
      match name with
      | "true" -> Expr.value True
      | "false" -> Expr.value False
      | "roundNearestTiesToEven" | "RNE" | "roundNearestTiesToAway" | "RNA"
      | "roundTowardPositive" | "RTP" | "roundTowardNegative" | "RTN"
      | "roundTowardZero" | "RTZ" ->
        Expr.symbol { id with ty = Ty_roundingMode }
      | "re.all" | "re.allchar" | "re.none" ->
        Expr.symbol { id with ty = Ty_regexp }
      | _ -> Expr.symbol id
    end
    | Term, Indexed { basename = base; indices } -> begin
      match (base, indices) with
      | bv, [ numbits ] when String.starts_with ~prefix:"bv" bv -> begin
        let str = String.sub bv 2 (String.length bv - 2) in
        match (z_of_string_opt str, int_of_string_opt numbits) with
        | Some z, Some width -> Expr.value (Bitv (Bitvector.make z width))
        | (None | Some _), _ -> assert false
      end
      | "+oo", [ ebits; sbits ] -> fp_of_size Float.infinity ebits sbits
      | "-oo", [ ebits; sbits ] -> fp_of_size Float.neg_infinity ebits sbits
      | "+zero", [ ebits; sbits ] -> fp_of_size Float.zero ebits sbits
      | "-zero", [ ebits; sbits ] ->
        fp_of_size (Float.neg Float.zero) ebits sbits
      | "NaN", [ ebits; sbits ] -> fp_of_size Float.nan ebits sbits
      | _ ->
        Log.debug (fun k -> k "const: unknown %a making app" Symbol.pp id);
        Expr.symbol id
    end
    | Attr, Simple _ -> Expr.symbol id
    | Attr, Indexed _ -> assert false
    | Var, _ -> Fmt.failwith "%acould not parse var: %a" pp_loc loc Symbol.pp id

  let str ?loc:_ (x : string) = Expr.value (Str x)

  let int ?loc (x : string) =
    match int_of_string_opt x with
    | Some x -> Expr.value (Int x)
    | None -> Fmt.failwith "%ainvalid int" pp_loc loc

  let real ?loc (x : string) =
    match float_of_string_opt x with
    | Some x -> Expr.value (Real x)
    | None -> Fmt.failwith "%ainvalid real" pp_loc loc

  let hexa ?loc:_ (h : string) =
    let len = String.length h in
    let hex = String.sub h 1 (len - 1) in
    let int = Z.of_string (String.cat "0" hex) in
    Expr.value (Bitv (Bitvector.make int ((len - 2) * 4)))

  let binary ?loc:_ (b : string) =
    let set (s : string) (i : int) (n : char) =
      let bs = Bytes.of_string s in
      Bytes.set bs i n;
      Bytes.to_string bs
    in
    let bv = set b 0 '0' in
    let len = String.length bv in
    let int = Z.of_string bv in
    Expr.value (Bitv (Bitvector.make int (len - 2)))

  let colon ?loc (symbol : t) (term : t) : t =
    match symbol with
    | Expr.Sym { node = Symbol s; _ } ->
      (* Hack: var bindings are 1 argument lambdas *)
      Log.debug (fun k -> k "colon: unknown '%a' making app" Expr.pp symbol);
      Expr.app s [ term ]
    | _ ->
      Fmt.failwith "%acould not parse colon: %a %a" pp_loc loc Expr.pp symbol
        Expr.pp term

  let make_fp_binop symbol (op : Ty.Binop.t) rm a b =
    match rm with
    | Expr.Sym
        { node = Symbol { name = Simple "roundNearestTiesToEven"; _ }; _ } ->
      Expr.raw_binop Ty_none op a b
    | _ -> Expr.app symbol [ rm; a; b ]

  let apply ?loc (id : t) (args : t list) : t =
    match id with
    | Expr.Sym
        { node = Symbol ({ namespace = Term; name = Simple name; _ } as symbol)
        ; _
        } -> begin
      match (name, args) with
      | "-", [ a ] -> Expr.raw_unop Ty_none Neg a
      | "not", [ a ] -> Expr.raw_unop Ty_bool Not a
      | "and", [ a; b ] -> Expr.raw_binop Ty_bool And a b
      | "and", ts -> Expr.raw_naryop Ty_bool Logand ts
      | "or", [ a; b ] -> Expr.raw_binop Ty_bool Or a b
      | "or", ts -> Expr.raw_naryop Ty_bool Logor ts
      | "xor", [ a; b ] -> Expr.raw_binop Ty_bool Xor a b
      | "=>", [ a; b ] -> Expr.raw_binop Ty_bool Implies a b
      | "+", [ a; b ] -> Expr.raw_binop Ty_none Add a b
      | "+", hd :: tl ->
        List.fold_left (fun acc hd -> Expr.raw_binop Ty_none Add acc hd) hd tl
      | "-", [ a; b ] -> Expr.raw_binop Ty_none Sub a b
      | "*", [ a; b ] -> Expr.raw_binop Ty_none Mul a b
      | "/", [ a; b ] -> Expr.raw_binop Ty_none Div a b
      | "mod", [ a; b ] -> Expr.raw_binop Ty_none Rem a b
      | "ite", [ a; b; c ] -> Expr.triop Ty_bool Ite a b c
      | "=", [ a; b ] -> Expr.raw_relop Ty_bool Eq a b
      | "distinct", [ a; b ] -> Expr.raw_relop Ty_bool Ne a b
      | ">", [ a; b ] -> Expr.raw_relop Ty_none Gt a b
      | ">=", [ a; b ] -> Expr.raw_relop Ty_none Ge a b
      | "<", [ a; b ] -> Expr.raw_relop Ty_none Lt a b
      | "<=", [ a; b ] -> Expr.raw_relop Ty_none Le a b
      | "to_real", [ a ] -> Expr.raw_cvtop Ty_real Reinterpret_int a
      | "to_int", [ a ] -> Expr.raw_cvtop Ty_int Reinterpret_float a
      | "str.len", [ a ] -> Expr.raw_unop Ty_str Length a
      | "str.at", [ a; b ] -> Expr.raw_binop Ty_str At a b
      | "str.prefixof", [ a; b ] -> Expr.raw_binop Ty_str String_prefix a b
      | "str.suffixof", [ a; b ] -> Expr.raw_binop Ty_str String_suffix a b
      | "str.contains", [ a; b ] -> Expr.raw_binop Ty_str String_contains a b
      | "str.in_re", [ a; b ] -> Expr.raw_binop Ty_str String_in_re a b
      | "str.substr", [ a; b; c ] -> Expr.triop Ty_str String_extract a b c
      | "str.indexof", [ a; b; c ] -> Expr.triop Ty_str String_index a b c
      | "str.replace", [ a; b; c ] -> Expr.triop Ty_str String_replace a b c
      | "str.replace_all", [ a; b; c ] ->
        Expr.triop Ty_str String_replace_all a b c
      | "str.replace_re", [ a; b; c ] ->
        Expr.triop Ty_str String_replace_re a b c
      | "str.replace_re_all", [ a; b; c ] ->
        Expr.triop Ty_str String_replace_re_all a b c
      | "str.++", n -> Expr.raw_naryop Ty_str Concat n
      | "str.<", [ a; b ] -> Expr.raw_relop Ty_str Lt a b
      | "str.<=", [ a; b ] -> Expr.raw_relop Ty_str Le a b
      | "str.to_code", [ a ] -> Expr.raw_cvtop Ty_str String_to_code a
      | "str.from_code", [ a ] -> Expr.raw_cvtop Ty_str String_from_code a
      | "str.to_int", [ a ] -> Expr.raw_cvtop Ty_str String_to_int a
      | "str.from_int", [ a ] -> Expr.raw_cvtop Ty_str String_from_int a
      | "str.to_re", [ a ] -> Expr.raw_cvtop Ty_str String_to_re a
      | "re.*", [ a ] -> Expr.raw_unop Ty_regexp Regexp_star a
      | "re.+", [ a ] -> Expr.raw_unop Ty_regexp Regexp_plus a
      | "re.opt", [ a ] -> Expr.raw_unop Ty_regexp Regexp_opt a
      | "re.comp", [ a ] -> Expr.raw_unop Ty_regexp Regexp_comp a
      | "re.range", [ a; b ] -> Expr.raw_binop Ty_regexp Regexp_range a b
      | "re.diff", [ a; b ] -> Expr.raw_binop Ty_regexp Regexp_diff a b
      | "re.inter", [ a; b ] -> Expr.raw_binop Ty_regexp Regexp_inter a b
      | "re.inter", [ a; b; c ] ->
        Expr.raw_binop Ty_regexp Regexp_inter a
          (Expr.raw_binop Ty_regexp Regexp_inter b c)
      | "re.union", n -> Expr.raw_naryop Ty_regexp Regexp_union n
      | "re.++", n -> Expr.raw_naryop Ty_regexp Concat n
      | "bvnot", [ a ] -> Expr.raw_unop Ty_none Not a
      | "bvneg", [ a ] -> Expr.raw_unop Ty_none Neg a
      | "bvand", [ a; b ] -> Expr.raw_binop Ty_none And a b
      | "bvor", [ a; b ] -> Expr.raw_binop Ty_none Or a b
      | "bvxor", [ a; b ] -> Expr.raw_binop Ty_none Xor a b
      | "bvadd", [ a; b ] -> Expr.raw_binop Ty_none Add a b
      | "bvsub", [ a; b ] -> Expr.raw_binop Ty_none Sub a b
      | "bvmul", [ a; b ] -> Expr.raw_binop Ty_none Mul a b
      | "bvudiv", [ a; b ] -> Expr.raw_binop Ty_none DivU a b
      | "bvurem", [ a; b ] -> Expr.raw_binop Ty_none RemU a b
      | "bvshl", [ a; b ] -> Expr.raw_binop Ty_none Shl a b
      | "bvlshr", [ a; b ] -> Expr.raw_binop Ty_none ShrL a b
      | "bvashr", [ a; b ] -> Expr.raw_binop Ty_none ShrA a b
      | "bvslt", [ a; b ] -> Expr.raw_relop Ty_none Lt a b
      | "bvult", [ a; b ] -> Expr.raw_relop Ty_none LtU a b
      | "bvsle", [ a; b ] -> Expr.raw_relop Ty_none Le a b
      | "bvule", [ a; b ] -> Expr.raw_relop Ty_none LeU a b
      | "bvsgt", [ a; b ] -> Expr.raw_relop Ty_none Gt a b
      | "bvugt", [ a; b ] -> Expr.raw_relop Ty_none GtU a b
      | "bvsge", [ a; b ] -> Expr.raw_relop Ty_none Ge a b
      | "bvuge", [ a; b ] -> Expr.raw_relop Ty_none GeU a b
      | "concat", [ a; b ] -> Expr.raw_concat a b
      | "fp", [ Imm (Bitv sign); Imm (Bitv eb); Imm (Bitv i) ] ->
        let fp = Bitvector.(concat sign (concat eb i)) in
        let fp_sz = Bitvector.numbits fp in
        if fp_sz = 32 then Expr.value (Num (F32 (Bitvector.to_int32 fp)))
        else if fp_sz = 64 then Expr.value (Num (F64 (Bitvector.to_int64 fp)))
        else Fmt.failwith "%afp size not supported" pp_loc loc
      | "fp.isNormal", [ a ] -> Expr.raw_unop Ty_none Is_normal a
      | "fp.isSubnormal", [ a ] -> Expr.raw_unop Ty_none Is_subnormal a
      | "fp.isNegative", [ a ] -> Expr.raw_unop Ty_none Is_negative a
      | "fp.isPositive", [ a ] -> Expr.raw_unop Ty_none Is_positive a
      | "fp.isInfinite", [ a ] -> Expr.raw_unop Ty_none Is_infinite a
      | "fp.isNaN", [ a ] -> Expr.raw_unop Ty_none Is_nan a
      | "fp.isZero", [ a ] -> Expr.raw_unop Ty_none Is_zero a
      | "fp.abs", [ a ] -> Expr.raw_unop Ty_none Abs a
      | "fp.neg", [ a ] -> Expr.raw_unop Ty_none Neg a
      | "fp.add", [ rm; a; b ] -> make_fp_binop symbol Add rm a b
      | "fp.sub", [ rm; a; b ] -> make_fp_binop symbol Sub rm a b
      | "fp.mul", [ rm; a; b ] -> make_fp_binop symbol Mul rm a b
      | "fp.div", [ rm; a; b ] -> make_fp_binop symbol Div rm a b
      | ( "fp.sqrt"
        , [ Sym
              { node = Symbol { name = Simple "roundNearestTiesToEven"; _ }; _ }
          ; a
          ] ) ->
        Expr.raw_unop Ty_none Sqrt a
      | "fp.rem", [ a; b ] -> Expr.raw_binop Ty_none Rem a b
      | "fp.roundToIntegral", [ Sym rm; a ] -> begin
        match Expr.view rm with
        | Symbol { name = Simple "roundNearestTiesToEven"; _ } ->
          Expr.raw_unop Ty_none Nearest a
        | Symbol { name = Simple "roundTowardPositive"; _ } ->
          Expr.raw_unop Ty_none Ceil a
        | Symbol { name = Simple "roundTowardNegative"; _ } ->
          Expr.raw_unop Ty_none Floor a
        | Symbol { name = Simple "roundTowardZero"; _ } ->
          Expr.raw_unop Ty_none Trunc a
        | _ -> Expr.app symbol args
      end
      | "fp.min", [ a; b ] -> Expr.raw_binop Ty_none Min a b
      | "fp.max", [ a; b ] -> Expr.raw_binop Ty_none Max a b
      | "fp.leq", [ a; b ] -> Expr.raw_relop Ty_none Le a b
      | "fp.lt", [ a; b ] -> Expr.raw_relop Ty_none Lt a b
      | "fp.geq", [ a; b ] -> Expr.raw_relop Ty_none Ge a b
      | "fp.gt", [ a; b ] -> Expr.raw_relop Ty_none Gt a b
      | "fp.eq", [ a; b ] -> Expr.raw_relop Ty_none Eq a b
      | _ ->
        Log.debug (fun k -> k "apply: unknown %a making app" Symbol.pp symbol);
        Expr.app symbol args
    end
    | Sym
        { node = Symbol ({ name = Simple _; namespace = Attr; _ } as attr); _ }
      ->
      Log.debug (fun k -> k "apply: unknown %a making app" Symbol.pp attr);
      Expr.app attr args
    | Sym { node = Symbol { name = Indexed { basename; indices }; _ }; _ } ->
      begin
      match (basename, indices, args) with
      | "extract", [ h; l ], [ a ] ->
        let high =
          match int_of_string_opt h with
          | None -> assert false
          | Some h -> (h + 1) / 8
        in
        let low =
          match int_of_string_opt l with
          | None -> assert false
          | Some l -> l / 8
        in
        Expr.raw_extract a ~high ~low
      | "zero_extend", [ bits ], [ a ] ->
        let bits =
          match int_of_string_opt bits with
          | None -> assert false
          | Some bits -> bits
        in
        Expr.raw_cvtop Ty_none (Zero_extend bits) a
      | "re.loop", [ i1; i2 ], [ a ] ->
        let i1 =
          match int_of_string_opt i1 with None -> assert false | Some i1 -> i1
        in
        let i2 =
          match int_of_string_opt i2 with None -> assert false | Some i2 -> i2
        in
        Expr.raw_unop Ty_regexp (Regexp_loop (i1, i2)) a
      | ( "to_fp"
        , [ "11"; "53" ]
        , [ Sym
              { node =
                  Symbol { name = Simple ("roundNearestTiesToEven" | "RNE"); _ }
              ; _
              }
          ; a
          ] ) ->
        Expr.raw_cvtop (Ty_fp 64) PromoteF32 a
      | _ ->
        Fmt.failwith "%acould not parse indexed app: %a" pp_loc loc Expr.pp id
    end
    | Sym { node = Symbol id; _ } ->
      Log.debug (fun k -> k "apply: unknown %a making app" Symbol.pp id);
      Expr.app id args
    | _ ->
      (* Ids can only be symbols. Any other expr here is super wrong *)
      assert false

  let letand ?loc:_ (vars : t list) (body : t) : t = Expr.let_in vars body

  let forall ?loc:_ (vars : t list) (body : t) : t = Expr.forall vars body

  let exists ?loc:_ (vars : t list) (body : t) : t = Expr.exists vars body

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
    match (id, ts1, ts2, return_sort) with
    | id, [], [], Expr.Sym { node = Symbol sort; _ } ->
      Declare_const { id; sort }
    | id, [], args, Sym { node = Symbol sort; _ } ->
      let args =
        List.map
          (fun e ->
            match e with
            | Expr.Sym { node = Symbol s; _ } -> s
            | _ -> assert false )
          args
      in
      Declare_fun { id; args; sort }
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
        ~use:(fun _ -> Logic.ALL)
        (Logic.of_string logic)
    in
    Set_logic logic
end

module Extension = struct
  let statement _ = None
end

include
  Dolmen.Smtlib2.Script.Latest.Make (Loc) (Symbol) (Term) (Statement)
    (Extension)
