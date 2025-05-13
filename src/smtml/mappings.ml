(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by the Smtml programmers *)

include Mappings_intf

module Make (M_with_make : M_with_make) : S_with_fresh = struct
  module Make_ (M : M) : S = struct
    open Ty
    module Smap = Map.Make (Symbol)

    type symbol_ctx = M.term Smap.t

    type model =
      { model : M.model
      ; ctx : symbol_ctx
      }

    type solver =
      { solver : M.solver
      ; ctx : symbol_ctx Stack.t
      ; mutable last_ctx :
          symbol_ctx option (* Used to save last check-sat ctx *)
      }

    type handle = M.handle

    type optimize =
      { opt : M.optimizer
      ; ctx : symbol_ctx Stack.t
      }

    let i8 = M.Types.bitv 8

    let i32 = M.Types.bitv 32

    let i64 = M.Types.bitv 64

    let f32 = M.Types.float 8 24

    let f64 = M.Types.float 11 53

    let int2str = M.Func.make "int_to_string" [ M.Types.int ] M.Types.string

    let str2int = M.Func.make "string_to_int" [ M.Types.string ] M.Types.int

    let real2str = M.Func.make "real_to_string " [ M.Types.real ] M.Types.string

    let str2real = M.Func.make "string_to_real" [ M.Types.string ] M.Types.real

    let str_trim = M.Func.make "string_trim" [ M.Types.string ] M.Types.string

    let f32_to_i32 = M.Func.make "f32_to_i32" [ f32 ] i32

    let f64_to_i64 = M.Func.make "f64_to_i64" [ f64 ] i64

    let get_type = function
      | Ty_int -> M.Types.int
      | Ty_real -> M.Types.real
      | Ty_bool -> M.Types.bool
      | Ty_str -> M.Types.string
      | Ty_bitv 8 -> i8
      | Ty_bitv 32 -> i32
      | Ty_bitv 64 -> i64
      | Ty_bitv n -> M.Types.bitv n
      | Ty_fp 32 -> f32
      | Ty_fp 64 -> f64
      | (Ty_fp _ | Ty_list | Ty_app | Ty_unit | Ty_none | Ty_regexp) as ty ->
        Fmt.failwith "Unsupported theory: %a@." Ty.pp ty

    let make_symbol (ctx : symbol_ctx) (s : Symbol.t) : symbol_ctx * M.term =
      let name = match s.name with Simple name -> name | _ -> assert false in
      if M.Internals.caches_consts then
        let sym = M.const name (get_type s.ty) in
        (Smap.add s sym ctx, sym)
      else
        match Smap.find_opt s ctx with
        | Some sym -> (ctx, sym)
        | None ->
          let sym = M.const name (get_type s.ty) in
          (Smap.add s sym ctx, sym)

    module Bool_impl = struct
      let true_ = M.true_

      let false_ = M.false_

      let unop = function
        | Unop.Not -> M.not_
        | op ->
          Fmt.failwith {|Bool: Unsupported Z3 unop operator "%a"|} Unop.pp op

      let binop = function
        | Binop.And -> M.and_
        | Or -> M.or_
        | Xor -> M.xor
        | Implies -> M.implies
        | op ->
          Fmt.failwith {|Bool: Unsupported Z3 binop operator "%a"|} Binop.pp op

      let triop = function
        | Triop.Ite -> M.ite
        | op ->
          Fmt.failwith {|Bool: Unsupported Z3 triop operator "%a"|} Triop.pp op

      let relop op e1 e2 =
        match op with
        | Relop.Eq -> M.eq e1 e2
        | Ne -> M.distinct [ e1; e2 ]
        | _ ->
          Fmt.failwith {|Bool: Unsupported Z3 relop operator "%a"|} Relop.pp op

      let naryop op l =
        match op with
        | Naryop.Logand -> M.logand l
        | Logor -> M.logor l
        | _ ->
          Fmt.failwith {|Bool: Unsupported Z3 naryop operator "%a"|} Naryop.pp
            op

      let cvtop _op _e = assert false
    end

    module Int_impl = struct
      let v i = M.int i [@@inline]

      let unop = function
        | Unop.Neg -> M.Int.neg
        | op -> Fmt.failwith {|Int: Unsupported unop operator "%a"|} Unop.pp op

      let binop = function
        | Binop.Add -> M.Int.add
        | Sub -> M.Int.sub
        | Mul -> M.Int.mul
        | Div -> M.Int.div
        | Rem -> M.Int.rem
        | Pow -> M.Int.pow
        | op ->
          Fmt.failwith {|Int: Unsupported binop operator "%a"|} Binop.pp op

      let relop = function
        | Relop.Eq | Ne -> assert false
        | Lt -> M.Int.lt
        | Gt -> M.Int.gt
        | Le -> M.Int.le
        | Ge -> M.Int.ge
        | op ->
          Fmt.failwith {|Int: Unsupported relop operator "%a"|} Relop.pp op

      let cvtop op e =
        match op with
        | Cvtop.ToString -> M.Func.apply int2str [ e ]
        | OfString -> M.Func.apply str2int [ e ]
        | Reinterpret_float -> M.Real.to_int e
        | op ->
          Fmt.failwith {|Int: Unsupported cvtop operator "%a"|} Cvtop.pp op
    end

    module Real_impl = struct
      let v f = M.real f [@@inline]

      let unop op e =
        let open M in
        match op with
        | Unop.Neg -> Real.neg e
        | Abs -> ite (Real.gt e (real 0.)) e (Real.neg e)
        | Sqrt -> Real.pow e (v 0.5)
        | Ceil ->
          let x_int = M.Real.to_int e in
          ite (eq (Int.to_real x_int) e) x_int (Int.add x_int (int 1))
        | Floor -> Real.to_int e
        | Nearest | Is_nan | _ ->
          Fmt.failwith {|Real: Unsupported unop operator "%a"|} Unop.pp op

      let binop op e1 e2 =
        match op with
        | Binop.Add -> M.Real.add e1 e2
        | Sub -> M.Real.sub e1 e2
        | Mul -> M.Real.mul e1 e2
        | Div -> M.Real.div e1 e2
        | Pow -> M.Real.pow e1 e2
        | Min -> M.ite (M.Real.le e1 e2) e1 e2
        | Max -> M.ite (M.Real.ge e1 e2) e1 e2
        | _ ->
          Fmt.failwith {|Real: Unsupported binop operator "%a"|} Binop.pp op

      let relop op e1 e2 =
        match op with
        | Relop.Eq -> M.eq e1 e2
        | Ne -> M.distinct [ e1; e2 ]
        | Lt -> M.Real.lt e1 e2
        | Gt -> M.Real.gt e1 e2
        | Le -> M.Real.le e1 e2
        | Ge -> M.Real.ge e1 e2
        | _ ->
          Fmt.failwith {|Real: Unsupported relop operator "%a"|} Relop.pp op

      let cvtop op e =
        match op with
        | Cvtop.ToString -> M.Func.apply real2str [ e ]
        | OfString -> M.Func.apply str2real [ e ]
        | Reinterpret_int -> M.Int.to_real e
        | op ->
          Fmt.failwith {|Real: Unsupported cvtop operator "%a"|} Cvtop.pp op
    end

    module String_impl = struct
      let v s = M.String.v s [@@inline]

      let unop op e =
        match op with
        | Unop.Length -> M.String.length e
        | Trim -> M.Func.apply str_trim [ e ]
        | op ->
          Fmt.failwith {|String: Unsupported unop operator "%a"|} Unop.pp op

      let binop op e1 e2 =
        match op with
        | Binop.At -> M.String.at e1 ~pos:e2
        | String_contains -> M.String.contains e1 ~sub:e2
        | String_prefix -> M.String.is_prefix e1 ~prefix:e2
        | String_suffix -> M.String.is_suffix e1 ~suffix:e2
        | String_in_re -> M.String.in_re e1 e2
        | _ ->
          Fmt.failwith {|String: Unsupported binop operator "%a"|} Binop.pp op

      let triop op e1 e2 e3 =
        match op with
        | Triop.String_extract -> M.String.sub e1 ~pos:e2 ~len:e3
        | String_index -> M.String.index_of e1 ~sub:e2 ~pos:e3
        | String_replace -> M.String.replace e1 ~pattern:e2 ~with_:e3
        | _ ->
          Fmt.failwith {|String: Unsupported triop operator "%a"|} Triop.pp op

      let relop op e1 e2 =
        match op with
        | Relop.Eq -> M.eq e1 e2
        | Ne -> M.distinct [ e1; e2 ]
        | Lt -> M.String.lt e1 e2
        | Le -> M.String.le e1 e2
        | _ ->
          Fmt.failwith {|String: Unsupported relop operator "%a"|} Relop.pp op

      let cvtop = function
        | Cvtop.String_to_code -> M.String.to_code
        | String_from_code -> M.String.of_code
        | String_to_int -> M.String.to_int
        | String_from_int -> M.String.of_int
        | String_to_re -> M.String.to_re
        | op ->
          Fmt.failwith {|String: Unsupported cvtop operator "%a"|} Cvtop.pp op

      let naryop op es =
        match op with
        | Naryop.Concat -> M.String.concat es
        | _ ->
          Fmt.failwith {|String: Unsupported naryop operator "%a"|} Naryop.pp op
    end

    module Regexp_impl = struct
      let unop op e =
        match op with
        | Unop.Regexp_star -> M.Re.star e
        | Regexp_plus -> M.Re.plus e
        | Regexp_opt -> M.Re.opt e
        | Regexp_comp -> M.Re.comp e
        | Regexp_loop (i1, i2) -> M.Re.loop e i1 i2
        | op ->
          Fmt.failwith {|Regexp: Unsupported unop operator "%a"|} Unop.pp op

      let binop op e1 e2 =
        match op with
        | Binop.Regexp_range -> M.Re.range e1 e2
        | op ->
          Fmt.failwith {|Regexp: Unsupported binop operator "%a"|} Binop.pp op

      let _triop _ = function
        | op ->
          Fmt.failwith {|Regexp: Unsupported triop operator "%a"|} Triop.pp op

      let _relop _ _ = function
        | op ->
          Fmt.failwith {|Regexp: Unsupported relop operator "%a"|} Relop.pp op

      let _cvtop = function
        | op ->
          Fmt.failwith {|Regexp: Unsupported cvtop operator "%a"|} Cvtop.pp op

      let naryop op es =
        match op with
        | Naryop.Concat -> M.Re.concat es
        | Regexp_union -> M.Re.union es
        | op ->
          Fmt.failwith {|Regexp: Unsupported naryop operator "%a"|} Naryop.pp op
    end

    module type Bitv_sig = sig
      type elt

      val v : elt -> M.term

      val bitwidth : int

      val to_ieee_bv : M.term -> M.term

      module Ixx : sig
        val of_int : int -> elt

        val shift_left : elt -> int -> elt
      end
    end

    module Bitv_impl (B : Bitv_sig) = struct
      open M
      include B

      (* Stolen from @krtab in OCamlPro/owi#195 *)
      let clz n =
        let rec loop (lb : int) (ub : int) =
          if ub = lb + 1 then v @@ Ixx.of_int (bitwidth - ub)
          else
            let mid = (lb + ub) / 2 in
            let pow_two_mid = v Ixx.(shift_left (of_int 1) mid) in
            ite (Bitv.lt_u n pow_two_mid) (loop lb mid) (loop mid ub)
        in
        ite
          (eq n (v @@ Ixx.of_int 0))
          (v @@ Ixx.of_int bitwidth)
          (loop 0 bitwidth)

      (* Stolen from @krtab in OCamlPro/owi #195 *)
      let ctz n =
        let zero = v (Ixx.of_int 0) in
        let rec loop (lb : int) (ub : int) =
          if ub = lb + 1 then v (Ixx.of_int lb)
          else
            let mid = (lb + ub) / 2 in
            let pow_two_mid = v Ixx.(shift_left (of_int 1) mid) in
            M.ite (eq (Bitv.rem n pow_two_mid) zero) (loop mid ub) (loop lb mid)
        in
        ite (eq n zero) (v @@ Ixx.of_int bitwidth) (loop 0 bitwidth)

      let popcnt n =
        let rec loop (next : int) count =
          if Prelude.Int.equal next bitwidth then count
          else
            (* We shift the original number so that the current bit to test is on the right. *)
            let shifted = Bitv.lshr n (v @@ Ixx.of_int next) in
            (* We compute the remainder of the shifted number *)
            let remainder = Bitv.rem_u shifted (v @@ Ixx.of_int 2) in
            (* The remainder is either 0 or 1, we add it directly to the count *)
            let count = Bitv.add count remainder in
            let next = succ next in
            loop next count
        in
        loop 0 (v @@ Ixx.of_int 0)

      let unop = function
        | Unop.Clz -> clz
        | Ctz -> ctz
        | Popcnt -> popcnt
        | Neg -> Bitv.neg
        | Not -> Bitv.lognot
        | op ->
          Fmt.failwith {|Bitv: Unsupported unary operator "%a"|} Unop.pp op

      let binop = function
        | Binop.Add -> Bitv.add
        | Sub -> Bitv.sub
        | Mul -> Bitv.mul
        | Div -> Bitv.div
        | DivU -> Bitv.div_u
        | And -> Bitv.logand
        | Xor -> Bitv.logxor
        | Or -> Bitv.logor
        | Shl -> Bitv.shl
        | ShrA -> Bitv.ashr
        | ShrL -> Bitv.lshr
        | Rem -> Bitv.rem
        | RemU -> Bitv.rem_u
        | Rotl -> Bitv.rotate_left
        | Rotr -> Bitv.rotate_right
        | op ->
          Fmt.failwith {|Bitv: Unsupported binary operator "%a"|} Binop.pp op

      let triop op _ =
        Fmt.failwith {|Bitv: Unsupported triop operator "%a"|} Triop.pp op

      let relop op e1 e2 =
        match op with
        | Relop.Eq | Ne -> assert false
        | Lt -> Bitv.lt e1 e2
        | LtU -> Bitv.lt_u e1 e2
        | Le -> Bitv.le e1 e2
        | LeU -> Bitv.le_u e1 e2
        | Gt -> Bitv.gt e1 e2
        | GtU -> Bitv.gt_u e1 e2
        | Ge -> Bitv.ge e1 e2
        | GeU -> Bitv.ge_u e1 e2

      let cvtop op e =
        match op with
        | Cvtop.WrapI64 -> Bitv.extract e ~high:(bitwidth - 1) ~low:0
        | Sign_extend n -> Bitv.sign_extend n e
        | Zero_extend n -> Bitv.zero_extend n e
        | TruncSF32 | TruncSF64 ->
          Float.to_sbv bitwidth ~rm:Float.Rounding_mode.rtz e
        | TruncUF32 | TruncUF64 ->
          Float.to_ubv bitwidth ~rm:Float.Rounding_mode.rtz e
        | Reinterpret_float -> to_ieee_bv e
        | ToBool -> M.distinct [ e; v @@ Ixx.of_int 0 ]
        | OfBool -> ite e (v @@ Ixx.of_int 1) (v @@ Ixx.of_int 0)
        | _ -> assert false
    end

    module I8 = Bitv_impl (struct
      type elt = int

      let v i = M.Bitv.v (string_of_int i) 8

      let bitwidth = 8

      let to_ieee_bv _ = assert false

      module Ixx = struct
        let of_int i = i [@@inline]

        let shift_left v i = v lsl i [@@inline]
      end
    end)

    let to_ieee_bv f e =
      if M.Internals.has_to_ieee_bv then M.Float.to_ieee_bv e
      else M.Func.apply f [ e ]

    module I32 = Bitv_impl (struct
      type elt = int32

      let v i = M.Bitv.v (Int32.to_string i) 32

      let bitwidth = 32

      let to_ieee_bv = to_ieee_bv f32_to_i32

      module Ixx = Int32
    end)

    module I64 = Bitv_impl (struct
      type elt = int64

      let v i = M.Bitv.v (Int64.to_string i) 64

      let to_ieee_bv = to_ieee_bv f64_to_i64

      let bitwidth = 64

      module Ixx = Int64
    end)

    module type Float_sig = sig
      type elt

      val eb : int

      val sb : int

      val zero : unit -> M.term

      val v : elt -> M.term
      (* TODO: *)
      (* val to_string : Z3.FuncDecl.func_decl *)
      (* val of_string : Z3.FuncDecl.func_decl *)
    end

    module Float_impl (F : Float_sig) = struct
      open M
      include F

      let unop op e =
        match op with
        | Unop.Neg -> Float.neg e
        | Abs -> Float.abs e
        | Sqrt -> Float.sqrt ~rm:Float.Rounding_mode.rne e
        | Is_nan -> Float.is_nan e
        | Ceil -> Float.round_to_integral ~rm:Float.Rounding_mode.rtp e
        | Floor -> Float.round_to_integral ~rm:Float.Rounding_mode.rtn e
        | Trunc -> Float.round_to_integral ~rm:Float.Rounding_mode.rtz e
        | Nearest -> Float.round_to_integral ~rm:Float.Rounding_mode.rne e
        | _ -> Fmt.failwith {|Fp: Unsupported unary operator "%a"|} Unop.pp op

      let binop op e1 e2 =
        match op with
        | Binop.Add -> Float.add ~rm:Float.Rounding_mode.rne e1 e2
        | Sub -> Float.sub ~rm:Float.Rounding_mode.rne e1 e2
        | Mul -> Float.mul ~rm:Float.Rounding_mode.rne e1 e2
        | Div -> Float.div ~rm:Float.Rounding_mode.rne e1 e2
        | Min -> Float.min e1 e2
        | Max -> Float.max e1 e2
        | Rem -> Float.rem e1 e2
        | Copysign ->
          let abs_float = Float.abs e1 in
          M.ite (Float.ge e2 (F.zero ())) abs_float (Float.neg abs_float)
        | _ -> Fmt.failwith {|Fp: Unsupported binop operator "%a"|} Binop.pp op

      let triop op _ =
        Fmt.failwith {|Fp: Unsupported triop operator "%a"|} Triop.pp op

      let relop op e1 e2 =
        match op with
        | Relop.Eq -> Float.eq e1 e2
        | Ne -> not_ @@ Float.eq e1 e2
        | Lt -> Float.lt e1 e2
        | Le -> Float.le e1 e2
        | Gt -> Float.gt e1 e2
        | Ge -> Float.ge e1 e2
        | _ -> Fmt.failwith {|Fp: Unsupported relop operator "%a"|} Relop.pp op

      let cvtop op e =
        match op with
        | Cvtop.PromoteF32 | DemoteF64 ->
          Float.to_fp eb sb ~rm:Float.Rounding_mode.rne e
        | ConvertSI32 | ConvertSI64 ->
          Float.sbv_to_fp eb sb ~rm:Float.Rounding_mode.rne e
        | ConvertUI32 | ConvertUI64 ->
          Float.ubv_to_fp eb sb ~rm:Float.Rounding_mode.rne e
        | Reinterpret_int -> Float.of_ieee_bv eb sb e
        | ToString ->
          (* TODO: FuncDecl.apply to_string [ e ] *)
          assert false
        | OfString ->
          (* TODO: FuncDecl.apply of_string [ e ] *)
          assert false
        | _ -> Fmt.failwith {|Fp: Unsupported cvtop operator "%a"|} Cvtop.pp op
    end

    module Float32_impl = Float_impl (struct
      type elt = int32

      let eb = 8

      let sb = 24

      let v f = M.Float.v (Int32.float_of_bits f) eb sb

      let zero () = v (Int32.bits_of_float 0.0)

      (* TODO: *)
      (* let to_string = *)
      (*   Z3.FuncDecl.mk_func_decl_s ctx "F32ToString" [ fp32_sort ] str_sort *)
      (* let of_string = *)
      (*   Z3.FuncDecl.mk_func_decl_s ctx "StringToF32" [ str_sort ] fp32_sort *)
    end)

    module Float64_impl = Float_impl (struct
      type elt = int64

      let eb = 11

      let sb = 53

      let v f = M.Float.v (Int64.float_of_bits f) eb sb

      let zero () = v (Int64.bits_of_float 0.0)

      (* TODO: *)
      (* let to_string = *)
      (*   Z3.FuncDecl.mk_func_decl_s ctx "F64ToString" [ fp64_sort ] str_sort *)
      (* let of_string = *)
      (*   Z3.FuncDecl.mk_func_decl_s ctx "StringToF64" [ str_sort ] fp64_sort *)
    end)

    let v : Value.t -> M.term = function
      | True -> Bool_impl.true_
      | False -> Bool_impl.false_
      | Int v -> Int_impl.v v
      | Real v -> Real_impl.v v
      | Str v -> String_impl.v v
      | Num (F32 x) -> Float32_impl.v x
      | Num (F64 x) -> Float64_impl.v x
      | Bitv bv -> M.Bitv.v (Bitvector.to_string bv) (Bitvector.numbits bv)
      | List _ | App _ | Unit | Nothing -> assert false

    let unop = function
      | Ty.Ty_int -> Int_impl.unop
      | Ty.Ty_real -> Real_impl.unop
      | Ty.Ty_bool -> Bool_impl.unop
      | Ty.Ty_str -> String_impl.unop
      | Ty.Ty_regexp -> Regexp_impl.unop
      | Ty.Ty_bitv 8 -> I8.unop
      | Ty.Ty_bitv 32 -> I32.unop
      | Ty.Ty_bitv 64 -> I64.unop
      | Ty.Ty_fp 32 -> Float32_impl.unop
      | Ty.Ty_fp 64 -> Float64_impl.unop
      | Ty.Ty_bitv _ | Ty_fp _ | Ty_list | Ty_app | Ty_unit | Ty_none ->
        assert false

    let binop = function
      | Ty.Ty_int -> Int_impl.binop
      | Ty.Ty_real -> Real_impl.binop
      | Ty.Ty_bool -> Bool_impl.binop
      | Ty.Ty_str -> String_impl.binop
      | Ty.Ty_regexp -> Regexp_impl.binop
      | Ty.Ty_bitv 8 -> I8.binop
      | Ty.Ty_bitv 32 -> I32.binop
      | Ty.Ty_bitv 64 -> I64.binop
      | Ty.Ty_fp 32 -> Float32_impl.binop
      | Ty.Ty_fp 64 -> Float64_impl.binop
      | Ty.Ty_bitv _ | Ty_fp _ | Ty_list | Ty_app | Ty_unit | Ty_none ->
        assert false

    let triop = function
      | Ty.Ty_int | Ty.Ty_real -> assert false
      | Ty.Ty_bool -> Bool_impl.triop
      | Ty.Ty_str -> String_impl.triop
      | Ty.Ty_bitv 8 -> I8.triop
      | Ty.Ty_bitv 32 -> I32.triop
      | Ty.Ty_bitv 64 -> I64.triop
      | Ty.Ty_fp 32 -> Float32_impl.triop
      | Ty.Ty_fp 64 -> Float64_impl.triop
      | Ty.Ty_bitv _ | Ty_fp _ | Ty_list | Ty_app | Ty_unit | Ty_none
      | Ty_regexp ->
        assert false

    let relop = function
      | Ty.Ty_int -> Int_impl.relop
      | Ty.Ty_real -> Real_impl.relop
      | Ty.Ty_bool -> Bool_impl.relop
      | Ty.Ty_str -> String_impl.relop
      | Ty.Ty_bitv 8 -> I8.relop
      | Ty.Ty_bitv 32 -> I32.relop
      | Ty.Ty_bitv 64 -> I64.relop
      | Ty.Ty_fp 32 -> Float32_impl.relop
      | Ty.Ty_fp 64 -> Float64_impl.relop
      | Ty.Ty_bitv _ | Ty_fp _ | Ty_list | Ty_app | Ty_unit | Ty_none
      | Ty_regexp ->
        assert false

    let cvtop = function
      | Ty.Ty_int -> Int_impl.cvtop
      | Ty.Ty_real -> Real_impl.cvtop
      | Ty.Ty_bool -> Bool_impl.cvtop
      | Ty.Ty_str -> String_impl.cvtop
      | Ty.Ty_bitv 8 -> I8.cvtop
      | Ty.Ty_bitv 32 -> I32.cvtop
      | Ty.Ty_bitv 64 -> I64.cvtop
      | Ty.Ty_fp 32 -> Float32_impl.cvtop
      | Ty.Ty_fp 64 -> Float64_impl.cvtop
      | Ty.Ty_bitv _ | Ty_fp _ | Ty_list | Ty_app | Ty_unit | Ty_none
      | Ty_regexp ->
        assert false

    let naryop = function
      | Ty.Ty_str -> String_impl.naryop
      | Ty.Ty_bool -> Bool_impl.naryop
      | Ty.Ty_regexp -> Regexp_impl.naryop
      | ty -> Fmt.failwith "Naryop for type \"%a\" not implemented" Ty.pp ty

    let rec encode_expr ctx (hte : Expr.t) : symbol_ctx * M.term =
      match Expr.view hte with
      | Val value -> (ctx, v value)
      | Ptr { base; offset } ->
        let base' = v (Bitv (Bitvector.of_int32 base)) in
        let ctx, offset' = encode_expr ctx offset in
        (ctx, I32.binop Add base' offset')
      | Symbol sym -> make_symbol ctx sym
      | Unop (ty, op, e) ->
        let ctx, e = encode_expr ctx e in
        (ctx, unop ty op e)
      | Binop (ty, op, e1, e2) ->
        let ctx, e1 = encode_expr ctx e1 in
        let ctx, e2 = encode_expr ctx e2 in
        (ctx, binop ty op e1 e2)
      | Triop (ty, op, e1, e2, e3) ->
        let ctx, e1 = encode_expr ctx e1 in
        let ctx, e2 = encode_expr ctx e2 in
        let ctx, e3 = encode_expr ctx e3 in
        (ctx, triop ty op e1 e2 e3)
      | Relop (ty, op, e1, e2) ->
        let ctx, e1 = encode_expr ctx e1 in
        let ctx, e2 = encode_expr ctx e2 in
        (ctx, relop ty op e1 e2)
      | Cvtop (ty, op, e) ->
        let ctx, e = encode_expr ctx e in
        (ctx, cvtop ty op e)
      | Naryop (ty, op, es) ->
        let ctx, es =
          List.fold_left
            (fun (ctx, es) e ->
              let ctx, e = encode_expr ctx e in
              (ctx, e :: es) )
            (ctx, []) es
        in
        (* This is needed so arguments don't end up out of order in the operator *)
        let es = List.rev es in
        (ctx, naryop ty op es)
      | Extract (e, h, l) ->
        let ctx, e = encode_expr ctx e in
        (ctx, M.Bitv.extract e ~high:((h * 8) - 1) ~low:(l * 8))
      | Concat (e1, e2) ->
        let ctx, e1 = encode_expr ctx e1 in
        let ctx, e2 = encode_expr ctx e2 in
        (ctx, M.Bitv.concat e1 e2)
      | Binder (Forall, vars, body) ->
        let ctx, vars = encode_exprs ctx vars in
        let ctx, body = encode_expr ctx body in
        (ctx, M.forall vars body)
      | Binder (Exists, vars, body) ->
        let ctx, vars = encode_exprs ctx vars in
        let ctx, body = encode_expr ctx body in
        (ctx, M.exists vars body)
      | List _ | App _ | Binder _ ->
        Fmt.failwith "Cannot encode expression: %a" Expr.pp hte

    and encode_exprs ctx (es : Expr.t list) : symbol_ctx * M.term list =
      List.fold_left
        (fun (ctx, es) e ->
          let ctx, e = encode_expr ctx e in
          (ctx, e :: es) )
        (ctx, []) es

    let value_of_term model ty term =
      let v =
        match M.Model.eval ~completion:true model term with
        | None -> assert false
        | Some v -> v
      in
      match ty with
      | Ty_int -> Value.Int (M.Interp.to_int v)
      | Ty_real -> Value.Real (M.Interp.to_real v)
      | Ty_bool -> if M.Interp.to_bool v then Value.True else Value.False
      | Ty_str ->
        let str = M.Interp.to_string v in
        Value.Str str
      | Ty_bitv 1 ->
        let b = M.Interp.to_bitv v 1 in
        if Int64.equal b 1L then Value.True
        else (
          assert (Int64.equal b 0L);
          Value.False )
      | Ty_bitv 8 ->
        let i8 = M.Interp.to_bitv v 8 in
        Value.Bitv (Bitvector.of_int8 (Int64.to_int i8))
      | Ty_bitv 32 ->
        let i32 = M.Interp.to_bitv v 32 in
        Value.Bitv (Bitvector.of_int32 (Int64.to_int32 i32))
      | Ty_bitv 64 ->
        let i64 = M.Interp.to_bitv v 64 in
        Value.Bitv (Bitvector.of_int64 i64)
      | Ty_fp 32 ->
        let float = M.Interp.to_float v 8 24 in
        Value.Num (F32 (Int32.bits_of_float float))
      | Ty_fp 64 ->
        let float = M.Interp.to_float v 11 53 in
        Value.Num (F64 (Int64.bits_of_float float))
      | Ty_bitv _ | Ty_fp _ | Ty_list | Ty_app | Ty_unit | Ty_none | Ty_regexp
        ->
        assert false

    let value ({ model = m; ctx } : model) (c : Expr.t) : Value.t =
      let _, e = encode_expr ctx c in
      value_of_term m (Expr.ty c) e

    let values_of_model ?symbols ({ model; ctx } as model0) =
      let m = Hashtbl.create 512 in
      ( match symbols with
      | Some symbols ->
        List.iter
          (fun sym ->
            let v = value model0 (Expr.symbol sym) in
            Hashtbl.replace m sym v )
          symbols
      | None ->
        Smap.iter
          (fun (sym : Symbol.t) term ->
            let v = value_of_term model sym.ty term in
            Hashtbl.replace m sym v )
          ctx );
      m

    let set_debug _ = ()

    module Smtlib = struct
      let pp ?name ?logic ?status fmt htes =
        (* FIXME: I don't know if encoding with the empty map is ok :\ *)
        let _, terms = encode_exprs Smap.empty htes in
        M.Smtlib.pp ?name ?logic ?status fmt terms
    end

    module Solver = struct
      let make ?params ?logic () =
        let ctx = Stack.create () in
        Stack.push Smap.empty ctx;
        { solver = M.Solver.make ?params ?logic (); ctx; last_ctx = None }

      let clone { solver; ctx; last_ctx } =
        { solver = M.Solver.clone solver; ctx = Stack.copy ctx; last_ctx }

      let push { solver; ctx; _ } =
        match Stack.top_opt ctx with
        | None -> assert false
        | Some top ->
          Stack.push top ctx;
          M.Solver.push solver

      let pop { solver; ctx; _ } n =
        match Stack.pop_opt ctx with
        | None -> assert false
        | Some _ -> M.Solver.pop solver n

      let reset (s : solver) =
        Stack.clear s.ctx;
        Stack.push Smap.empty s.ctx;
        s.last_ctx <- None;
        M.Solver.reset s.solver

      let add (s : solver) (exprs : Expr.t list) =
        match Stack.pop_opt s.ctx with
        | None -> assert false
        | Some ctx ->
          let ctx, exprs = encode_exprs ctx exprs in
          Stack.push ctx s.ctx;
          M.Solver.add s.solver exprs

      let check (s : solver) ~assumptions =
        match Stack.top_opt s.ctx with
        | None -> assert false
        | Some ctx ->
          let ctx, assumptions = encode_exprs ctx assumptions in
          s.last_ctx <- Some ctx;
          M.Solver.check s.solver ~assumptions

      let model { solver; last_ctx; _ } =
        match last_ctx with
        | Some ctx ->
          M.Solver.model solver |> Option.map (fun m -> { model = m; ctx })
        | None ->
          Fmt.failwith "model: Trying to fetch model berfore check-sat call"

      let add_simplifier s =
        { s with solver = M.Solver.add_simplifier s.solver }

      let interrupt _ = M.Solver.interrupt ()

      let get_statistics { solver; _ } = M.Solver.get_statistics solver
    end

    module Optimizer = struct
      let make () =
        let ctx = Stack.create () in
        Stack.push Smap.empty ctx;
        { opt = M.Optimizer.make (); ctx }

      let push { opt; _ } = M.Optimizer.push opt

      let pop { opt; _ } = M.Optimizer.pop opt

      let add (o : optimize) exprs =
        match Stack.pop_opt o.ctx with
        | None -> assert false
        | Some ctx ->
          let ctx, exprs = encode_exprs ctx exprs in
          Stack.push ctx o.ctx;
          M.Optimizer.add o.opt exprs

      let check { opt; _ } = M.Optimizer.check opt

      let model { opt; ctx } =
        match Stack.top_opt ctx with
        | None -> assert false
        | Some ctx ->
          M.Optimizer.model opt |> Option.map (fun m -> { model = m; ctx })

      let maximize (o : optimize) (expr : Expr.t) =
        match Stack.pop_opt o.ctx with
        | None -> assert false
        | Some ctx ->
          let ctx, expr = encode_expr ctx expr in
          Stack.push ctx o.ctx;
          M.Optimizer.maximize o.opt expr

      let minimize (o : optimize) (expr : Expr.t) =
        match Stack.pop_opt o.ctx with
        | None -> assert false
        | Some ctx ->
          let ctx, expr = encode_expr ctx expr in
          Stack.push ctx o.ctx;
          M.Optimizer.minimize o.opt expr

      let interrupt _ = M.Optimizer.interrupt ()

      let get_statistics { opt; _ } = M.Optimizer.get_statistics opt
    end
  end

  module Fresh = struct
    module Make () = Make_ (M_with_make.Make ())
  end

  let is_available = M_with_make.is_available

  include Make_ (M_with_make)
end
