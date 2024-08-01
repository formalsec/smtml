(***************************************************************************)
(* This file is part of the third-party OCaml library `smtml`.             *)
(* Copyright (C) 2023-2024 formalsec                                       *)
(*                                                                         *)
(* This program is free software: you can redistribute it and/or modify    *)
(* it under the terms of the GNU General Public License as published by    *)
(* the Free Software Foundation, either version 3 of the License, or       *)
(* (at your option) any later version.                                     *)
(*                                                                         *)
(* This program is distributed in the hope that it will be useful,         *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of          *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           *)
(* GNU General Public License for more details.                            *)
(*                                                                         *)
(* You should have received a copy of the GNU General Public License       *)
(* along with this program.  If not, see <https://www.gnu.org/licenses/>.  *)
(***************************************************************************)

include Mappings_intf

module Make (M_with_make : M_with_make) : S_with_fresh = struct
  module Make_ (M : M) : S = struct
    open Ty

    type sym_tbl = (Symbol.t, M.term) Hashtbl.t

    type model =
      { model : M.model
      ; symbol_table : sym_tbl
      }

    type solver =
      { solver : M.solver
      ; symbol_table : sym_tbl
      }

    type handle = M.handle

    type optimize =
      { opt : M.optimizer
      ; symbol_table : sym_tbl
      }

    let err = Log.err

    let i8 = M.Types.bitv 8

    let i32 = M.Types.bitv 32

    let i64 = M.Types.bitv 64

    let f32 = M.Types.float 8 24

    let f64 = M.Types.float 11 53

    let get_type = function
      | Ty_int -> M.Types.int
      | Ty_real -> M.Types.real
      | Ty_bool -> M.Types.bool
      | Ty_str -> M.Types.string
      | Ty_bitv 8 -> i8
      | Ty_bitv 32 -> i32
      | Ty_bitv 64 -> i64
      | Ty_fp 32 -> f32
      | Ty_fp 64 -> f64
      | Ty_bitv _ | Ty_fp _ | Ty_list | Ty_app | Ty_unit -> assert false

    let make_symbol (symbol_table : sym_tbl) (s : Symbol.t) : M.term =
      match Hashtbl.find_opt symbol_table s with
      | Some sym -> sym
      | None ->
        let sym = M.const s.name (get_type s.ty) in
        Hashtbl.replace symbol_table s sym;
        sym

    module Bool_impl = struct
      let true_ = M.true_

      let false_ = M.false_

      let unop = function
        | Not -> M.not_
        | op -> err {|Bool: Unsupported Z3 unop operator "%a"|} Ty.pp_unop op

      let binop = function
        | And -> M.and_
        | Or -> M.or_
        | Xor -> M.xor
        | op -> err {|Bool: Unsupported Z3 binop operator "%a"|} Ty.pp_binop op

      let triop = function
        | Ite -> M.ite
        | op -> err {|Bool: Unsupported Z3 triop operator "%a"|} Ty.pp_triop op

      let relop op e1 e2 =
        match op with
        | Eq -> M.eq e1 e2
        | Ne -> M.distinct [ e1; e2 ]
        | _ -> err {|Bool: Unsupported Z3 relop operator "%a"|} Ty.pp_relop op

      let cvtop _op _e = assert false
    end

    module Int_impl = struct
      let v i = M.int i [@@inline]

      let unop = function
        | Neg -> M.Int.neg
        | op -> err {|Int: Unsupported unop operator "%a"|} Ty.pp_unop op

      let binop = function
        | Add -> M.Int.add
        | Sub -> M.Int.sub
        | Mul -> M.Int.mul
        | Div -> M.Int.div
        | Rem -> M.Int.rem
        | Pow -> M.Int.pow
        | op -> err {|Int: Unsupported binop operator "%a"|} Ty.pp_binop op

      let relop = function
        | Eq | Ne -> assert false
        | Lt -> M.Int.lt
        | Gt -> M.Int.gt
        | Le -> M.Int.le
        | Ge -> M.Int.ge
        | op -> err {|Int: Unsupported relop operator "%a"|} Ty.pp_relop op

      (* TODO: Uninterpreted cvtops *)
      let cvtop op e =
        match op with
        | ToString -> assert false
        | OfString -> assert false
        | Reinterpret_float -> M.Real.to_int e
        | op -> err {|Int: Unsupported cvtop operator "%a"|} Ty.pp_cvtop op
    end

    module Real_impl = struct
      let v f = M.real f [@@inline]

      let unop op e =
        let open M in
        match op with
        | Neg -> Real.neg e
        | Abs -> ite (Real.gt e (real 0.)) e (Real.neg e)
        | Sqrt -> Real.pow e (v 0.5)
        | Ceil ->
          let x_int = M.Real.to_int e in
          ite (eq (Int.to_real x_int) e) x_int (Int.add x_int (int 1))
        | Floor -> Real.to_int e
        | Nearest | Is_nan | _ ->
          err {|Real: Unsupported unop operator "%a"|} Ty.pp_unop op

      let binop op e1 e2 =
        match op with
        | Add -> M.Real.add e1 e2
        | Sub -> M.Real.sub e1 e2
        | Mul -> M.Real.mul e1 e2
        | Div -> M.Real.div e1 e2
        | Pow -> M.Real.pow e1 e2
        | Min -> M.ite (M.Real.le e1 e2) e1 e2
        | Max -> M.ite (M.Real.ge e1 e2) e1 e2
        | _ -> err {|Real: Unsupported binop operator "%a"|} Ty.pp_binop op

      let relop op e1 e2 =
        match op with
        | Eq -> M.eq e1 e2
        | Ne -> M.distinct [ e1; e2 ]
        | Lt -> M.Real.lt e1 e2
        | Gt -> M.Real.gt e1 e2
        | Le -> M.Real.le e1 e2
        | Ge -> M.Real.ge e1 e2
        | _ -> err {|Real: Unsupported relop operator "%a"|} Ty.pp_relop op

      (* TODO: Uninterpreted cvtops *)
      let cvtop op e =
        match op with
        | ToString -> assert false
        | OfString -> assert false
        | ConvertUI32 -> assert false
        | Reinterpret_int -> M.Int.to_real e
        | op -> err {|Real: Unsupported cvtop operator "%a"|} Ty.pp_cvtop op
    end

    module String_impl = struct
      let v s = M.String.v s [@@inline]

      (* let trim = FuncDecl.mk_func_decl_s ctx "Trim" [ str_sort ] str_sort *)

      let unop = function
        | Length -> M.String.length
        | Trim ->
          (* FuncDecl.apply trim [ e ] *)
          assert false
        | op -> err {|String: Unsupported unop operator "%a"|} Ty.pp_unop op

      let binop op e1 e2 =
        match op with
        | At -> M.String.at e1 ~pos:e2
        | String_contains -> M.String.contains e1 ~sub:e2
        | String_prefix -> M.String.is_prefix e1 ~prefix:e2
        | String_suffix -> M.String.is_suffix e1 ~suffix:e2
        | _ -> err {|String: Unsupported binop operator "%a"|} Ty.pp_binop op

      let triop op e1 e2 e3 =
        match op with
        | String_extract -> M.String.sub e1 ~pos:e2 ~len:e3
        | String_index -> M.String.index_of e1 ~sub:e2 ~pos:e3
        | String_replace -> M.String.replace e1 ~pattern:e2 ~with_:e3
        | _ -> err {|String: Unsupported triop operator "%a"|} Ty.pp_triop op

      let relop op e1 e2 =
        match op with
        | Eq -> M.eq e1 e2
        | Ne -> M.distinct [ e1; e2 ]
        | _ -> err {|String: Unsupported relop operator "%a"|} Ty.pp_relop op

      let cvtop = function
        | String_to_code -> M.String.to_code
        | String_from_code -> M.String.of_code
        | String_to_int -> M.String.to_int
        | String_from_int -> M.String.of_int
        | op -> err {|String: Unsupported cvtop operator "%a"|} Ty.pp_cvtop op

      let naryop op es =
        match op with
        | Concat -> M.String.concat es
        | _ -> err {|String: Unsupported naryop operator "%a"|} Ty.pp_naryop op
    end

    module type Bitv_sig = sig
      type elt

      val v : elt -> M.term

      val bitwidth : int

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

      let unop = function
        | Clz -> clz
        | Ctz -> ctz
        | Neg -> Bitv.neg
        | Not -> Bitv.lognot
        | op -> err {|Bitv: Unsupported unary operator "%a"|} Ty.pp_unop op

      let binop = function
        | Add -> Bitv.add
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
        | op -> err {|Bitv: Unsupported binary operator "%a"|} Ty.pp_binop op

      let triop op _ =
        err {|Bitv: Unsupported triop operator "%a"|} Ty.pp_triop op

      let relop op e1 e2 =
        match op with
        | Eq | Ne -> assert false
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
        | WrapI64 -> Bitv.extract e ~high:(bitwidth - 1) ~low:0
        | Sign_extend n -> Bitv.sign_extend n e
        | Zero_extend n -> Bitv.zero_extend n e
        | TruncSF32 | TruncSF64 ->
          Float.to_sbv bitwidth ~rm:Float.Rounding_mode.rtz e
        | TruncUF32 | TruncUF64 ->
          Float.to_ubv bitwidth ~rm:Float.Rounding_mode.rtz e
        | Reinterpret_float -> Float.to_ieee_bv e
        | ToBool -> M.distinct [ e; v @@ Ixx.of_int 0 ]
        | OfBool -> ite e (v @@ Ixx.of_int 1) (v @@ Ixx.of_int 0)
        | _ -> assert false
    end

    module I8 = Bitv_impl (struct
      type elt = int

      let v i = M.Bitv.v (string_of_int i) 8

      let bitwidth = 8

      module Ixx = struct
        let of_int i = i [@@inline]

        let shift_left v i = v lsl i [@@inline]
      end
    end)

    module I32 = Bitv_impl (struct
      type elt = int32

      let v i = M.Bitv.v (Int32.to_string i) 32

      let bitwidth = 32

      module Ixx = Int32
    end)

    module I64 = Bitv_impl (struct
      type elt = int64

      let v i = M.Bitv.v (Int64.to_string i) 64

      let bitwidth = 64

      module Ixx = Int64
    end)

    module type Float_sig = sig
      type elt

      val eb : int

      val sb : int

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
        | Neg -> Float.neg e
        | Abs -> Float.abs e
        | Sqrt -> Float.sqrt ~rm:Float.Rounding_mode.rne e
        | Is_nan -> Float.is_nan e
        | Ceil -> Float.round_to_integral ~rm:Float.Rounding_mode.rtp e
        | Floor -> Float.round_to_integral ~rm:Float.Rounding_mode.rtn e
        | Trunc -> Float.round_to_integral ~rm:Float.Rounding_mode.rtz e
        | Nearest -> Float.round_to_integral ~rm:Float.Rounding_mode.rne e
        | _ -> err {|Fp: Unsupported Z3 unary operator "%a"|} Ty.pp_unop op

      let binop op e1 e2 =
        match op with
        | Add -> Float.add ~rm:Float.Rounding_mode.rne e1 e2
        | Sub -> Float.sub ~rm:Float.Rounding_mode.rne e1 e2
        | Mul -> Float.mul ~rm:Float.Rounding_mode.rne e1 e2
        | Div -> Float.div ~rm:Float.Rounding_mode.rne e1 e2
        | Min -> Float.min e1 e2
        | Max -> Float.max e1 e2
        | Rem -> Float.rem e1 e2
        | _ -> err {|Fp: Unsupported Z3 binop operator "%a"|} Ty.pp_binop op

      let triop op _ =
        err {|Fp: Unsupported Z3 triop operator "%a"|} Ty.pp_triop op

      let relop op e1 e2 =
        match op with
        | Eq -> Float.eq e1 e2
        | Ne -> not_ @@ Float.eq e1 e2
        | Lt -> Float.lt e1 e2
        | Le -> Float.le e1 e2
        | Gt -> Float.gt e1 e2
        | Ge -> Float.ge e1 e2
        | _ -> err {|Fp: Unsupported Z3 relop operator "%a"|} Ty.pp_relop op

      let cvtop op e =
        match op with
        | PromoteF32 | DemoteF64 ->
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
        | _ -> err {|Fp: Unsupported Z3 cvtop operator "%a"|} Ty.pp_cvtop op
    end

    module Float32_impl = Float_impl (struct
      type elt = int32

      let eb = 8

      let sb = 24

      let v f = M.Float.v (Int32.float_of_bits f) eb sb

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
      | Num (I8 x) -> I8.v x
      | Num (I32 x) -> I32.v x
      | Num (I64 x) -> I64.v x
      | Num (F32 x) -> Float32_impl.v x
      | Num (F64 x) -> Float64_impl.v x
      | List _ | App _ | Unit -> assert false

    let unop = function
      | Ty.Ty_int -> Int_impl.unop
      | Ty.Ty_real -> Real_impl.unop
      | Ty.Ty_bool -> Bool_impl.unop
      | Ty.Ty_str -> String_impl.unop
      | Ty.Ty_bitv 8 -> I8.unop
      | Ty.Ty_bitv 32 -> I32.unop
      | Ty.Ty_bitv 64 -> I64.unop
      | Ty.Ty_fp 32 -> Float32_impl.unop
      | Ty.Ty_fp 64 -> Float64_impl.unop
      | Ty.Ty_bitv _ | Ty_fp _ | Ty_list | Ty_app | Ty_unit -> assert false

    let binop = function
      | Ty.Ty_int -> Int_impl.binop
      | Ty.Ty_real -> Real_impl.binop
      | Ty.Ty_bool -> Bool_impl.binop
      | Ty.Ty_str -> String_impl.binop
      | Ty.Ty_bitv 8 -> I8.binop
      | Ty.Ty_bitv 32 -> I32.binop
      | Ty.Ty_bitv 64 -> I64.binop
      | Ty.Ty_fp 32 -> Float32_impl.binop
      | Ty.Ty_fp 64 -> Float64_impl.binop
      | Ty.Ty_bitv _ | Ty_fp _ | Ty_list | Ty_app | Ty_unit -> assert false

    let triop = function
      | Ty.Ty_int | Ty.Ty_real -> assert false
      | Ty.Ty_bool -> Bool_impl.triop
      | Ty.Ty_str -> String_impl.triop
      | Ty.Ty_bitv 8 -> I8.triop
      | Ty.Ty_bitv 32 -> I32.triop
      | Ty.Ty_bitv 64 -> I64.triop
      | Ty.Ty_fp 32 -> Float32_impl.triop
      | Ty.Ty_fp 64 -> Float64_impl.triop
      | Ty.Ty_bitv _ | Ty_fp _ | Ty_list | Ty_app | Ty_unit -> assert false

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
      | Ty.Ty_bitv _ | Ty_fp _ | Ty_list | Ty_app | Ty_unit -> assert false

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
      | Ty.Ty_bitv _ | Ty_fp _ | Ty_list | Ty_app | Ty_unit -> assert false

    let naryop = function
      | Ty.Ty_str -> String_impl.naryop
      | ty -> err "Naryop for type \"%a\" not implemented" Ty.pp ty

    let rec encode_expr symbol_table (hte : Expr.t) : M.term =
      match Expr.view hte with
      | Val value -> v value
      | Ptr { base; offset } ->
        let base' = v (Num (I32 base)) in
        let offset' = encode_expr symbol_table offset in
        I32.binop Add base' offset'
      | Symbol sym -> make_symbol symbol_table sym
      | Unop (ty, op, e) ->
        let e = encode_expr symbol_table e in
        unop ty op e
      | Binop (ty, op, e1, e2) ->
        let e1 = encode_expr symbol_table e1 in
        let e2 = encode_expr symbol_table e2 in
        binop ty op e1 e2
      | Triop (ty, op, e1, e2, e3) ->
        let e1 = encode_expr symbol_table e1 in
        let e2 = encode_expr symbol_table e2 in
        let e3 = encode_expr symbol_table e3 in
        triop ty op e1 e2 e3
      | Relop (ty, op, e1, e2) ->
        let e1 = encode_expr symbol_table e1 in
        let e2 = encode_expr symbol_table e2 in
        relop ty op e1 e2
      | Cvtop (ty, op, e) ->
        let e = encode_expr symbol_table e in
        cvtop ty op e
      | Naryop (ty, op, es) ->
        let es = List.map (encode_expr symbol_table) es in
        naryop ty op es
      | Extract (e, h, l) ->
        let e = encode_expr symbol_table e in
        M.Bitv.extract e ~high:((h * 8) - 1) ~low:(l * 8)
      | Concat (e1, e2) ->
        let e1 = encode_expr symbol_table e1 in
        let e2 = encode_expr symbol_table e2 in
        M.Bitv.concat e1 e2
      | List _ | App _ | Stringop _ -> assert false

    (* TODO: pp_smt *)
    let pp_smt ?status:_ _ _ = assert false

    let value_of_term model ty term =
      let v = M.Model.eval ~completion:true model term |> Option.get in
      match ty with
      | Ty_int -> Value.Int (M.Interp.to_int v)
      | Ty_real -> Value.Real (M.Interp.to_real v)
      | Ty_bool -> if M.Interp.to_bool v then Value.True else Value.False
      | Ty_str ->
        let str = M.Interp.to_string v in
        Value.Str str
      | Ty_bitv 8 ->
        let i8 = M.Interp.to_bitv v 8 in
        Value.Num (I8 (Int64.to_int i8))
      | Ty_bitv 32 ->
        let i32 = M.Interp.to_bitv v 32 in
        Value.Num (I32 (Int64.to_int32 i32))
      | Ty_bitv 64 ->
        let i64 = M.Interp.to_bitv v 64 in
        Value.Num (I64 i64)
      | Ty_fp 32 ->
        let float = M.Interp.to_float v 8 24 in
        Value.Num (F32 (Int32.bits_of_float float))
      | Ty_fp 64 ->
        let float = M.Interp.to_float v 11 53 in
        Value.Num (F64 (Int64.bits_of_float float))
      | Ty_bitv _ | Ty_fp _ | Ty_list | Ty_app | Ty_unit -> assert false

    let value ({ model = m; symbol_table } : model) (c : Expr.t) : Value.t =
      value_of_term m (Expr.ty c) (encode_expr symbol_table c)

    let values_of_model ?symbols ({ model; symbol_table } as model0) =
      let m = Hashtbl.create 512 in
      ( match symbols with
      | Some symbols ->
        List.iter
          (fun sym ->
            let v = value model0 (Expr.mk_symbol sym) in
            Hashtbl.replace m sym v )
          symbols
      | None ->
        Hashtbl.iter
          (fun (sym : Symbol.t) term ->
            let v = value_of_term model sym.ty term in
            Hashtbl.replace m sym v )
          symbol_table );
      m

    let set_debug _ = ()

    module Solver = struct
      let make ?params ?logic () =
        { solver = M.Solver.make ?params ?logic ()
        ; symbol_table = Hashtbl.create 16
        }

      let clone { solver; symbol_table } =
        { solver = M.Solver.clone solver
        ; symbol_table = Hashtbl.copy symbol_table
        }

      let push { solver; _ } = M.Solver.push solver

      let pop { solver; _ } n = M.Solver.pop solver n

      let reset { solver; _ } = M.Solver.reset solver

      let add { solver; symbol_table } (exprs : Expr.t list) =
        M.Solver.add solver (List.map (encode_expr symbol_table) exprs)

      let check { solver; symbol_table } ~assumptions =
        let assumptions = List.map (encode_expr symbol_table) assumptions in
        M.Solver.check solver ~assumptions

      let model { solver; symbol_table } =
        M.Solver.model solver
        |> Option.map (fun m -> { model = m; symbol_table })

      let add_simplifier { solver; symbol_table } =
        { solver = M.Solver.add_simplifier solver; symbol_table }

      let interrupt _ = M.Solver.interrupt ()

      let get_statistics { solver; _ } = M.Solver.get_statistics solver
    end

    module Optimizer = struct
      let make () =
        { opt = M.Optimizer.make (); symbol_table = Hashtbl.create 16 }

      let push { opt; _ } = M.Optimizer.push opt

      let pop { opt; _ } = M.Optimizer.pop opt

      let add { opt; symbol_table } exprs =
        M.Optimizer.add opt (List.map (encode_expr symbol_table) exprs)

      let check { opt; _ } = M.Optimizer.check opt

      let model { opt; symbol_table } =
        M.Optimizer.model opt
        |> Option.map (fun m -> { model = m; symbol_table })

      let maximize { opt; symbol_table } (expr : Expr.t) =
        M.Optimizer.maximize opt (encode_expr symbol_table expr)

      let minimize { opt; symbol_table } (expr : Expr.t) =
        M.Optimizer.minimize opt (encode_expr symbol_table expr)

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
