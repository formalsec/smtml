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

module Fresh = struct
  module Make () = struct
    let err = Log.err

    type expr = Z3.Expr.expr

    type model = Z3.Model.model

    type solver = Z3.Solver.solver

    type optimize = Z3.Optimize.optimize

    type handle = Z3.Optimize.handle

    let ctx = Z3.mk_context []

    let int_sort = Z3.Arithmetic.Integer.mk_sort ctx

    let real_sort = Z3.Arithmetic.Real.mk_sort ctx

    let bool_sort = Z3.Boolean.mk_sort ctx

    let str_sort = Z3.Seq.mk_string_sort ctx

    type poly_operations_t =
      { (****************)
        (* constructors *)
        (****************)
        undefined_constructor : Z3.FuncDecl.func_decl
      ; null_constructor : Z3.FuncDecl.func_decl
      ; error_constructor : Z3.FuncDecl.func_decl
      ; boolean_constructor : Z3.FuncDecl.func_decl
      ; int_constructor : Z3.FuncDecl.func_decl
      ; real_constructor : Z3.FuncDecl.func_decl
      ; string_constructor : Z3.FuncDecl.func_decl
      ; loc_constructor : Z3.FuncDecl.func_decl
      ; (*************)
        (* accessors *)
        (*************)
        boolean_accessor : Z3.FuncDecl.func_decl
      ; int_accessor : Z3.FuncDecl.func_decl
      ; real_accessor : Z3.FuncDecl.func_decl
      ; string_accessor : Z3.FuncDecl.func_decl
      }

    let mk_string_symb s = Z3.Symbol.mk_string ctx s

    let undefined_constructor =
      Z3.Datatype.mk_constructor ctx
        (mk_string_symb "Undefined")
        (mk_string_symb "isUndefined")
        [] [] []

    let null_constructor =
      Z3.Datatype.mk_constructor ctx (mk_string_symb "Null")
        (mk_string_symb "isNull") [] [] []

    let error_constructor =
      Z3.Datatype.mk_constructor ctx (mk_string_symb "Error")
        (mk_string_symb "isError") [] [] []

    let bool_constructor =
      Z3.Datatype.mk_constructor ctx (mk_string_symb "Bool")
        (mk_string_symb "isBool")
        [ mk_string_symb "bValue" ]
        [ Some bool_sort ] [ 0 ]

    let real_constructor =
      Z3.Datatype.mk_constructor ctx (mk_string_symb "Real")
        (mk_string_symb "isReal")
        [ mk_string_symb "rValue" ]
        [ Some real_sort ] [ 0 ]

    let int_constructor =
      Z3.Datatype.mk_constructor ctx (mk_string_symb "Int")
        (mk_string_symb "isInt")
        [ mk_string_symb "iValue" ]
        [ Some int_sort ] [ 0 ]

    let string_constructor =
      Z3.Datatype.mk_constructor ctx (mk_string_symb "String")
        (mk_string_symb "isString")
        [ mk_string_symb "sValue" ]
        [ Some str_sort ] [ 0 ]

    let loc_constructor =
      Z3.Datatype.mk_constructor ctx (mk_string_symb "Loc")
        (mk_string_symb "isLoc")
        [ mk_string_symb "locValue" ]
        [ Some int_sort ] [ 0 ]

    let poly_sort =
      Z3.Datatype.mk_sort ctx (mk_string_symb "Poly")
        [ undefined_constructor
        ; null_constructor
        ; error_constructor
        ; bool_constructor
        ; int_constructor
        ; real_constructor
        ; string_constructor
        ; loc_constructor
        ]

    (* Constructors *)
    let constructors = Z3.Datatype.get_constructors poly_sort

    let undefined_constructor = List.nth constructors 0

    let null_constructor = List.nth constructors 1

    let error_constructor = List.nth constructors 2

    let boolean_constructor = List.nth constructors 3

    let int_constructor = List.nth constructors 4

    let real_constructor = List.nth constructors 5

    let string_constructor = List.nth constructors 6

    let loc_constructor = List.nth constructors 7

    (* Accessors *)
    let accessors = Z3.Datatype.get_accessors poly_sort

    let boolean_accessor = List.nth (List.nth accessors 3) 0

    let int_accessor = List.nth (List.nth accessors 4) 0

    let real_accessor = List.nth (List.nth accessors 5) 0

    let string_accessor = List.nth (List.nth accessors 6) 0

    let poly_operations =
      { undefined_constructor
      ; null_constructor
      ; error_constructor
      ; boolean_constructor
      ; int_constructor
      ; real_constructor
      ; string_constructor
      ; loc_constructor
      ; boolean_accessor
      ; int_accessor
      ; real_accessor
      ; string_accessor
      }

    module Arithmetic = struct
      open Ty
      open Z3

      let int i = Arithmetic.Integer.mk_numeral_i ctx i

      let float f = Arithmetic.Real.mk_numeral_s ctx (Float.to_string f)

      let encode_unop op e =
        match op with
        | Neg -> Arithmetic.mk_unary_minus ctx e
        | Abs ->
          Boolean.mk_ite ctx
            (Arithmetic.mk_gt ctx e (float 0.))
            e
            (Arithmetic.mk_unary_minus ctx e)
        | Sqrt -> Arithmetic.mk_power ctx e (float 0.5)
        | Ceil ->
          let x_int = Arithmetic.Real.mk_real2int ctx e in
          Boolean.mk_ite ctx
            (Boolean.mk_eq ctx (Arithmetic.Integer.mk_int2real ctx x_int) e)
            x_int
            Arithmetic.(mk_add ctx [ x_int; Integer.mk_numeral_i ctx 1 ])
        | Floor -> Arithmetic.Real.mk_real2int ctx e
        | Trunc -> Arithmetic.Real.mk_real2int ctx e
        | Nearest | Is_nan | _ ->
          err {|Arith: Unsupported Z3 unop operator "%a"|} Ty.pp_unop op

      let encode_binop op e1 e2 =
        match op with
        | Add -> Arithmetic.mk_add ctx [ e1; e2 ]
        | Sub -> Arithmetic.mk_sub ctx [ e1; e2 ]
        | Mul -> Arithmetic.mk_mul ctx [ e1; e2 ]
        | Div -> Arithmetic.mk_div ctx e1 e2
        | Rem -> Arithmetic.Integer.mk_rem ctx e1 e2
        | Pow -> Arithmetic.mk_power ctx e1 e2
        | Min -> Boolean.mk_ite ctx (Arithmetic.mk_le ctx e1 e2) e1 e2
        | Max -> Boolean.mk_ite ctx (Arithmetic.mk_ge ctx e1 e2) e1 e2
        | _ -> err {|Real: Unsupported Z3 binop operator "%a"|} Ty.pp_binop op

      let encode_triop op _ =
        err {|Arith: Unsupported Z3 triop operator "%a"|} Ty.pp_triop op

      let encode_relop op e1 e2 =
        match op with
        | Lt -> Arithmetic.mk_lt ctx e1 e2
        | Gt -> Arithmetic.mk_gt ctx e1 e2
        | Le -> Arithmetic.mk_le ctx e1 e2
        | Ge -> Arithmetic.mk_ge ctx e1 e2
        | op -> err {|Arith: Unsupported Z3 relop operator "%a"|} Ty.pp_relop op

      module Integer = struct
        let v i = int i

        let int2str =
          FuncDecl.mk_func_decl_s ctx "IntToString" [ int_sort ] str_sort

        let str2int =
          FuncDecl.mk_func_decl_s ctx "StringToInt" [ str_sort ] int_sort

        let encode_cvtop op e =
          match op with
          | ToString ->
            let n_e = Z3.Expr.mk_app ctx poly_operations.int_accessor [ e ] in
            let e' = FuncDecl.apply int2str [ n_e ] in
            Z3.Expr.mk_app ctx poly_operations.string_constructor [ e' ]
          | OfString ->
            let n_e =
              Z3.Expr.mk_app ctx poly_operations.string_accessor [ e ]
            in
            let e' = FuncDecl.apply str2int [ n_e ] in
            Z3.Expr.mk_app ctx poly_operations.int_constructor [ e' ]
          | Reinterpret_float ->
            let n_e = Z3.Expr.mk_app ctx poly_operations.real_accessor [ e ] in
            let e' = Arithmetic.Real.mk_real2int ctx n_e in
            Z3.Expr.mk_app ctx poly_operations.int_constructor [ e' ]
          | op -> err {|Int: Unsupported Z3 cvtop operator "%a"|} Ty.pp_cvtop op
      end

      module Real = struct
        let v f = float f

        let real2str =
          FuncDecl.mk_func_decl_s ctx "RealToString" [ real_sort ] str_sort

        let str2real =
          FuncDecl.mk_func_decl_s ctx "StringToReal" [ str_sort ] real_sort

        let to_uint32 =
          FuncDecl.mk_func_decl_s ctx "ToUInt32" [ real_sort ] real_sort

        let encode_cvtop op e =
          match op with
          | ToString ->
            let n_e = Z3.Expr.mk_app ctx poly_operations.real_accessor [ e ] in
            let e' = FuncDecl.apply real2str [ n_e ] in
            Z3.Expr.mk_app ctx poly_operations.string_constructor [ e' ]
          | OfString ->
            let n_e =
              Z3.Expr.mk_app ctx poly_operations.string_accessor [ e ]
            in
            let e' = FuncDecl.apply str2real [ n_e ] in
            Z3.Expr.mk_app ctx poly_operations.real_constructor [ e' ]
          | ConvertUI32 ->
            let n_e = Z3.Expr.mk_app ctx poly_operations.real_accessor [ e ] in
            let e' = FuncDecl.apply to_uint32 [ n_e ] in
            Z3.Expr.mk_app ctx poly_operations.real_constructor [ e' ]
          | Reinterpret_int ->
            let n_e = Z3.Expr.mk_app ctx poly_operations.int_accessor [ e ] in
            let e' = Arithmetic.Integer.mk_int2real ctx n_e in
            Z3.Expr.mk_app ctx poly_operations.real_constructor [ e' ]
          | _ -> err {|Real: Unsupported Z3 cvtop operator "%a"|} Ty.pp_cvtop op
      end
    end

    module Boolean = struct
      open Z3
      open Ty

      let true_ = Boolean.mk_true ctx

      let false_ = Boolean.mk_false ctx

      let encode_unop = function
        | Not -> Boolean.mk_not ctx
        | op -> err {|Bool: Unsupported Z3 unop operator "%a"|} Ty.pp_unop op

      let encode_binop op e1 e2 =
        match op with
        | And -> Boolean.mk_and ctx [ e1; e2 ]
        | Or -> Boolean.mk_or ctx [ e1; e2 ]
        | Xor -> Boolean.mk_xor ctx e1 e2
        | _ -> err {|Bool: Unsupported Z3 binop operator "%a"|} Ty.pp_binop op

      let encode_triop op e1 e2 e3 =
        match op with
        | Ite ->
          let cond =
            Z3.Expr.mk_app ctx poly_operations.boolean_accessor [ e1 ]
          in
          Boolean.mk_ite ctx cond e2 e3
        | op -> err {|Bool: Unsupported Z3 triop operator "%a"|} Ty.pp_triop op

      let encode_cvtop _op _e = assert false

      let encode_naryop op es =
        match op with
        | Logand -> Boolean.mk_and ctx es
        | Logor -> Boolean.mk_or ctx es
        | _ -> err {|Bool: Unsupported Z3 naryop operator "%a"|} Ty.pp_naryop op
    end

    module Str = struct
      open Ty
      module Seq = Z3.Seq
      module FuncDecl = Z3.FuncDecl

      let v s = Seq.mk_string ctx s

      let trim = FuncDecl.mk_func_decl_s ctx "Trim" [ str_sort ] str_sort

      let encode_unop op e =
        match op with
        | Length ->
          let n_e = Z3.Expr.mk_app ctx poly_operations.string_accessor [ e ] in
          let op_e = Seq.mk_seq_length ctx n_e in
          Z3.Expr.mk_app ctx poly_operations.int_constructor [ op_e ]
        | Trim ->
          let n_e = Z3.Expr.mk_app ctx poly_operations.string_accessor [ e ] in
          let op_e = FuncDecl.apply trim [ n_e ] in
          Z3.Expr.mk_app ctx poly_operations.string_constructor [ op_e ]
        | _ -> err {|Str: Unsupported Z3 unop operator "%a"|} Ty.pp_unop op

      let encode_binop op e1 e2 =
        match op with
        | At ->
          let n_e1 =
            Z3.Expr.mk_app ctx poly_operations.string_accessor [ e1 ]
          in
          let n_e2 = Z3.Expr.mk_app ctx poly_operations.int_accessor [ e2 ] in
          let e = Seq.mk_seq_at ctx n_e1 n_e2 in
          Z3.Expr.mk_app ctx poly_operations.string_constructor [ e ]
        | String_prefix ->
          let n_e1 =
            Z3.Expr.mk_app ctx poly_operations.string_accessor [ e1 ]
          in
          let n_e2 =
            Z3.Expr.mk_app ctx poly_operations.string_accessor [ e2 ]
          in
          let e = Seq.mk_seq_prefix ctx n_e1 n_e2 in
          Z3.Expr.mk_app ctx poly_operations.string_constructor [ e ]
        | String_suffix ->
          let n_e1 =
            Z3.Expr.mk_app ctx poly_operations.string_accessor [ e1 ]
          in
          let n_e2 =
            Z3.Expr.mk_app ctx poly_operations.string_accessor [ e2 ]
          in
          let e = Seq.mk_seq_suffix ctx n_e1 n_e2 in
          Z3.Expr.mk_app ctx poly_operations.string_constructor [ e ]
        | String_contains ->
          let n_e1 =
            Z3.Expr.mk_app ctx poly_operations.string_accessor [ e1 ]
          in
          let n_e2 =
            Z3.Expr.mk_app ctx poly_operations.string_accessor [ e2 ]
          in
          let e = Seq.mk_seq_contains ctx n_e1 n_e2 in
          Z3.Expr.mk_app ctx poly_operations.boolean_constructor [ e ]
        | String_last_index ->
          let n_e1 =
            Z3.Expr.mk_app ctx poly_operations.string_accessor [ e1 ]
          in
          let n_e2 =
            Z3.Expr.mk_app ctx poly_operations.string_accessor [ e2 ]
          in
          let e = Seq.mk_seq_last_index ctx n_e1 n_e2 in
          Z3.Expr.mk_app ctx poly_operations.int_constructor [ e ]
        | _ -> err {|Str: Unsupported Z3 binop operator "%a"|} Ty.pp_binop op

      let encode_triop op e1 e2 e3 =
        match op with
        | String_extract ->
          let n_e1 =
            Z3.Expr.mk_app ctx poly_operations.string_accessor [ e1 ]
          in
          let n_e2 = Z3.Expr.mk_app ctx poly_operations.int_accessor [ e2 ] in
          let n_e3 = Z3.Expr.mk_app ctx poly_operations.int_accessor [ e3 ] in
          let e = Seq.mk_seq_extract ctx n_e1 n_e2 n_e3 in
          Z3.Expr.mk_app ctx poly_operations.string_constructor [ e ]
        | String_replace ->
          let n_e1 =
            Z3.Expr.mk_app ctx poly_operations.string_accessor [ e1 ]
          in
          let n_e2 =
            Z3.Expr.mk_app ctx poly_operations.string_accessor [ e2 ]
          in
          let n_e3 =
            Z3.Expr.mk_app ctx poly_operations.string_accessor [ e3 ]
          in
          let e = Seq.mk_seq_replace ctx n_e1 n_e2 n_e3 in
          Z3.Expr.mk_app ctx poly_operations.string_constructor [ e ]
        | String_index ->
          let n_e1 =
            Z3.Expr.mk_app ctx poly_operations.string_accessor [ e1 ]
          in
          let n_e2 =
            Z3.Expr.mk_app ctx poly_operations.string_accessor [ e2 ]
          in
          let n_e3 = Z3.Expr.mk_app ctx poly_operations.int_accessor [ e3 ] in
          let e = Seq.mk_seq_index ctx n_e1 n_e2 n_e3 in
          Z3.Expr.mk_app ctx poly_operations.int_constructor [ e ]
        | op -> err {|Str: Unsupported Z3 triop operator "%a"|} Ty.pp_triop op

      let encode_cvtop op e =
        match op with
        | String_to_code ->
          let n_e = Z3.Expr.mk_app ctx poly_operations.string_accessor [ e ] in
          let e' = Seq.mk_string_to_code ctx n_e in
          Z3.Expr.mk_app ctx poly_operations.int_constructor [ e' ]
        | String_from_code ->
          let n_e = Z3.Expr.mk_app ctx poly_operations.int_accessor [ e ] in
          let e' = Seq.mk_string_from_code ctx n_e in
          Z3.Expr.mk_app ctx poly_operations.string_constructor [ e' ]
        | String_to_int ->
          let n_e = Z3.Expr.mk_app ctx poly_operations.string_accessor [ e ] in
          let e' = Seq.mk_str_to_int ctx n_e in
          Z3.Expr.mk_app ctx poly_operations.int_constructor [ e' ]
        | String_from_int ->
          let n_e = Z3.Expr.mk_app ctx poly_operations.int_accessor [ e ] in
          let e' = Seq.mk_int_to_str ctx n_e in
          Z3.Expr.mk_app ctx poly_operations.string_constructor [ e' ]
        | op -> err {|Str: Unsupported Z3 cvtop operator "%a"|} Ty.pp_cvtop op

      let encode_naryop op es =
        match op with
        | Concat -> Seq.mk_seq_concat ctx es
        | _ -> err {|Str: Unsupported Z3 naryop operator "%a"|} Ty.pp_naryop op
    end

    let symtable = Hashtbl.create 512

    let encode_val : Value.t -> Z3.Expr.expr = function
      | True ->
        Z3.Expr.mk_app ctx poly_operations.boolean_constructor [ Boolean.true_ ]
      | False ->
        Z3.Expr.mk_app ctx poly_operations.boolean_constructor
          [ Boolean.false_ ]
      | Int v ->
        Z3.Expr.mk_app ctx poly_operations.int_constructor
          [ Arithmetic.Integer.v v ]
      | Real v ->
        Z3.Expr.mk_app ctx poly_operations.real_constructor
          [ Arithmetic.Real.v v ]
      | Str v ->
        Z3.Expr.mk_app ctx poly_operations.string_constructor [ Str.v v ]
      | App (v, l) -> (
        match v with
        | `Op "symbol" ->
          Z3.Expr.mk_app ctx poly_operations.undefined_constructor []
        | `Op "null" -> Z3.Expr.mk_app ctx poly_operations.null_constructor []
        | `Op "error" -> Z3.Expr.mk_app ctx poly_operations.error_constructor []
        | `Op "loc" -> (
          match List.hd l with
          | Int v ->
            Z3.Expr.mk_app ctx poly_operations.loc_constructor
              [ Arithmetic.Integer.v v ]
          | _ -> assert false )
        | _ -> assert false )
      | _ -> assert false

    let unop_ints op e =
      let n_e = Z3.Expr.mk_app ctx poly_operations.int_accessor [ e ] in
      let op_e = op n_e in
      Z3.Expr.mk_app ctx poly_operations.int_constructor [ op_e ]

    let unop_reals op e =
      let n_e = Z3.Expr.mk_app ctx poly_operations.real_accessor [ e ] in
      let op_e = op n_e in
      Z3.Expr.mk_app ctx poly_operations.real_constructor [ op_e ]

    let unop_bools op e =
      let n_e = Z3.Expr.mk_app ctx poly_operations.boolean_accessor [ e ] in
      let op_e = op n_e in
      Z3.Expr.mk_app ctx poly_operations.boolean_constructor [ op_e ]

    let encode_unop (t : Ty.t) (unop : Ty.unop) (e : expr) : expr =
      match t with
      | Ty.Ty_int -> unop_ints (Arithmetic.encode_unop unop) e
      | Ty.Ty_real -> unop_reals (Arithmetic.encode_unop unop) e
      | Ty.Ty_bool -> unop_bools (Boolean.encode_unop unop) e
      | Ty.Ty_str -> Str.encode_unop unop e
      | _ -> assert false

    let binop_ints_to_ints op e1 e2 =
      let n_e1 = Z3.Expr.mk_app ctx poly_operations.int_accessor [ e1 ] in
      let n_e2 = Z3.Expr.mk_app ctx poly_operations.int_accessor [ e2 ] in
      let op_e = op n_e1 n_e2 in
      Z3.Expr.mk_app ctx poly_operations.int_constructor [ op_e ]

    let binop_reals_to_reals op e1 e2 =
      let n_e1 = Z3.Expr.mk_app ctx poly_operations.real_accessor [ e1 ] in
      let n_e2 = Z3.Expr.mk_app ctx poly_operations.real_accessor [ e2 ] in
      let op_e = op n_e1 n_e2 in
      Z3.Expr.mk_app ctx poly_operations.real_constructor [ op_e ]

    let binop_bools op e1 e2 =
      let n_e1 = Z3.Expr.mk_app ctx poly_operations.boolean_accessor [ e1 ] in
      let n_e2 = Z3.Expr.mk_app ctx poly_operations.boolean_accessor [ e2 ] in
      let op_e = op n_e1 n_e2 in
      Z3.Expr.mk_app ctx poly_operations.boolean_constructor [ op_e ]

    let encode_binop (t : Ty.t) (binop : Ty.binop) (e1 : expr) (e2 : expr) :
      expr =
      match t with
      | Ty.Ty_int -> binop_ints_to_ints (Arithmetic.encode_binop binop) e1 e2
      | Ty.Ty_real -> binop_reals_to_reals (Arithmetic.encode_binop binop) e1 e2
      | Ty.Ty_bool -> binop_bools (Boolean.encode_binop binop) e1 e2
      (* binops on strings differ a lot in argument types so this is handled
         directly in the Str module *)
      | Ty.Ty_str -> Str.encode_binop binop e1 e2
      | _ -> assert false

    let encode_triop = function
      | Ty.Ty_int | Ty_real -> Arithmetic.encode_triop
      | Ty.Ty_bool -> Boolean.encode_triop
      | Ty.Ty_str -> Str.encode_triop
      | _ -> assert false

    let relop_ints_to_bools op e1 e2 =
      let n_e1 = Z3.Expr.mk_app ctx poly_operations.int_accessor [ e1 ] in
      let n_e2 = Z3.Expr.mk_app ctx poly_operations.int_accessor [ e2 ] in
      let op_e = op n_e1 n_e2 in
      Z3.Expr.mk_app ctx poly_operations.boolean_constructor [ op_e ]

    let relop_reals_to_bools op e1 e2 =
      let n_e1 = Z3.Expr.mk_app ctx poly_operations.real_accessor [ e1 ] in
      let n_e2 = Z3.Expr.mk_app ctx poly_operations.real_accessor [ e2 ] in
      let op_e = op n_e1 n_e2 in
      Z3.Expr.mk_app ctx poly_operations.boolean_constructor [ op_e ]

    let encode_relop (t : Ty.t) (relop : Ty.relop) (e1 : expr) (e2 : expr) :
      expr =
      match (t, relop) with
      | _, Eq ->
        let e = Z3.Boolean.mk_eq ctx e1 e2 in
        Z3.Expr.mk_app ctx poly_operations.boolean_constructor [ e ]
      | _, Ne ->
        let e = Z3.Boolean.mk_distinct ctx [ e1; e2 ] in
        Z3.Expr.mk_app ctx poly_operations.boolean_constructor [ e ]
      | Ty.Ty_int, _ ->
        relop_ints_to_bools (Arithmetic.encode_relop relop) e1 e2
      | Ty.Ty_real, _ ->
        relop_reals_to_bools (Arithmetic.encode_relop relop) e1 e2
      | _ -> assert false

    let encode_cvtop = function
      | Ty.Ty_int -> Arithmetic.Integer.encode_cvtop
      | Ty.Ty_real -> Arithmetic.Real.encode_cvtop
      | Ty.Ty_bool -> Boolean.encode_cvtop
      | Ty.Ty_str -> Str.encode_cvtop
      | _ -> assert false

    let naryop_bools_to_bools op es =
      let n_es =
        List.map
          (fun e -> Z3.Expr.mk_app ctx poly_operations.boolean_accessor [ e ])
          es
      in
      let op_e = op n_es in
      Z3.Expr.mk_app ctx poly_operations.boolean_constructor [ op_e ]

    let naryop_str_to_str op es =
      let n_es =
        List.map
          (fun e -> Z3.Expr.mk_app ctx poly_operations.string_accessor [ e ])
          es
      in
      let op_e = op n_es in
      Z3.Expr.mk_app ctx poly_operations.string_constructor [ op_e ]

    let encode_naryop (t : Ty.t) (naryop : Ty.naryop) (es : expr list) : expr =
      match t with
      | Ty.Ty_bool -> naryop_bools_to_bools (Boolean.encode_naryop naryop) es
      | Ty.Ty_str -> naryop_str_to_str (Str.encode_naryop naryop) es
      | _ -> assert false

    let rec encode_expr (hte : Expr.t) : expr =
      let open Expr in
      match view hte with
      | Val v -> encode_val v
      | Unop (ty, op, e) ->
        let e' = encode_expr e in
        encode_unop ty op e'
      | Binop (ty, op, e1, e2) ->
        let e1' = encode_expr e1 in
        let e2' = encode_expr e2 in
        encode_binop ty op e1' e2'
      | Triop (ty, op, e1, e2, e3) ->
        let e1' = encode_expr e1
        and e2' = encode_expr e2
        and e3' = encode_expr e3 in
        encode_triop ty op e1' e2' e3'
      | Relop (ty, op, e1, e2) ->
        let e1' = encode_expr e1
        and e2' = encode_expr e2 in
        encode_relop ty op e1' e2'
      | Cvtop (ty, op, e) ->
        let e' = encode_expr e in
        encode_cvtop ty op e'
      | Naryop (ty, op, es) ->
        let es' = List.map encode_expr es in
        encode_naryop ty op es'
      | Symbol { name; _ } ->
        Hashtbl.replace symtable name hte;
        Z3.Expr.mk_const_s ctx name poly_sort
      | _ -> assert false

    let set_params (params : Params.t) =
      let module P = Z3.Params in
      Z3.set_global_param "smt.ematching"
        (string_of_bool @@ Params.get params Ematching);
      P.update_param_value ctx "timeout"
        (string_of_int @@ Params.get params Timeout);
      P.update_param_value ctx "model"
        (string_of_bool @@ Params.get params Model);
      P.update_param_value ctx "unsat_core"
        (string_of_bool @@ Params.get params Unsat_core)

    let pp_smt ?status fmt (es : Expr.t list) =
      let st = match status with Some b -> string_of_bool b | None -> "" in
      let es' = List.map encode_expr es in
      Z3.Params.set_print_mode ctx Z3enums.PRINT_SMTLIB2_COMPLIANT;
      Format.fprintf fmt "%s"
        (Z3.SMT.benchmark_to_smtstring ctx "" "" st "" (List.tl es')
           (List.hd es') )

    module Solver = struct
      let make ?params ?logic () : solver =
        Option.iter set_params params;
        let logic =
          Option.map
            (fun l ->
              Format.kasprintf (Z3.Symbol.mk_string ctx) "%a" Ty.pp_logic l )
            logic
        in
        Z3.Solver.mk_solver ctx logic

      let add_simplifier solver =
        let simplify = Z3.Simplifier.mk_simplifier ctx "simplify" in
        let solve_eqs = Z3.Simplifier.mk_simplifier ctx "solve-eqs" in
        let then_ =
          List.map
            (Z3.Simplifier.mk_simplifier ctx)
            [ "elim-unconstrained"; "propagate-values"; "simplify" ]
        in
        let simplifier = Z3.Simplifier.and_then ctx simplify solve_eqs then_ in
        Z3.Solver.add_simplifier ctx solver simplifier

      let clone s = Z3.Solver.translate s ctx

      let push s = Z3.Solver.push s

      let pop s lvl = Z3.Solver.pop s lvl

      let reset s = Z3.Solver.reset s [@@inline]

      let check_bool_sort e =
        match Z3.Boolean.is_bool e with
        | true -> e
        | false -> Z3.Expr.mk_app ctx poly_operations.boolean_accessor [ e ]

      let add s es =
        let l = List.map check_bool_sort (List.map encode_expr es) in
        Z3.Solver.add s l

      let check s ~assumptions =
        let l = List.map check_bool_sort (List.map encode_expr assumptions) in
        match Z3.Solver.check s l with
        | Z3.Solver.UNKNOWN -> `Unknown
        | Z3.Solver.SATISFIABLE -> `Sat
        | Z3.Solver.UNSATISFIABLE -> `Unsat

      let model s = Z3.Solver.get_model s

      let interrupt _ = Z3.Tactic.interrupt ctx

      let get_statistics _ =
        failwith "Poly_mappings: Solver.get_statistics not implemented!"
    end

    module Optimizer = struct
      let make () = Z3.Optimize.mk_opt ctx

      let push o = Z3.Optimize.push o

      let pop o = Z3.Optimize.pop o

      let add o es = Z3.Optimize.add o (List.map encode_expr es)

      let check o =
        match Z3.Optimize.check o with
        | Z3.Solver.UNKNOWN -> `Unknown
        | Z3.Solver.SATISFIABLE -> `Sat
        | Z3.Solver.UNSATISFIABLE -> `Unsat

      let model o = Z3.Optimize.get_model o

      let maximize o e = Z3.Optimize.maximize o (encode_expr e)

      let minimize o e = Z3.Optimize.minimize o (encode_expr e)

      let interrupt _ = Z3.Tactic.interrupt ctx

      let get_statistics _ =
        failwith "Poly_mappings: Optimizer.get_statistics not implemented!"
    end

    let recover_z3_num (n : Z3.Expr.expr) : float option =
      if Z3.Expr.is_numeral n then
        Some (float_of_string (Z3.Arithmetic.Real.to_decimal_string n 16))
      else None

    let recover_z3_int (n : Z3.Expr.expr) : int option =
      let i = recover_z3_num n in
      Option.map int_of_float i

    let value (model : Z3.Model.model) (c : Expr.t) : Value.t =
      let open Value in
      let e = Z3.Model.eval model (encode_expr c) true |> Option.get in
      match (Expr.ty c, Z3.Sort.get_sort_kind @@ Z3.Expr.get_sort e) with
      | Ty_int, Z3enums.DATATYPE_SORT -> (
        let e' = Z3.Expr.mk_app ctx poly_operations.int_accessor [ e ] in
        let v = Z3.Model.eval model e' true |> Option.get in
        match recover_z3_int v with Some i -> Int i | None -> assert false )
      | Ty_real, Z3enums.DATATYPE_SORT -> (
        let e' = Z3.Expr.mk_app ctx poly_operations.real_accessor [ e ] in
        let v = Z3.Model.eval model e' true |> Option.get in
        match recover_z3_num v with Some f -> Real f | None -> assert false )
      | Ty_bool, Z3enums.DATATYPE_SORT ->
        let e' = Z3.Expr.mk_app ctx poly_operations.boolean_accessor [ e ] in
        let v = Z3.Model.eval model e' true |> Option.get in
        if Z3.Boolean.get_bool_value v = Z3enums.L_TRUE then True else False
      | Ty_str, Z3enums.DATATYPE_SORT ->
        let e' = Z3.Expr.mk_app ctx poly_operations.string_accessor [ e ] in
        let v = Z3.Model.eval model e' true |> Option.get in
        Str (Z3.Seq.get_string ctx v)
      | _ -> assert false

    let symbols_of_model (model : Z3.Model.model) : Symbol.t list =
      let decls = Z3.Model.get_const_decls model in
      let name_list =
        List.map Z3.FuncDecl.get_name decls |> List.map Z3.Symbol.get_string
      in
      let sym_list =
        Hashtbl.fold
          (fun k v acc -> if List.mem k name_list then v :: acc else acc)
          symtable []
      in
      Expr.get_symbols sym_list

    let values_of_model ?(symbols : Symbol.t list option)
      (model : Z3.Model.model) : Model.t =
      let m = Hashtbl.create 512 in
      let symbols' = Option.value symbols ~default:(symbols_of_model model) in
      List.iter
        (fun sym ->
          let v = value model (Expr.mk_symbol sym) in
          Hashtbl.replace m sym v )
        symbols';
      m

    let set_debug _ = ()
  end
end

let is_available = true

include Fresh.Make ()
