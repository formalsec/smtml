module Fresh = struct
  module Make () = struct
    let err = Log.err

    type expr = Z3.Expr.expr
    type model = Z3.Model.model
    type solver = Z3.Solver.solver
    type status = Z3.Solver.status
    type optimize = Z3.Optimize.optimize
    type handle = Z3.Optimize.handle

    let ctx = Z3.mk_context []
    let int_sort = Z3.Arithmetic.Integer.mk_sort ctx
    let real_sort = Z3.Arithmetic.Real.mk_sort ctx
    let bool_sort = Z3.Boolean.mk_sort ctx
    let str_sort = Z3.Seq.mk_string_sort ctx
    let bv8_sort = Z3.BitVector.mk_sort ctx 8
    let bv32_sort = Z3.BitVector.mk_sort ctx 32
    let bv64_sort = Z3.BitVector.mk_sort ctx 64
    let fp32_sort = Z3.FloatingPoint.mk_sort_single ctx
    let fp64_sort = Z3.FloatingPoint.mk_sort_double ctx
    let rne = Z3.FloatingPoint.RoundingMode.mk_rne ctx
    let rtz = Z3.FloatingPoint.RoundingMode.mk_rtz ctx
    let rtp = Z3.FloatingPoint.RoundingMode.mk_rtp ctx
    let rtn = Z3.FloatingPoint.RoundingMode.mk_rtn ctx

    let get_sort (e : Ty.t) : Z3.Sort.sort =
      match e with
      | Ty_int -> int_sort
      | Ty_real -> real_sort
      | Ty_bool -> bool_sort
      | Ty_str -> str_sort
      | Ty_bitv S8 -> bv8_sort
      | Ty_bitv S32 -> bv32_sort
      | Ty_bitv S64 -> bv64_sort
      | Ty_fp S32 -> fp32_sort
      | Ty_fp S64 -> fp64_sort
      | Ty_fp S8 -> assert false

    module Arith = struct
      open Ty
      open Z3

      let encode_triop op _ =
        err {|Arith: Unsupported Z3 triop operator "%a"|} Ty.pp_triop op

      let encode_relop op e1 e2 =
        let op' =
          match op with
          | Eq -> Boolean.mk_eq ctx
          | Ne -> fun v1 v2 -> Boolean.mk_eq ctx v1 v2 |> Boolean.mk_not ctx
          | Lt -> Arithmetic.mk_lt ctx
          | Gt -> Arithmetic.mk_gt ctx
          | Le -> Arithmetic.mk_le ctx
          | Ge -> Arithmetic.mk_ge ctx
          | _ ->
            err {|Arith: Unsupported Z3 relop operator "%a"|} Ty.pp_relop op
        in
        op' e1 e2
    end

    module I :
      Op_intf.S
        with type v := int
         and type t := Z3.Expr.expr
         and type unop := Ty.unop
         and type binop := Ty.binop
         and type relop := Ty.relop
         and type cvtop := Ty.cvtop
         and type triop := Ty.triop = struct
      open Ty
      open Z3
      include Arith

      let int2str =
        FuncDecl.mk_func_decl_s ctx "IntToString" [ int_sort ] str_sort

      let str2int =
        FuncDecl.mk_func_decl_s ctx "StringToInt" [ str_sort ] int_sort

      let encode_val i = Expr.mk_numeral_int ctx i int_sort

      let encode_unop op e =
        let op' =
          match op with
          | Neg -> Arithmetic.mk_unary_minus ctx
          | _ -> err {|Int: Unsupported Z3 unop operator "%a"|} Ty.pp_unop op
        in
        op' e

      let encode_binop op e1 e2 =
        let op' =
          match op with
          | Add -> fun v1 v2 -> Arithmetic.mk_add ctx [ v1; v2 ]
          | Sub -> fun v1 v2 -> Arithmetic.mk_sub ctx [ v1; v2 ]
          | Mul -> fun v1 v2 -> Arithmetic.mk_mul ctx [ v1; v2 ]
          | Div -> Arithmetic.mk_div ctx
          | Rem -> Arithmetic.Integer.mk_rem ctx
          | Pow -> Arithmetic.mk_power ctx
          | _ -> err {|Int: Unsupported Z3 binop operator "%a"|} Ty.pp_binop op
        in
        op' e1 e2

      let encode_cvtop op e =
        let op' =
          match op with
          | ToString -> fun v -> FuncDecl.apply int2str [ v ]
          | OfString -> fun v -> FuncDecl.apply str2int [ v ]
          | Reinterpret_float -> Arithmetic.Real.mk_real2int ctx
          | _ -> err {|Int: Unsupported Z3 cvtop operator "%a"|} Ty.pp_cvtop op
        in
        op' e
    end

    module Real :
      Op_intf.S
        with type v := float
         and type t := Z3.Expr.expr
         and type unop := Ty.unop
         and type binop := Ty.binop
         and type relop := Ty.relop
         and type cvtop := Ty.cvtop
         and type triop := Ty.triop = struct
      open Z3
      open Ty
      include Arith

      let real2str =
        FuncDecl.mk_func_decl_s ctx "RealToString" [ real_sort ] str_sort

      let str2real =
        FuncDecl.mk_func_decl_s ctx "StringToReal" [ str_sort ] real_sort

      let to_uint32 =
        FuncDecl.mk_func_decl_s ctx "ToUInt32" [ real_sort ] real_sort

      let encode_val f = Arithmetic.Real.mk_numeral_s ctx (Float.to_string f)

      let encode_unop op e =
        let op' =
          match op with
          | Neg -> Arithmetic.mk_unary_minus ctx
          | Abs ->
            fun x ->
              Boolean.mk_ite ctx
                (Arithmetic.mk_gt ctx x (encode_val 0.))
                x
                (Arithmetic.mk_unary_minus ctx x)
          | Sqrt -> fun x -> Arithmetic.mk_power ctx x (encode_val 0.5)
          | Ceil ->
            fun x ->
              let x_int = Arithmetic.Real.mk_real2int ctx x in
              Boolean.mk_ite ctx
                (Boolean.mk_eq ctx (Arithmetic.Integer.mk_int2real ctx x_int) x)
                x_int
                Arithmetic.(mk_add ctx [ x_int; Integer.mk_numeral_i ctx 1 ])
          | Floor -> Arithmetic.Real.mk_real2int ctx
          | Nearest | Is_nan | _ ->
            err {|Real: Unsupported Z3 cvtop operator "%a"|} Ty.pp_unop op
        in
        op' e

      let encode_binop op e1 e2 =
        let op' =
          match op with
          | Add -> fun v1 v2 -> Arithmetic.mk_add ctx [ v1; v2 ]
          | Sub -> fun v1 v2 -> Arithmetic.mk_sub ctx [ v1; v2 ]
          | Mul -> fun v1 v2 -> Arithmetic.mk_mul ctx [ v1; v2 ]
          | Div -> Arithmetic.mk_div ctx
          | Min ->
            fun v1 v2 -> Boolean.mk_ite ctx (Arithmetic.mk_le ctx v1 v2) v1 v2
          | Max ->
            fun v1 v2 -> Boolean.mk_ite ctx (Arithmetic.mk_ge ctx v1 v2) v1 v2
          | _ -> err {|Real: Unsupported Z3 binop operator "%a"|} Ty.pp_binop op
        in
        op' e1 e2

      let encode_cvtop op e =
        let op' =
          match op with
          | ToString -> fun v -> FuncDecl.apply real2str [ v ]
          | OfString -> fun v -> FuncDecl.apply str2real [ v ]
          | ConvertUI32 -> fun v -> FuncDecl.apply to_uint32 [ v ]
          | Reinterpret_int -> Arithmetic.Integer.mk_int2real ctx
          | _ -> err {|Real: Unsupported Z3 cvtop operator "%a"|} Ty.pp_cvtop op
        in
        op' e
    end

    module Boolean = struct
      open Z3
      open Ty

      let encode_true () = Boolean.mk_true ctx
      let encode_false () = Boolean.mk_false ctx

      let encode_unop op e =
        let op' =
          match op with
          | Not -> Boolean.mk_not ctx
          | _ -> err {|Bool: Unsupported Z3 unop operator "%a"|} Ty.pp_unop op
        in
        op' e

      let encode_binop op e1 e2 =
        let op' =
          match op with
          | And -> fun v1 v2 -> Boolean.mk_and ctx [ v1; v2 ]
          | Or -> fun v1 v2 -> Boolean.mk_or ctx [ v1; v2 ]
          | Xor -> Boolean.mk_xor ctx
          | _ -> err {|Bool: Unsupported Z3 binop operator "%a"|} Ty.pp_binop op
        in
        op' e1 e2

      let encode_triop op e1 e2 e3 =
        let op' =
          match op with
          | Ite -> Boolean.mk_ite ctx
          | _ -> err {|Bool: Unsupported Z3 triop operator "%a"|} Ty.pp_triop op
        in
        op' e1 e2 e3

      let encode_relop op e1 e2 =
        let op' =
          match op with
          | Eq -> Boolean.mk_eq ctx
          | Ne -> fun v1 v2 -> Boolean.mk_eq ctx v1 v2 |> Boolean.mk_not ctx
          | _ -> err {|Bool: Unsupported Z3 relop operator "%a"|} Ty.pp_relop op
        in
        op' e1 e2

      let encode_cvtop _op _e = assert false
    end

    module Str :
      Op_intf.S
        with type v := string
         and type t := Z3.Expr.expr
         and type unop := Ty.unop
         and type binop := Ty.binop
         and type relop := Ty.relop
         and type cvtop := Ty.cvtop
         and type triop := Ty.triop = struct
      open Z3
      open Ty

      let encode_val s = Seq.mk_string ctx s
      let trim = FuncDecl.mk_func_decl_s ctx "Trim" [ str_sort ] str_sort

      let encode_unop op e =
        let op' =
          match op with
          | Len -> Seq.mk_seq_length ctx
          | Trim -> fun v -> FuncDecl.apply trim [ v ]
          | _ -> err {|Str: Unsupported Z3 unop operator "%a"|} Ty.pp_unop op
        in
        op' e

      let encode_binop op e1 e2 =
        let op' =
          match op with
          | Nth ->
            fun v1 v2 ->
              Seq.mk_seq_extract ctx v1 v2 (Expr.mk_numeral_int ctx 1 int_sort)
          | Concat -> fun v1 v2 -> Seq.mk_seq_concat ctx [ v1; v2 ]
          | _ -> err {|Str: Unsupported Z3 binop operator "%a"|} Ty.pp_binop op
        in
        op' e1 e2

      let encode_triop op e1 e2 e3 =
        let op' =
          match op with
          | Substr -> Seq.mk_seq_extract ctx
          | _ -> err {|Str: Unsupported Z3 triop operator "%a"|} Ty.pp_triop op
        in
        op' e1 e2 e3

      let encode_relop op e1 e2 =
        let op' =
          match op with
          | Eq -> Boolean.mk_eq ctx
          | Ne -> fun v1 v2 -> Boolean.mk_eq ctx v1 v2 |> Boolean.mk_not ctx
          | _ -> err {|Str: Unsupported Z3 relop operator "%a"|} Ty.pp_relop op
        in
        op' e1 e2

      let encode_cvtop op e =
        match op with
        | String_to_code -> Seq.mk_string_to_code ctx e
        | String_from_code -> Seq.mk_string_from_code ctx e
        | _ -> err {|Str: Unsupported Z3 cvtop operator "%a"|} Ty.pp_cvtop op
    end

    module Bv = struct
      open Ty
      open Z3

      let v (type a) (cast : a Ty.cast) (i : a) =
        match cast with
        | C8 -> BitVector.mk_numeral ctx (string_of_int i) 8
        | C32 -> BitVector.mk_numeral ctx (Int32.to_string i) 32
        | C64 -> BitVector.mk_numeral ctx (Int64.to_string i) 64

      let encode_unop op e =
        let op' =
          match op with
          | Not -> BitVector.mk_not ctx
          | Neg -> BitVector.mk_neg ctx
          | _ -> err {|Bv: Unsupported Z3 unary operator "%a"|} Ty.pp_unop op
        in
        op' e

      let encode_binop op e1 e2 =
        let op' =
          match op with
          | Add -> BitVector.mk_add ctx
          | Sub -> BitVector.mk_sub ctx
          | Mul -> BitVector.mk_mul ctx
          | Div -> BitVector.mk_sdiv ctx
          | DivU -> BitVector.mk_udiv ctx
          | And -> BitVector.mk_and ctx
          | Xor -> BitVector.mk_xor ctx
          | Or -> BitVector.mk_or ctx
          | Shl -> BitVector.mk_shl ctx
          | ShrA -> BitVector.mk_ashr ctx
          | ShrL -> BitVector.mk_lshr ctx
          | Rem -> BitVector.mk_srem ctx
          | RemU -> BitVector.mk_urem ctx
          | Rotl -> BitVector.mk_ext_rotate_left ctx
          | Rotr -> BitVector.mk_ext_rotate_right ctx
          | _ -> err {|Bv: Unsupported Z3 binary operator "%a"|} Ty.pp_binop op
        in
        op' e1 e2

      let encode_triop op _ =
        err {|Bv: Unsupported Z3 triop operator "%a"|} Ty.pp_triop op

      let encode_relop op e1 e2 =
        let op' =
          match op with
          | Eq -> Boolean.mk_eq ctx
          | Ne -> fun x1 x2 -> Boolean.mk_eq ctx x1 x2 |> Boolean.mk_not ctx
          | Lt -> BitVector.mk_slt ctx
          | LtU -> BitVector.mk_ult ctx
          | Le -> BitVector.mk_sle ctx
          | LeU -> BitVector.mk_ule ctx
          | Gt -> BitVector.mk_sgt ctx
          | GtU -> BitVector.mk_ugt ctx
          | Ge -> BitVector.mk_sge ctx
          | GeU -> BitVector.mk_uge ctx
        in
        op' e1 e2

      let encode_cvtop sz op e =
        let op' =
          match sz with
          | Ty.S8 -> assert false
          | Ty.S32 -> (
            match op with
            | ExtS n -> BitVector.mk_sign_ext ctx n
            | ExtU n -> BitVector.mk_zero_ext ctx n
            | WrapI64 -> BitVector.mk_extract ctx 31 0
            | TruncSF32 | TruncSF64 ->
              fun f -> FloatingPoint.mk_to_sbv ctx rtz f 32
            | TruncUF32 | TruncUF64 ->
              fun f -> FloatingPoint.mk_to_ubv ctx rtz f 32
            | Reinterpret_float -> FloatingPoint.mk_to_ieee_bv ctx
            | ToBool -> encode_relop Ne (v C32 0l)
            | OfBool -> fun e -> Boolean.mk_ite ctx e (v C32 1l) (v C32 0l)
            | _ -> assert false )
          | Ty.S64 -> (
            match op with
            | ExtS n -> BitVector.mk_sign_ext ctx n
            | ExtU n -> BitVector.mk_zero_ext ctx n
            (* rounding towards zero (aka truncation) *)
            | TruncSF32 | TruncSF64 ->
              fun f -> FloatingPoint.mk_to_sbv ctx rtz f 64
            | TruncUF32 | TruncUF64 ->
              fun f -> FloatingPoint.mk_to_ubv ctx rtz f 64
            | Reinterpret_float -> FloatingPoint.mk_to_ieee_bv ctx
            | ToBool -> encode_relop Ne (v C64 0L)
            | OfBool -> fun e -> Boolean.mk_ite ctx e (v C64 1L) (v C64 0L)
            | WrapI64 | _ -> assert false )
        in
        op' e
    end

    module Fp = struct
      open Z3
      open Ty

      let v (type a) (sz : a Ty.cast) (f : a) =
        match sz with
        | C8 -> err "Unable to create FP numeral using 8 bits"
        | C32 ->
          FloatingPoint.mk_numeral_f ctx (Int32.float_of_bits f) fp32_sort
        | C64 ->
          FloatingPoint.mk_numeral_f ctx (Int64.float_of_bits f) fp64_sort

      let encode_unop op e =
        let op' =
          match op with
          | Neg -> FloatingPoint.mk_neg ctx
          | Abs -> FloatingPoint.mk_abs ctx
          | Sqrt -> FloatingPoint.mk_sqrt ctx rne
          | Is_nan -> FloatingPoint.mk_is_nan ctx
          | Ceil -> FloatingPoint.mk_round_to_integral ctx rtp
          | Floor -> FloatingPoint.mk_round_to_integral ctx rtn
          | Trunc -> FloatingPoint.mk_round_to_integral ctx rtz
          | Nearest -> FloatingPoint.mk_round_to_integral ctx rne
          | _ -> err {|Fp: Unsupported Z3 unary operator "%a"|} Ty.pp_unop op
        in
        op' e

      let encode_binop op e1 e2 =
        let op' =
          match op with
          | Add -> FloatingPoint.mk_add ctx rne
          | Sub -> FloatingPoint.mk_sub ctx rne
          | Mul -> FloatingPoint.mk_mul ctx rne
          | Div -> FloatingPoint.mk_div ctx rne
          | Min -> FloatingPoint.mk_min ctx
          | Max -> FloatingPoint.mk_max ctx
          | Rem -> FloatingPoint.mk_rem ctx
          | _ -> err {|Fp: Unsupported Z3 binop operator "%a"|} Ty.pp_binop op
        in
        op' e1 e2

      let encode_triop op _ =
        err {|Fp: Unsupported Z3 triop operator "%a"|} Ty.pp_triop op

      let encode_relop op e1 e2 =
        let op' =
          match op with
          | Eq -> FloatingPoint.mk_eq ctx
          | Ne ->
            fun x1 x2 -> FloatingPoint.mk_eq ctx x1 x2 |> Boolean.mk_not ctx
          | Lt -> FloatingPoint.mk_lt ctx
          | Le -> FloatingPoint.mk_leq ctx
          | Gt -> FloatingPoint.mk_gt ctx
          | Ge -> FloatingPoint.mk_geq ctx
          | _ -> err {|Fp: Unsupported Z3 relop operator "%a"|} Ty.pp_relop op
        in
        op' e1 e2

      let f322str =
        FuncDecl.mk_func_decl_s ctx "F32ToString" [ fp32_sort ] str_sort

      let str2f32 =
        FuncDecl.mk_func_decl_s ctx "StringToF32" [ str_sort ] fp32_sort

      let f642str =
        FuncDecl.mk_func_decl_s ctx "F64ToString" [ fp64_sort ] str_sort

      let str2f64 =
        FuncDecl.mk_func_decl_s ctx "StringToF64" [ str_sort ] fp64_sort

      let encode_cvtop sz op e =
        let op' =
          match sz with
          | Ty.S8 -> assert false
          | Ty.S32 -> (
            match op with
            | DemoteF64 ->
              fun bv -> FloatingPoint.mk_to_fp_float ctx rne bv fp32_sort
            | ConvertSI32 | ConvertSI64 ->
              fun bv -> FloatingPoint.mk_to_fp_signed ctx rne bv fp32_sort
            | ConvertUI32 | ConvertUI64 ->
              fun bv -> FloatingPoint.mk_to_fp_unsigned ctx rne bv fp32_sort
            | Reinterpret_int ->
              fun bv -> FloatingPoint.mk_to_fp_bv ctx bv fp32_sort
            | ToString -> fun v -> FuncDecl.apply f322str [ v ]
            | OfString -> fun v -> FuncDecl.apply str2f32 [ v ]
            | PromoteF32 | _ -> assert false )
          | Ty.S64 -> (
            match op with
            | PromoteF32 ->
              fun bv -> FloatingPoint.mk_to_fp_float ctx rne bv fp64_sort
            | ConvertSI32 | ConvertSI64 ->
              fun bv -> FloatingPoint.mk_to_fp_signed ctx rne bv fp64_sort
            | ConvertUI32 | ConvertUI64 ->
              fun bv -> FloatingPoint.mk_to_fp_unsigned ctx rne bv fp64_sort
            | Reinterpret_int ->
              fun bv -> FloatingPoint.mk_to_fp_bv ctx bv fp64_sort
            | ToString -> fun v -> FuncDecl.apply f642str [ v ]
            | OfString -> fun v -> FuncDecl.apply str2f64 [ v ]
            | DemoteF64 | _ -> assert false )
        in
        op' e
    end

    let encode_val : Value.t -> Z3.Expr.expr = function
      | True -> Boolean.encode_true ()
      | False -> Boolean.encode_false ()
      | Int v -> I.encode_val v
      | Real v -> Real.encode_val v
      | Str v -> Str.encode_val v
      | Num (I8 x) -> Bv.v C8 x
      | Num (I32 x) -> Bv.v C32 x
      | Num (I64 x) -> Bv.v C64 x
      | Num (F32 x) -> Fp.v C32 x
      | Num (F64 x) -> Fp.v C64 x

    let encode_unop = function
      | Ty.Ty_int -> I.encode_unop
      | Ty.Ty_real -> Real.encode_unop
      | Ty.Ty_bool -> Boolean.encode_unop
      | Ty.Ty_str -> Str.encode_unop
      | Ty.Ty_bitv _ -> Bv.encode_unop
      | Ty.Ty_fp _ -> Fp.encode_unop

    let encode_binop = function
      | Ty.Ty_int -> I.encode_binop
      | Ty.Ty_real -> Real.encode_binop
      | Ty.Ty_bool -> Boolean.encode_binop
      | Ty.Ty_str -> Str.encode_binop
      | Ty.Ty_bitv _ -> Bv.encode_binop
      | Ty.Ty_fp _ -> Fp.encode_binop

    let encode_triop = function
      | Ty.Ty_int -> I.encode_triop
      | Ty.Ty_real -> Real.encode_triop
      | Ty.Ty_bool -> Boolean.encode_triop
      | Ty.Ty_str -> Str.encode_triop
      | Ty.Ty_bitv _ -> Bv.encode_triop
      | Ty.Ty_fp _ -> Fp.encode_triop

    let encode_relop = function
      | Ty.Ty_int -> I.encode_relop
      | Ty.Ty_real -> Real.encode_relop
      | Ty.Ty_bool -> Boolean.encode_relop
      | Ty.Ty_str -> Str.encode_relop
      | Ty.Ty_bitv _ -> Bv.encode_relop
      | Ty.Ty_fp _ -> Fp.encode_relop

    let encode_cvtop = function
      | Ty.Ty_int -> I.encode_cvtop
      | Ty.Ty_real -> Real.encode_cvtop
      | Ty.Ty_bool -> Boolean.encode_cvtop
      | Ty.Ty_str -> Str.encode_cvtop
      | Ty.Ty_bitv sz -> Bv.encode_cvtop sz
      | Ty.Ty_fp sz -> Fp.encode_cvtop sz

    (* let encode_quantifier (t : bool) (vars_list : Symbol.t list) *)
    (*   (body : Z3.Expr.expr) (patterns : Z3.Quantifier.Pattern.pattern list) : *)
    (*   Z3.Expr.expr = *)
    (*   if List.length vars_list > 0 then *)
    (*     let quantified_assertion = *)
    (*       Z3.Quantifier.mk_quantifier_const ctx t *)
    (*         (List.map *)
    (*            (fun s -> *)
    (*              Z3.Expr.mk_const_s ctx (Symbol.to_string s) *)
    (*                (get_sort (Symbol.type_of s)) ) *)
    (*            vars_list ) *)
    (*         body None patterns [] None None *)
    (*     in *)
    (*     let quantified_assertion = *)
    (*       Z3.Quantifier.expr_of_quantifier quantified_assertion *)
    (*     in *)
    (*     let quantified_assertion = Z3.Expr.simplify quantified_assertion None in *)
    (*     quantified_assertion *)
    (*   else body *)

    let rec encode_expr (hte : Expr.t) : expr =
      let open Expr in
      match hte.node with
      | Val v -> encode_val v
      | Ptr (base, offset) ->
        let base' = encode_val (Num (I32 base)) in
        let offset' = encode_expr offset in
        Bv.encode_binop Add base' offset'
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
      | Symbol s ->
        let x = Symbol.name s in
        let ty = Symbol.ty s in
        Z3.Expr.mk_const_s ctx x (get_sort ty)
      | Extract (e, h, l) ->
        let e' = encode_expr e in
        Z3.BitVector.mk_extract ctx ((h * 8) - 1) (l * 8) e'
      | Concat (e1, e2) ->
        let e1' = encode_expr e1
        and e2' = encode_expr e2 in
        Z3.BitVector.mk_concat ctx e1' e2'
    (* | Quantifier (t, vars, body, patterns) -> *)
    (*   let body' = encode_expr body in *)
    (*   let encode_pattern p = *)
    (*     Z3.Quantifier.mk_pattern ctx (List.map encode_expr p) *)
    (*   in *)
    (*   let patterns' = List.map encode_pattern patterns in *)
    (*   let t' = match t with Forall -> true | Exists -> false in *)
    (*   encode_quantifier t' vars body' patterns' *)

    let update_param_value (type a) (param : a Params.param) (value : a) =
      let module P = Z3.Params in
      match param with
      | Params.Timeout ->
        P.update_param_value ctx "timeout" (string_of_int value)
      | Params.Model -> P.update_param_value ctx "model" (string_of_bool value)
      | Params.Unsat_core ->
        P.update_param_value ctx "unsat_core" (string_of_bool value)
      | Params.Ematching ->
        Z3.set_global_param "smt.ematching" (string_of_bool value)

    let interrupt () = Z3.Tactic.interrupt ctx

    let satisfiability =
      let open Mappings_intf in
      function
      | Z3.Solver.SATISFIABLE -> Satisfiable
      | Z3.Solver.UNSATISFIABLE -> Unsatisfiable
      | Z3.Solver.UNKNOWN -> Unknown

    let pp_smt ?status fmt (es : Expr.t list) =
      let st = match status with Some b -> string_of_bool b | None -> "" in
      let es' = List.map encode_expr es in
      Z3.Params.set_print_mode ctx Z3enums.PRINT_SMTLIB2_COMPLIANT;
      Format.fprintf fmt "%s"
        (Z3.SMT.benchmark_to_smtstring ctx "" "" st "" (List.tl es')
           (List.hd es') )

    let pp_entry fmt entry =
      let key = Z3.Statistics.Entry.get_key entry in
      let value = Z3.Statistics.Entry.to_string_value entry in
      Format.fprintf fmt "%s: %s" key value

    module Solver = struct
      let logic_to_string : Solver_intf.logic -> string = function
        | AUFLIA -> "AUFLIA"
        | AUFLIRA -> "AUFLIRA"
        | AUFNIRA -> "AUFNIRA"
        | LIA -> "LIA"
        | LRA -> "LRA"
        | QF_ABV -> "QF_ABV"
        | QF_AUFBV -> "QF_AUFBV"
        | QF_AUFLIA -> "QF_AUFLIA"
        | QF_AX -> "QF_AX"
        | QF_BV -> "QF_BV"
        | QF_BVFP -> "QF_BVFP"
        | QF_IDL -> "QF_IDL"
        | QF_LIA -> "QF_LIA"
        | QF_LRA -> "QF_LRA"
        | QF_NIA -> "QF_NIA"
        | QF_NRA -> "QF_NRA"
        | QF_RDL -> "QF_RDL"
        | QF_UF -> "QF_UF"
        | QF_UFBV -> "QF_UFBV"
        | QF_UFIDL -> "QF_UFIDL"
        | QF_UFLIA -> "QF_UFLIA"
        | QF_UFLRA -> "QF_UFLRA"
        | QF_UFNRA -> "QF_UFNRA"
        | UFLRA -> "UFLRA"
        | UFNIA -> "UFNIA"

      let make ?logic () : solver =
        match logic with
        | Some logic ->
          let logic = Z3.Symbol.mk_string ctx @@ logic_to_string logic in
          Z3.Solver.mk_solver ctx (Some logic)
        | None -> Z3.Solver.mk_simple_solver ctx

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
      let add s es = Z3.Solver.add s (List.map encode_expr es)
      let check s es = Z3.Solver.check s (List.map encode_expr es)
      let model s = Z3.Solver.get_model s

      let pp_statistics fmt solver =
        let module Entry = Z3.Statistics.Entry in
        let stats = Z3.Solver.get_statistics solver in
        let entries = Z3.Statistics.get_entries stats in
        Format.pp_print_list ~pp_sep:Format.pp_print_newline pp_entry fmt
          entries
    end

    module Optimizer = struct
      let make () = Z3.Optimize.mk_opt ctx
      let push o = Z3.Optimize.push o
      let pop o = Z3.Optimize.pop o
      let add o es = Z3.Optimize.add o (List.map encode_expr es)
      let check o = Z3.Optimize.check o
      let model o = Z3.Optimize.get_model o
      let maximize o e = Z3.Optimize.maximize o (encode_expr e)
      let minimize o e = Z3.Optimize.minimize o (encode_expr e)

      let pp_statistics fmt o =
        let module Entry = Z3.Statistics.Entry in
        let stats = Z3.Optimize.get_statistics o in
        let entries = Z3.Statistics.get_entries stats in
        Format.pp_print_list ~pp_sep:Format.pp_print_newline pp_entry fmt
          entries
    end

    let set (s : string) (i : int) (n : char) =
      let bs = Bytes.of_string s in
      Bytes.set bs i n;
      Bytes.to_string bs

    let int64_of_bv (bv : Z3.Expr.expr) : int64 =
      assert (Z3.Expr.is_numeral bv);
      Int64.of_string (set (Z3.Expr.to_string bv) 0 '0')

    let float_of_numeral (fp : Z3.Expr.expr) : float =
      assert (Z3.Expr.is_numeral fp);
      let module Fp = Z3.FloatingPoint in
      if Fp.is_numeral_nan ctx fp then Float.nan
      else if Fp.is_numeral_inf ctx fp then
        if Fp.is_numeral_negative ctx fp then Float.neg_infinity
        else Float.infinity
      else if Fp.is_numeral_zero ctx fp then
        if Fp.is_numeral_negative ctx fp then Float.neg Float.zero
        else Float.zero
      else
        let sort = Z3.Expr.get_sort fp in
        let ebits = Fp.get_ebits ctx sort in
        let sbits = Fp.get_sbits ctx sort in
        let _, sign = Fp.get_numeral_sign ctx fp in
        (* true => biased exponent *)
        let _, exponent = Fp.get_numeral_exponent_int ctx fp true in
        let _, significand = Fp.get_numeral_significand_uint ctx fp in
        let fp_bits =
          Int64.(
            logor
              (logor
                 (shift_left (of_int sign) (ebits + sbits - 1))
                 (shift_left exponent (sbits - 1)) )
              significand )
        in
        match ebits + sbits with
        | 32 -> Int32.float_of_bits @@ Int64.to_int32 fp_bits
        | 64 -> Int64.float_of_bits fp_bits
        | _ -> assert false

    let value (model : Z3.Model.model) (c : Expr.t) : Value.t =
      let open Value in
      (* we have a model with completion => should never be None *)
      let e = Z3.Model.eval model (encode_expr c) true |> Option.get in
      match (Expr.ty c, Z3.Sort.get_sort_kind @@ Z3.Expr.get_sort e) with
      | Ty_int, Z3enums.INT_SORT ->
        Int (Z.to_int @@ Z3.Arithmetic.Integer.get_big_int e)
      | Ty_real, Z3enums.REAL_SORT ->
        Real (Q.to_float @@ Z3.Arithmetic.Real.get_ratio e)
      | Ty_bool, Z3enums.BOOL_SORT -> (
        match Z3.Boolean.get_bool_value e with
        | Z3enums.L_TRUE -> True
        | Z3enums.L_FALSE -> False
        | Z3enums.L_UNDEF ->
          (* It can never be something else *)
          assert false )
      | Ty_str, Z3enums.SEQ_SORT -> Str (Z3.Seq.get_string ctx e)
      | Ty_bitv S8, Z3enums.BV_SORT -> Num (I8 (Int64.to_int (int64_of_bv e)))
      | Ty_bitv S32, Z3enums.BV_SORT ->
        Num (I32 (Int64.to_int32 (int64_of_bv e)))
      | Ty_bitv S64, Z3enums.BV_SORT -> Num (I64 (int64_of_bv e))
      | Ty_fp S32, Z3enums.FLOATING_POINT_SORT ->
        Num (F32 (Int32.bits_of_float @@ float_of_numeral e))
      | Ty_fp S64, Z3enums.FLOATING_POINT_SORT ->
        Num (F64 (Int64.bits_of_float @@ float_of_numeral e))
      | _ -> assert false

    let type_of_sort (sort : Z3.Sort.sort) : Ty.t =
      match Z3.Sort.get_sort_kind sort with
      | Z3enums.INT_SORT -> Ty.Ty_int
      | Z3enums.REAL_SORT -> Ty.Ty_real
      | Z3enums.BOOL_SORT -> Ty.Ty_bool
      | Z3enums.SEQ_SORT -> Ty.Ty_str
      | Z3enums.BV_SORT -> (
        match Z3.BitVector.get_size sort with
        | 8 -> Ty.Ty_bitv S8
        | 32 -> Ty.Ty_bitv S32
        | 64 -> Ty.Ty_bitv S64
        | bits -> err "Unable to recover type of BitVector with %d bits" bits )
      | Z3enums.FLOATING_POINT_SORT ->
        let ebits = Z3.FloatingPoint.get_ebits ctx sort in
        let sbits = Z3.FloatingPoint.get_sbits ctx sort in
        let size = ebits + sbits in
        if size = 32 then Ty.Ty_fp S32
        else if size = 64 then Ty.Ty_fp S64
        else err "Unable to recover type of FP with %d bits" size
      | _ -> assert false

    let symbols_of_model (model : Z3.Model.model) : Symbol.t list =
      List.map
        (fun const ->
          let x = Z3.Symbol.to_string (Z3.FuncDecl.get_name const) in
          let t = type_of_sort (Z3.FuncDecl.get_range const) in
          Symbol.make t x )
        (Z3.Model.get_const_decls model)

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

include Fresh.Make ()
