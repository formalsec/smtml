include Mappings_intf

module Make (M : Mappings_intf.M) = struct
  open M
  open Ty
  open Cont

  type 'a with_cont =
    { node : 'a
    ; cont : M.cont
    }

  type model = M.model with_cont

  type solver = M.solver with_cont

  type handle = M.handle

  type optimize = M.optimizer with_cont

  let err = Log.err

  let get_type = function
    | Ty_int -> Types.int
    | Ty_real -> Types.real
    | Ty_bool -> Types.bool
    | Ty_str -> Types.string
    | Ty_bitv 8 -> Types.bitv 8
    | Ty_bitv 32 -> Types.bitv 32
    | Ty_bitv 64 -> Types.bitv 64
    | Ty_fp 32 -> Types.float 8 24
    | Ty_fp 64 -> Types.float 11 53
    | Ty_bitv _ | Ty_fp _ | Ty_list | Ty_array | Ty_tuple -> assert false

  module Bool_impl = struct
    let true_ = true_

    let false_ = false_

    let unop = function
      | Not -> not_
      | op -> err {|Bool: Unsupported Z3 unop operator "%a"|} Ty.pp_unop op

    let binop = function
      | And -> and_
      | Or -> or_
      | Xor -> xor
      | op -> err {|Bool: Unsupported Z3 binop operator "%a"|} Ty.pp_binop op

    let triop = function
      | Ite -> ite
      | op -> err {|Bool: Unsupported Z3 triop operator "%a"|} Ty.pp_triop op

    let relop op e1 e2 =
      match op with
      | Eq -> eq e1 e2
      | Ne -> distinct [ e1; e2 ]
      | _ -> err {|Bool: Unsupported Z3 relop operator "%a"|} Ty.pp_relop op

    let cvtop _op _e = assert false
  end

  module Int_impl = struct
    let v i = int i [@@inline]

    let unop = function
      | Neg -> Int.neg
      | op -> err {|Int: Unsupported unop operator "%a"|} Ty.pp_unop op

    let binop = function
      | Add -> Int.add
      | Sub -> Int.sub
      | Mul -> Int.mul
      | Div -> Int.div
      | Rem -> Int.rem
      | Pow -> Int.pow
      | op -> err {|Int: Unsupported binop operator "%a"|} Ty.pp_binop op

    let relop = function
      | Eq -> eq
      | Ne -> fun e1 e2 -> distinct [ e1; e2 ]
      | Lt -> Int.lt
      | Gt -> Int.gt
      | Le -> Int.le
      | Ge -> Int.ge
      | op -> err {|Int: Unsupported relop operator "%a"|} Ty.pp_relop op

    (* TODO: Uninterpreted cvtops *)
    let cvtop op e =
      match op with
      | ToString -> assert false
      | OfString -> assert false
      | Reinterpret_float -> Real.to_int e
      | op -> err {|Int: Unsupported cvtop operator "%a"|} Ty.pp_cvtop op
  end

  module Real_impl = struct
    let v f = real f [@@inline]

    let unop op e =
      match op with
      | Neg -> Real.neg e
      | Abs ->
        let* zero = real 0. in
        let* e_gt_zero = Real.gt e zero in
        let* neg_e = Real.neg e in
        ite e_gt_zero e neg_e
      | Sqrt -> bind (v 0.5) (Real.pow e)
      | Ceil ->
        let* x_int = Real.to_int e in
        let* x_int_real = Int.to_real x_int in
        let* x_int_real_eq_e = eq x_int_real e in
        let* x_int_add_one = bind (int 1) (Int.add x_int) in
        ite x_int_real_eq_e x_int x_int_add_one
      | Floor -> Real.to_int e
      | Nearest | Is_nan | _ ->
        err {|Real: Unsupported unop operator "%a"|} Ty.pp_unop op

    let binop op e1 e2 =
      match op with
      | Add -> Real.add e1 e2
      | Sub -> Real.sub e1 e2
      | Mul -> Real.mul e1 e2
      | Div -> Real.div e1 e2
      | Pow -> Real.pow e1 e2
      | Min ->
        let* e1_le_e2 = Real.le e1 e2 in
        ite e1_le_e2 e1 e2
      | Max ->
        let* e1_ge_e2 = Real.ge e1 e2 in
        ite e1_ge_e2 e1 e2
      | _ -> err {|Real: Unsupported binop operator "%a"|} Ty.pp_binop op

    let relop op e1 e2 =
      match op with
      | Eq -> eq e1 e2
      | Ne -> distinct [ e1; e2 ]
      | Lt -> Real.lt e1 e2
      | Gt -> Real.gt e1 e2
      | Le -> Real.le e1 e2
      | Ge -> Real.ge e1 e2
      | _ -> err {|Real: Unsupported relop operator "%a"|} Ty.pp_relop op

    (* TODO: Uninterpreted cvtops *)
    let cvtop op e =
      match op with
      | ToString -> assert false
      | OfString -> assert false
      | ConvertUI32 -> assert false
      | Reinterpret_int -> Int.to_real e
      | op -> err {|Real: Unsupported cvtop operator "%a"|} Ty.pp_cvtop op
  end

  module String_impl = struct
    let v s = String.v s [@@inline]

    (* let trim = FuncDecl.mk_func_decl_s ctx "Trim" [ str_sort ] str_sort *)

    let unop = function
      | Seq_length -> String.length
      | Trim ->
        (* FuncDecl.apply trim [ e ] *)
        assert false
      | op -> err {|String: Unsupported unop operator "%a"|} Ty.pp_unop op

    let binop op e1 e2 =
      match op with
      | Seq_at -> String.at e1 ~pos:e2
      | Seq_concat -> String.concat e1 e2
      | _ -> err {|String: Unsupported binop operator "%a"|} Ty.pp_binop op

    let triop op e1 e2 e3 =
      match op with
      | Seq_extract -> String.sub e1 ~pos:e2 ~len:e3
      | _ -> err {|String: Unsupported triop operator "%a"|} Ty.pp_triop op

    let relop op e1 e2 =
      match op with
      | Eq -> eq e1 e2
      | Ne -> distinct [ e1; e2 ]
      | _ -> err {|String: Unsupported relop operator "%a"|} Ty.pp_relop op

    let cvtop = function
      | String_to_code -> String.to_code
      | String_from_code -> String.of_code
      | op -> err {|String: Unsupported cvtop operator "%a"|} Ty.pp_cvtop op
  end

  module type Bitv_sig = sig
    type elt

    val v : elt -> term M.t

    val bitwidth : int

    module Ixx : sig
      val of_int : int -> elt

      val shift_left : elt -> int -> elt
    end
  end

  module Bitv_impl (B : Bitv_sig) = struct
    include B

    (* Stolen from @krtab in OCamlPro/owi #195 *)
    let clz n =
      let rec loop (lb : int) (ub : int) =
        if ub = lb + 1 then v @@ Ixx.of_int (bitwidth - ub)
        else
          let mid = (lb + ub) / 2 in
          let pow_two_mid = Ixx.(shift_left (of_int 1) mid) in
          let* pow_two_mid = v pow_two_mid in
          let* n_lt_pow_two = Bitv.lt_u n pow_two_mid in
          let* left = loop lb mid in
          let* right = loop mid ub in
          ite n_lt_pow_two left right
      in
      let* zero = v (Ixx.of_int 0) in
      let* n_eq_zero = eq n zero in
      let* bitwidth' = v (Ixx.of_int bitwidth) in
      let* right = loop 0 bitwidth in
      ite n_eq_zero bitwidth' right

    (* Stolen from @krtab in OCamlPro/owi #195 *)
    let ctz n =
      let* zero = v (Ixx.of_int 0) in
      let rec loop (lb : int) (ub : int) =
        if ub = lb + 1 then v (Ixx.of_int lb)
        else
          let mid = (lb + ub) / 2 in
          let pow_two_mid = Ixx.(shift_left (of_int 1) mid) in
          let* pow_two_mid = v pow_two_mid in
          let* rem_pow_two = Bitv.rem n pow_two_mid in
          let* rem_pow_eq_zero = eq rem_pow_two zero in
          let* right = loop mid ub in
          let* left = loop lb mid in
          ite rem_pow_eq_zero right left
      in
      let* n_eq_zero = eq n zero in
      let* bitwidth' = v (Ixx.of_int bitwidth) in
      let* right = loop 0 bitwidth in
      ite n_eq_zero bitwidth' right

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
      | Eq -> eq e1 e2
      | Ne -> distinct [ e1; e2 ]
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
      | ExtS n -> Bitv.sign_extend n e
      | ExtU n -> Bitv.zero_extend n e
      | TruncSF32 | TruncSF64 ->
        let* rm = Float.Rounding_mode.rtz in
        Float.to_sbv bitwidth ~rm e
      | TruncUF32 | TruncUF64 ->
        let* rm = Float.Rounding_mode.rtz in
        Float.to_ubv bitwidth ~rm e
      | Reinterpret_float -> Float.to_ieee_bv e
      | ToBool ->
        let* zero = v (Ixx.of_int 0) in
        distinct [ e; zero ]
      | OfBool ->
        let* one = v (Ixx.of_int 1) in
        let* zero = v (Ixx.of_int 0) in
        ite e one zero
      | _ -> assert false
  end

  module I8 = Bitv_impl (struct
    type elt = int

    let v i = Bitv.v (string_of_int i) 8

    let bitwidth = 8

    module Ixx = struct
      let of_int i = i [@@inline]

      let shift_left v i = v lsl i [@@inline]
    end
  end)

  module I32 = Bitv_impl (struct
    type elt = int32

    let v i = Bitv.v (Int32.to_string i) 32

    let bitwidth = 32

    module Ixx = Int32
  end)

  module I64 = Bitv_impl (struct
    type elt = int64

    let v i = Bitv.v (Int64.to_string i) 64

    let bitwidth = 64

    module Ixx = Int64
  end)

  module type Float_sig = sig
    type elt

    val eb : int

    val sb : int

    val v : elt -> term M.t
    (* TODO: *)
    (* val to_string : Z3.FuncDecl.func_decl *)
    (* val of_string : Z3.FuncDecl.func_decl *)
  end

  module Float_impl (F : Float_sig) = struct
    include F

    let unop op e =
      match op with
      | Neg -> Float.neg e
      | Abs -> Float.abs e
      | Sqrt ->
        let* rne = Float.Rounding_mode.rne in
        Float.sqrt ~rm:rne e
      | Is_nan -> Float.is_nan e
      | Ceil ->
        let* rm = Float.Rounding_mode.rtp in
        Float.round_to_integral ~rm e
      | Floor ->
        let* rm = Float.Rounding_mode.rtn in
        Float.round_to_integral ~rm e
      | Trunc ->
        let* rm = Float.Rounding_mode.rtz in
        Float.round_to_integral ~rm e
      | Nearest ->
        let* rm = Float.Rounding_mode.rne in
        Float.round_to_integral ~rm e
      | _ -> err {|Fp: Unsupported Z3 unary operator "%a"|} Ty.pp_unop op

    let binop op e1 e2 =
      match op with
      | Add ->
        let* rm = Float.Rounding_mode.rne in
        Float.add ~rm e1 e2
      | Sub ->
        let* rm = Float.Rounding_mode.rne in
        Float.sub ~rm e1 e2
      | Mul ->
        let* rm = Float.Rounding_mode.rne in
        Float.mul ~rm e1 e2
      | Div ->
        let* rm = Float.Rounding_mode.rne in
        Float.div ~rm e1 e2
      | Min -> Float.min e1 e2
      | Max -> Float.max e1 e2
      | Rem -> Float.rem e1 e2
      | _ -> err {|Fp: Unsupported Z3 binop operator "%a"|} Ty.pp_binop op

    let triop op _ =
      err {|Fp: Unsupported Z3 triop operator "%a"|} Ty.pp_triop op

    let relop op e1 e2 =
      match op with
      | Eq -> Float.eq e1 e2
      | Ne ->
        let* eq_ = Float.eq e1 e2 in
        not_ eq_
      | Lt -> Float.lt e1 e2
      | Le -> Float.le e1 e2
      | Gt -> Float.gt e1 e2
      | Ge -> Float.ge e1 e2
      | _ -> err {|Fp: Unsupported Z3 relop operator "%a"|} Ty.pp_relop op

    let cvtop op e =
      match op with
      | PromoteF32 | DemoteF64 ->
        let* rm = Float.Rounding_mode.rne in
        Float.to_fp eb sb ~rm e
      | ConvertSI32 | ConvertSI64 ->
        let* rm = Float.Rounding_mode.rne in
        Float.sbv_to_fp eb sb ~rm e
      | ConvertUI32 | ConvertUI64 ->
        let* rm = Float.Rounding_mode.rne in
        Float.ubv_to_fp eb sb ~rm e
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

    let v f = Float.v (Int32.float_of_bits f) eb sb

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

    let v f = Float.v (Int64.float_of_bits f) eb sb

    (* TODO: *)
    (* let to_string = *)
    (*   Z3.FuncDecl.mk_func_decl_s ctx "F64ToString" [ fp64_sort ] str_sort *)
    (* let of_string = *)
    (*   Z3.FuncDecl.mk_func_decl_s ctx "StringToF64" [ str_sort ] fp64_sort *)
  end)

  let v : Value.t -> term M.t = function
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
    | Ty.Ty_bitv _ | Ty_fp _ | Ty_list | Ty_array | Ty_tuple -> assert false

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
    | Ty.Ty_bitv _ | Ty_fp _ | Ty_list | Ty_array | Ty_tuple -> assert false

  let triop = function
    | Ty.Ty_int | Ty.Ty_real -> assert false
    | Ty.Ty_bool -> Bool_impl.triop
    | Ty.Ty_str -> String_impl.triop
    | Ty.Ty_bitv 8 -> I8.triop
    | Ty.Ty_bitv 32 -> I32.triop
    | Ty.Ty_bitv 64 -> I64.triop
    | Ty.Ty_fp 32 -> Float32_impl.triop
    | Ty.Ty_fp 64 -> Float64_impl.triop
    | Ty.Ty_bitv _ | Ty_fp _ | Ty_list | Ty_array | Ty_tuple -> assert false

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
    | Ty.Ty_bitv _ | Ty_fp _ | Ty_list | Ty_array | Ty_tuple -> assert false

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
    | Ty.Ty_bitv _ | Ty_fp _ | Ty_list | Ty_array | Ty_tuple -> assert false

  let rec encode_expr (hte : Expr.t) : term M.t =
    match Expr.view hte with
    | Val value -> v value
    | Ptr (base, offset) ->
      let* base' = v (Num (I32 base)) in
      let* offset' = encode_expr offset in
      I32.binop Add base' offset'
    | Symbol s ->
      let x = Symbol.to_string s in
      let* ty = get_type @@ Symbol.type_of s in
      const x ty
    | Unop (ty, op, e) ->
      let* e = encode_expr e in
      unop ty op e
    | Binop (ty, op, e1, e2) ->
      let* e1 = encode_expr e1 in
      let* e2 = encode_expr e2 in
      binop ty op e1 e2
    | Triop (ty, op, e1, e2, e3) ->
      let* e1 = encode_expr e1 in
      let* e2 = encode_expr e2 in
      let* e3 = encode_expr e3 in
      triop ty op e1 e2 e3
    | Relop (ty, op, e1, e2) ->
      let* e1 = encode_expr e1 in
      let* e2 = encode_expr e2 in
      relop ty op e1 e2
    | Cvtop (ty, op, e) ->
      let* e = encode_expr e in
      cvtop ty op e
    | Extract (e, h, l) ->
      let* e = encode_expr e in
      M.Bitv.extract e ~high:((h * 8) - 1) ~low:(l * 8)
    | Concat (e1, e2) ->
      let* e1 = encode_expr e1 in
      let* e2 = encode_expr e2 in
      M.Bitv.concat e1 e2
    | List _ | Array _ | Tuple _ | App _ -> assert false

  (* TODO: pp_smt *)
  let pp_smt ?status:_ _ _ = assert false

  let value ({ node = m; cont } : model) (c : Expr.t) : Value.t =
    let open M in
    let v =
      let* term = encode_expr c in
      let v = Model.eval ~completion:true m term |> Option.get in
      match Expr.ty c with
      | Ty_int -> M.Cont.return @@ Value.Int (Interp.to_int v)
      | Ty_real -> M.Cont.return @@ Value.Real (Interp.to_real v)
      | Ty_bool ->
        M.Cont.return @@ if Interp.to_bool v then Value.True else Value.False
      | Ty_str ->
        let+ str = Interp.to_string v in
        Value.Str str
      | Ty_bitv 8 ->
        let i8 = Interp.to_bitv v 8 in
        M.Cont.return @@ Value.Num (I8 (Int64.to_int i8))
      | Ty_bitv 32 ->
        let i32 = Interp.to_bitv v 32 in
        M.Cont.return @@ Value.Num (I32 (Int64.to_int32 i32))
      | Ty_bitv 64 ->
        let i64 = Interp.to_bitv v 64 in
        M.Cont.return @@ Value.Num (I64 i64)
      | Ty_fp 32 ->
        let+ float = Interp.to_float v 8 24 in
        Value.Num (F32 (Int32.bits_of_float float))
      | Ty_fp 64 ->
        let+ float = Interp.to_float v 11 53 in
        Value.Num (F64 (Int64.bits_of_float float))
      | Ty_bitv _ | Ty_fp _ | Ty_list | Ty_array | Ty_tuple -> assert false
    in
    Cont.run v cont

  let values_of_model ?symbols ({ node = model; cont } as model') =
    let m = Hashtbl.create 512 in
    let symbols =
      match symbols with
      | None -> Cont.run (Model.get_symbols model) cont
      | Some symbols -> symbols
    in
    List.iter
      (fun sym ->
        let v = value model' (Expr.mk_symbol sym) in
        Hashtbl.replace m sym v )
      symbols;
    m

  let set_debug _ = ()

  module Solver = struct
    let make ?params ?logic () =
      let cont = make_cont () in
      let solver = Cont.run (Solver.make ?params ?logic ()) cont in
      { node = solver; cont }

    let clone { node = solver; cont } =
      let node = Cont.run (Solver.clone solver) cont in
      { node; cont }

    let push { node = solver; _ } = Solver.push solver

    let pop { node = solver; _ } n = Solver.pop solver n

    let reset { node = solver; _ } = Solver.reset solver

    let add { node = solver; cont } (exprs : Expr.t list) =
      let exprs = List.map (fun e -> Cont.run (encode_expr e) cont) exprs in
      Solver.add solver exprs

    let check { node = solver; cont } ~assumptions =
      let assumptions =
        List.map (fun e -> Cont.run (encode_expr e) cont) assumptions
      in
      Solver.check solver ~assumptions

    let model { node = solver; cont } =
      Option.map (fun node -> { node; cont }) (Solver.model solver)

    let add_simplifier { node = solver; cont } =
      { node = Cont.run (Solver.add_simplifier solver) cont; cont }

    let interrupt { cont; _ } = Cont.run (Solver.interrupt ()) cont

    let pp_statistics fmt { node = solver; _ } = Solver.pp_statistics fmt solver
  end

  module Optimizer = struct
    let make () =
      let cont = make_cont () in
      let opt = Cont.run (Optimizer.make ()) cont in
      { node = opt; cont }

    let push { node = opt; _ } = Optimizer.push opt

    let pop { node = opt; _ } = Optimizer.pop opt

    let add { node = opt; cont } (exprs : Expr.t list) =
      let unit =
        let+ terms =
          List.fold_left
            (fun acc e ->
              let* acc in
              let+ term = encode_expr e in
              term :: acc )
            (Cont.return []) exprs
        in
        Optimizer.add opt terms
      in
      Cont.run unit cont

    let check { node = opt; _ } = Optimizer.check opt

    let model { node = opt; cont } =
      Option.map (fun node -> { node; cont }) (Optimizer.model opt)

    let maximize { node = opt; cont } (expr : Expr.t) =
      let handle =
        let+ term = encode_expr expr in
        Optimizer.maximize opt term
      in
      Cont.run handle cont

    let minimize { node = opt; cont } (expr : Expr.t) =
      let handle =
        let+ expr = encode_expr expr in
        Optimizer.minimize opt expr
      in
      Cont.run handle cont

    let interrupt { cont; _ } = Cont.run (Optimizer.interrupt ()) cont

    let pp_statistics fmt { node = opt; _ } = Optimizer.pp_statistics fmt opt
  end
end

module Make' (M : Mappings_intf.M) : S = Make (M)
