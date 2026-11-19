open Crowbar
open Crowbar.Syntax

(* Basics *)
let symbol_bv32_ (state : State.t) =
  let+ symbol = state.bv32_names in
  Smtml.Typed.Bitv32.symbol symbol

let symbol_bool (state : State.t) =
  let+ symbol = state.bool_names in
  Smtml.Typed.Bool.symbol symbol

let gen_float_nbr =
  let+ v = float in
  Smtml.Typed.Float32.of_float v

let symbol_f32 (state : State.t) =
  let+ symbol = state.float32_names in
  Smtml.Typed.Float32.symbol symbol

let symbol_int (state : State.t) =
  let+ symbol = state.int_names in
  Logs.debug (fun m -> m "symbol int");
  Smtml.Typed.Int.symbol symbol

let gen_int_nbr =
  let+ v = int in
  Logs.debug (fun m -> m "gen_int_nbr");
  Smtml.Typed.Int.v (Z.of_int v)

let symbol_f64 (state : State.t) =
  let+ symbol = state.float64_names in
  Smtml.Typed.Float64.symbol symbol

let gen_float64_nbr =
  let+ v = float in
  Smtml.Typed.Float64.of_float v

let gen_bv_nbr =
  let+ v = int32 in
  Smtml.Typed.Bitv32.of_int32 v

type symbol_type =
  | Bool
  | Bv32
  | F32
  | F64
  | Int

let gen_symbol_type theory =
  match theory with
  | Theory.LIA -> choose [ const Int ]
  | _ -> choose [ const Bool; const Bv32; const F32; const F64; const Int ]

(* Integers *)

let int_expr (state : State.t) =
  fix (fun int_expr ->
    (* Logs.debug (fun m -> m "int_expr"); *)
    let int_bin_expr =
      let* op = Op.binop_int in
      Logs.debug (fun m -> m "int bin expr");
      let+ left = int_expr
      and+ right = int_expr in
      (* Logs.debug (fun m -> m "int_bin_expr"); *)
      op left right
    in

    let int_unop_expr =
      let+ op = Op.unop_int
      and+ expr = int_expr in
      Logs.debug (fun m -> m "int_unop_expr");
      op expr
    in

    choose_
      [ (const (Smtml.Typed.Int.v (Z.of_int 42)), 1)
      ; (int_bin_expr, 100)
      ; (int_unop_expr, 90)
      ; (symbol_int state, 5)
      ; (gen_int_nbr, 2)
      ] )

(* BV32 *)
let rec bv32_expr (state : State.t) =
  fix (fun bv32_expr ->
    (* Logs.debug (fun m -> m "bv32_expr"); *)
    let bv32_bin_expr =
      let+ op = Op.bv32_binop
      and+ left = bv32_expr
      and+ right = bv32_expr in
      op left right
    in

    let bv32_unop_expr =
      let+ op = Op.bv32_unop
      and+ expr = bv32_expr in
      op expr
    in

    let gen_rotate =
      let+ op = Op.rotate
      and+ left = int32
      and+ right = bv32_expr in
      let left = Int32.to_int left in
      op left right
    in

    let float32_tobv32 =
      let op = Smtml.Typed.Float32.to_bv in
      let+ expr = float32_expr_convert state in
      op expr
    in
    let expr_allows_bv32 state =
      let l =
        [ const (Smtml.Typed.Bitv32.of_int32 42l)
        ; bv32_bin_expr
        ; bv32_unop_expr
        ; symbol_bv32_ state
        ; gen_bv_nbr
        ; gen_rotate
        ]
      in
      (*TODO float32_tobv32 :: l *)
      if Theory.has_fp state.theory then l @ [ float32_tobv32 ] else l
    in
    choose (expr_allows_bv32 state) )

and float32_expr_convert (state : State.t) =
  fix (fun float32_expr ->
    (* Logs.debug (fun m -> m "float32_expr"); *)
    let float32_binexpr =
      let+ op = Op.binop_float32
      and+ left = float32_expr
      and+ right = float32_expr in
      op left right
    in

    let float32_unop_expr =
      let+ op = Op.unop_float32
      and+ expr = float32_expr in
      op expr
    in
    choose
      [ const (Smtml.Typed.Float32.of_float 42.)
      ; gen_float_nbr
      ; symbol_f32 state
      ; float32_binexpr
      ; float32_unop_expr
      ] )

(* F32 *)
and float32_expr (state : State.t) =
  fix (fun float32_expr ->
    (* Logs.debug (fun m -> m "float32_expr"); *)
    let float32_binexpr =
      let+ op = Op.binop_float32
      and+ left = float32_expr
      and+ right = float32_expr in
      op left right
    in

    let float32_unop_expr =
      let+ op = Op.unop_float32
      and+ expr = float32_expr in
      op expr
    in

    let bv32expr_to_float32 =
      let+ op = Op.float32_convert
      and+ expr = bv32_expr state in
      op expr
    in
    let expr_allows_float32 state =
      let l =
        [ const (Smtml.Typed.Float32.of_float 42.)
        ; gen_float_nbr
        ; symbol_f32 state
        ; float32_binexpr
        ; float32_unop_expr
        ]
      in
      if Theory.has_bv state.theory then l @ [ bv32expr_to_float32 ] else l
    in
    choose (expr_allows_float32 state) )

(* F64 *)
let float64_expr (state : State.t) =
  fix (fun float64_expr ->
    (* Logs.debug (fun m -> m "float64_expr..."); *)
    let float64_binexpr =
      let+ op = Op.binop_float64
      and+ left = float64_expr
      and+ right = float64_expr in
      op left right
    in

    let float64_unop_expr =
      let+ op = Op.unop_float64
      and+ expr = float64_expr in
      op expr
    in

    let float32_to_float64 =
      let op = Smtml.Typed.Float64.promote_f32 in
      let+ expr = float32_expr state in
      op expr
    in

    let bv32_to_float64 =
      let+ op = Op.bv32_to_float64
      and+ expr = bv32_expr state in
      op expr
    in
    let expr_allows_float64 state =
      let l =
        [ const (Smtml.Typed.Float64.of_float 42.)
        ; gen_float64_nbr
        ; symbol_f64 state
        ; float64_binexpr
        ; float64_unop_expr
        ; float32_to_float64
        ]
      in
      if Theory.has_bv state.theory then l @ [ bv32_to_float64 ] else l
    in
    choose (expr_allows_float64 state) )

let rec boolean_expr (state : State.t) =
  fix (fun boolean_expr ->
    Logs.debug (fun m -> m "boolean_expr...");
    (* Booleans *)
    let bool_bin_expr =
      let+ op = Op.bool_binop
      and+ left = boolean_expr
      and+ right = boolean_expr in
      op left right
    in

    let bool_unop_expr =
      let+ op = Op.bool_unop
      and+ expr = boolean_expr in
      op expr
    in

    (* Mixing things *)
    let int_bool_bin =
      let+ op = Op.int_bool_bin
      and+ left = int_expr state
      and+ right = int_expr state in
      op left right
    in

    let gen_bv32_expr =
      let+ op = Op.bv32_bool_bin
      and+ left = bv32_expr state
      and+ right = bv32_expr state in
      op left right
    in

    let float32_bool_bin =
      let+ op = Op.float32_bool_bin
      and+ left = float32_expr state
      and+ right = float32_expr state in
      op left right
    in

    let float32_bool_unop =
      let+ op = Op.float32_bool_unop
      and+ expr = float32_expr state in
      op expr
    in

    let float64_bool_bin =
      let+ op = Op.float64_bool_bin
      and+ left = float64_expr state
      and+ right = float64_expr state in
      op left right
    in

    let float64_bool_unop =
      let+ op = Op.float64_bool_unop
      and+ expr = float64_expr state in
      op expr
    in

    let get_expr_allows state =
      let l =
        [ (const Smtml.Typed.Bool.true_, 0)
        ; (const Smtml.Typed.Bool.false_, 0)
        ; (bool_bin_expr, 100)
        ; (bool_unop_expr, 100)
        ; (symbol_bool state, 5 (* ; gen_let_in state *))
        ]
      in
      if Theory.has_bv state.theory then l @ [ (gen_bv32_expr, 10) ]
      else if Theory.has_fp state.theory then
        l
        @ [ (float32_bool_bin, 10)
          ; (float32_bool_unop, 5)
          ; (float64_bool_bin, 10)
          ; (float64_bool_unop, 5)
          ]
      else if Theory.has_qf_lia state.theory then l @ [ (int_bool_bin, 100) ]
      else if Theory.has_all state.theory then
        l
        @ [ (gen_bv32_expr, 10)
          ; (float32_bool_bin, 10)
          ; (float32_bool_unop, 5)
          ; (int_bool_bin, 10)
          ; (float64_bool_bin, 10)
          ; (float64_bool_unop, 5)
          ; (gen_forall state, 5)
          ; (gen_exist state, 5)
          ]
      else if Theory.has_bv state.theory && Theory.has_fp state.theory then
        l
        @ [ (gen_bv32_expr, 10)
          ; (float32_bool_bin, 10)
          ; (float32_bool_unop, 5)
          ; (float64_bool_bin, 5)
          ; (float64_bool_unop, 5)
          ]
      else if Theory.has_lia state.theory then
        l @ [ (int_bool_bin, 10); (gen_forall state, 5); (gen_exist state, 5) ]
      else l
    in
    choose_ (get_expr_allows state) )

and gen_quantifier f symbol_type state =
  let* x =
    match symbol_type with
    | Bool ->
      let* s = symbol_bool state in
      const (Smtml.Typed.Unsafe.unwrap s)
    | Bv32 ->
      let* s = symbol_bv32_ state in
      const (Smtml.Typed.Unsafe.unwrap s)
    | F32 ->
      let* s = symbol_f32 state in
      const (Smtml.Typed.Unsafe.unwrap s)
    | F64 ->
      let* s = symbol_f64 state in
      const (Smtml.Typed.Unsafe.unwrap s)
    | Int ->
      let* s = symbol_int state in
      const (Smtml.Typed.Unsafe.unwrap s)
  in
  let* expr = boolean_expr state in
  let expr = f [ x ] (Smtml.Typed.Unsafe.unwrap expr) in
  let expr = Smtml.Typed.Unsafe.wrap expr in
  const expr

and gen_forall state =
  let* symbol_type = gen_symbol_type state.theory in
  gen_quantifier Smtml.Expr.forall symbol_type state

and gen_exist state =
  let* symbol_type = gen_symbol_type state.theory in
  gen_quantifier Smtml.Expr.exists symbol_type state

(* and gen_let_in state =
  let* symbol_type =
    choose [ const Bool; const Bv32; const F32; const F64; const Int ]
  in
  gen_quantifier Smtml.Expr.let_in symbol_type state *)

let expr theory =
  let open State in
  let* state = state theory in
  Logs.debug (fun m -> m "generating expr");
  try with_printer Smtml.Typed.Bool.pp (boolean_expr state)
  with Division_by_zero ->
    Logs.warn (fun m -> m "BAD TEST (division by zero)");
    Crowbar.bad_test ()

let expr_list theory =
  let open State in
  let* state = state theory in
  let expr_list = list (boolean_expr state) in
  try
    with_printer
      (fun fmt expr_list ->
        Fmt.pf fmt "%a" (Fmt.list ~sep:Fmt.semi Smtml.Typed.Bool.pp) expr_list )
      expr_list
  with Division_by_zero ->
    Logs.warn (fun m -> m "BAD TEST (division by zero)");
    Crowbar.bad_test ()
