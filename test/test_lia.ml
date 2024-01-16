open Encoding
open Ty
open Expr
module Solver = Solver.Z3_batch
module Optimizer = Optimizer.Z3

let v i = Val (Int i) @: Ty_int
let eq i1 i2 = Relop (Eq, i1, i2) @: Ty_int
let ne i1 i2 = Relop (Ne, i1, i2) @: Ty_int
let ( < ) i1 i2 = Relop (Lt, i1, i2) @: Ty_int
let ( <= ) i1 i2 = Relop (Le, i1, i2) @: Ty_int
let ( > ) i1 i2 = Relop (Gt, i1, i2) @: Ty_int
let ( >= ) i1 i2 = Relop (Ge, i1, i2) @: Ty_int
let ( + ) i1 i2 = Binop (Add, i1, i2) @: Ty_int
let ( - ) i1 i2 = Binop (Sub, i1, i2) @: Ty_int
let ( * ) i1 i2 = Binop (Mul, i1, i2) @: Ty_int
let ( / ) i1 i2 = Binop (Div, i1, i2) @: Ty_int
let rem i1 i2 = Binop (Rem, i1, i2) @: Ty_int
let ( ** ) i1 i2 = Binop (Pow, i1, i2) @: Ty_int
let neg i = Unop (Neg, i) @: Ty_int

let () =
  let solver = Solver.create ~logic:LIA () in
  let x = mk_symbol Symbol.("x" @: Ty_int) in
  Solver.add solver [ x > v 0; x <= v 10 ];
  assert (Solver.check solver [ ne x (v 0) ]);
  let y = mk_symbol Symbol.("y" @: Ty_int) in
  assert (Solver.check solver [ eq (x + y - y) x ]);
  assert (Solver.check solver [ eq (x * y / y) x ]);
  assert (Solver.check solver [ rem y x < v 10 ]);
  assert (Solver.check solver [ eq ((x ** v 2) / x) (x * x / x) ]);
  let z = mk_symbol Symbol.("z" @: Ty_int) in
  Solver.add solver [ z > v 0 ];
  assert (Solver.check solver [ neg z < v 0 ])

let () =
  let solver = Solver.create ~logic:LIA () in
  let symbol_x = Symbol.("x" @: Ty_int) in
  let x = mk_symbol symbol_x in
  assert (Solver.check solver []);
  assert (Solver.check solver [ x >= v 0 ]);
  assert ({ e = Val (Int 0); ty = Ty_int } = Solver.get_value solver x);
  assert (Solver.check solver [ eq x (v 3) ]);
  assert ({ e = Val (Int 9); ty = Ty_int } = Solver.get_value solver (x * x));
  assert (Solver.check solver []);
  let model = Solver.model ~symbols:[ symbol_x ] solver in
  let val_x = Option.bind model (fun m -> Model.evaluate m symbol_x) in
  assert (Option.is_some val_x);
  assert (Solver.check solver [ eq x (v 5) ]);
  let model = Solver.model solver in
  let val_x = Option.bind model (fun m -> Model.evaluate m symbol_x) in
  assert (Some (Value.Int 5) = val_x)

let () =
  let opt = Optimizer.create () in
  let x = mk_symbol Symbol.("x" @: Ty_int) in
  Optimizer.add opt [ x >= v 0; x < v 5 ];
  Optimizer.push opt;
  Optimizer.push opt;
  assert (Some (Value.Int 0) = Optimizer.minimize opt x);
  Optimizer.pop opt;
  assert (Some (Value.Int 4) = Optimizer.maximize opt x);
  let model = Optimizer.model opt in
  Option.iter (Model.pp Format.std_formatter) model
