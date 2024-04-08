open Encoding
module Z3 = Optimizer.Z3

let read_int () = Scanf.scanf " %d" (fun x -> x)

let int x = Expr.value (Int x)

let symbol x = Expr.mk_symbol Symbol.(x @: Ty_int)

let ( = ) i1 i2 = Expr.relop Ty_bool Eq i1 i2

let ( >= ) i1 i2 = Expr.relop Ty_int Ge i1 i2

let ( <= ) i1 i2 = Expr.relop Ty_int Le i1 i2

let ( + ) i1 i2 = Expr.binop Ty_int Add i1 i2

let ( * ) i1 i2 = Expr.binop Ty_int Mul i1 i2

let ( && ) b1 b2 = Expr.binop Ty_bool And b1 b2

let sum (lst : Expr.t list) : Expr.t = List.fold_left ( + ) (int 0) lst

type toy =
  { var : Expr.t
  ; profit : Expr.t
  ; prod_capacity : Expr.t
  ; mutable in_packs : Expr.t list
  }

type pack =
  { var : Expr.t
  ; profit : Expr.t
  }

let toy (i : int) : toy =
  let var = symbol ("t" ^ string_of_int i) in
  let profit = int (read_int ()) in
  let prod_capacity = int (read_int ()) in
  { var; profit; prod_capacity; in_packs = [] }

let pack (toys : toy list) (i : int) =
  let var = symbol ("p" ^ string_of_int i) in
  let add_toy_in_pack i =
    let toy = List.nth toys i in
    toy.in_packs <- var :: toy.in_packs
  in
  add_toy_in_pack (read_int () - 1);
  add_toy_in_pack (read_int () - 1);
  add_toy_in_pack (read_int () - 1);
  let profit = int (read_int ()) in
  { var; profit }

let solve (toys : toy list) (packs : pack list) (daily_max : int) =
  let opt = Z3.create () in

  (* C0: Constrain capacity of each toy and pack *)
  (* Pack quantity is non-zero *)
  let zero = int 0 in
  let pack_quantity = List.map (fun (p : pack) -> p.var >= zero) packs in
  (* Toy quantity is non-zero and does not exceed prod_capacity *)
  let toy_quantity =
    List.map
      (fun (t : toy) ->
        t.var >= zero && t.var + sum t.in_packs <= t.prod_capacity )
      toys
  in
  Z3.add opt (pack_quantity @ toy_quantity);

  (* C1: Constrain maximum daily production capacity *)
  let cap =
    sum (List.map (fun (t : toy) -> t.var) toys)
    + sum (List.map (fun (p : pack) -> p.var * int 3) packs)
    <= int daily_max
  in
  Z3.add opt [ cap ];

  (* Objective: maximum profit *)
  let profit = symbol "profit" in
  let objective =
    sum (List.map (fun (t : toy) -> t.var * t.profit) toys)
    + sum (List.map (fun (p : pack) -> p.var * p.profit) packs)
    = profit
  in
  Z3.add opt [ objective ];

  (* Maximise *)
  match Z3.maximize opt profit with
  | Some v -> Format.printf "%a@." Value.pp v
  | None -> Format.printf "Unfeasible@."

let () =
  let num_toys : int = read_int () in
  let num_packs : int = read_int () in
  let max_daily_toys : int = read_int () in
  let toys : toy list = List.init num_toys toy in
  let packs : pack list = List.init num_packs (pack toys) in
  solve toys packs max_daily_toys
