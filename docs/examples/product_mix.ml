open Smtml
module Z3 = Optimizer.Z3

let read_int () = Scanf.scanf " %d" (fun x -> x)

let int x = Typed.Int.v x

let symbol x = Typed.symbol Typed.Types.int x

let ( = ) x y = Typed.Int.eq x y

let ( <= ) x y = Typed.Int.le x y

let ( + ) x y = Typed.Int.add x y

let ( * ) x y = Typed.Int.mul x y

let ( && ) x y = Typed.Bool.and_ x y

let sum (lst : Typed.Int.t list) : Typed.Int.t =
  List.fold_left ( + ) (int 0) lst

type toy =
  { var : Typed.Int.t
  ; profit : Typed.Int.t
  ; prod_capacity : Typed.Int.t
  ; mutable in_packs : Typed.Int.t list
  }

type pack =
  { var : Typed.Int.t
  ; profit : Typed.Int.t
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
  let pack_quantity = List.map (fun (p : pack) -> zero <= p.var) packs in
  (* Toy quantity is non-zero and does not exceed prod_capacity *)
  let toy_quantity =
    List.map
      (fun (t : toy) ->
        zero <= t.var && t.var + sum t.in_packs <= t.prod_capacity )
      toys
  in
  Z3.add opt (List.map Typed.Unsafe.unwrap (pack_quantity @ toy_quantity));

  (* C1: Constrain maximum daily production capacity *)
  let cap =
    sum (List.map (fun (t : toy) -> t.var) toys)
    + sum (List.map (fun (p : pack) -> p.var * int 3) packs)
    <= int daily_max
  in
  Z3.add opt [ Typed.Unsafe.unwrap cap ];

  (* Objective: maximum profit *)
  let profit = symbol "profit" in
  let objective =
    sum (List.map (fun (t : toy) -> t.var * t.profit) toys)
    + sum (List.map (fun (p : pack) -> p.var * p.profit) packs)
    = profit
  in
  Z3.add opt [ Typed.Unsafe.unwrap objective ];

  (* Maximise *)
  match Z3.maximize opt (Typed.Unsafe.unwrap profit) with
  | Some v -> Fmt.pr "%a@." Value.pp v
  | None -> Fmt.pr "Unfeasible@."

let () =
  let num_toys : int = read_int () in
  let num_packs : int = read_int () in
  let max_daily_toys : int = read_int () in
  let toys : toy list = List.init num_toys toy in
  let packs : pack list = List.init num_packs (pack toys) in
  solve toys packs max_daily_toys
