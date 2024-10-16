(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by the Smtml programmers *)

include Optimizer_intf

let solver_time = ref 0.0

let ( let+ ) o f = Option.map f o

module Make (M : Mappings_intf.S) = struct
  module O = M.Optimizer

  type t = M.optimize

  let create () : t = O.make ()

  let push (opt : t) : unit = O.push opt

  let pop (opt : t) : unit = O.pop opt

  let add (opt : t) (es : Expr.t list) : unit = O.add opt es

  let protect (opt : t) (f : unit -> 'a) : 'a =
    push opt;
    let result = f () in
    pop opt;
    result

  let check (opt : t) =
    Utils.run_and_time_call
      ~use:(fun time -> solver_time := !solver_time +. time)
      (fun () -> O.check opt)

  let model opt =
    let+ model = O.model opt in
    M.values_of_model model

  let maximize (opt : t) (e : Expr.t) : Value.t option =
    let _ : M.handle = O.maximize opt e in
    match check opt with
    | `Sat ->
      let+ model = O.model opt in
      M.value model e
    | _ -> None

  let minimize (opt : t) (e : Expr.t) : Value.t option =
    let _ : M.handle = O.minimize opt e in
    match check opt with
    | `Sat ->
      let+ model = O.model opt in
      M.value model e
    | _ -> None

  let get_statistics (opt : t) : Statistics.t = M.Optimizer.get_statistics opt
end

module Z3 = Make (Z3_mappings)
