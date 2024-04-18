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

let solver_time = ref 0.0

let ( let+ ) o f = Option.map f o

module Make (M : Mappings_intf.S) = struct
  module O = M.Optimizer

  type t = M.optimize

  let create () : t = O.make ()

  let push (opt : t) : unit = O.push opt

  let pop (opt : t) : unit = O.pop opt

  let add (opt : t) (es : Expr.t list) : unit = O.add opt es

  let check (opt : t) =
    Utils.run_and_time_call
      ~use:(fun time -> solver_time := !solver_time +. time)
      (fun () -> O.check opt)

  let model opt =
    let+ model = O.model opt in
    M.values_of_model model

  let maximize (opt : t) (e : Expr.t) : Value.t option =
    ignore @@ O.maximize opt e;
    match check opt with
    | `Sat ->
      let+ model = O.model opt in
      M.value model e
    | _ -> None

  let minimize (opt : t) (e : Expr.t) : Value.t option =
    ignore @@ O.minimize opt e;
    match check opt with
    | `Sat ->
      let+ model = O.model opt in
      M.value model e
    | _ -> None
end

module Z3 = Make (Z3_mappings)
