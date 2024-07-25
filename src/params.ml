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

type _ param =
  | Timeout : int param
  | Model : bool param
  | Unsat_core : bool param
  | Ematching : bool param

type t =
  { timeout : int
  ; model : bool
  ; unsat_core : bool
  ; ematching : bool
  }

let default_timeout = 2147483647

let default_model = true

let default_unsat_core = false

let default_ematching = true

let default_value (type a) (param : a param) : a =
  match param with
  | Timeout -> default_timeout
  | Model -> default_model
  | Unsat_core -> default_unsat_core
  | Ematching -> default_ematching

let default () =
  { timeout = default_timeout
  ; model = default_model
  ; unsat_core = default_unsat_core
  ; ematching = default_ematching
  }

let set (type a) (params : t) (param : a param) (value : a) : t =
  match param with
  | Timeout -> { params with timeout = value }
  | Model -> { params with model = value }
  | Unsat_core -> { params with unsat_core = value }
  | Ematching -> { params with ematching = value }

let ( $ ) (type a) (params : t) ((param, value) : a param * a) : t =
  set params param value

let get (type a) (params : t) (param : a param) : a =
  match param with
  | Timeout -> params.timeout
  | Model -> params.model
  | Unsat_core -> params.unsat_core
  | Ematching -> params.ematching
