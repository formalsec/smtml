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
    (** Specifies a timeout in miliseconds for each [check] call *)
  | Model : bool param  (** Turn model production on/off *)
  | Unsat_core : bool param  (** Turn unsatisfiable core on/off *)
  | Ematching : bool param  (** Turn ematching on/off *)
  | Parallel : bool param  (** Turn parallel mode on/off *)
  | Num_threads : int param
    (** Speficied the maximum number of threads to use in parallel mode *)

type t

val default_value : 'a param -> 'a

val default : unit -> t

val ( $ ) : t -> 'a param * 'a -> t

(** [set params p v] updates parameter [p] with value [v] *)
val set : t -> 'a param -> 'a -> t

(** [opt params p v_opt] updates parameter [p] with value [v] if [opt_v] is
    [Some v] *)
val opt : t -> 'a param -> 'a option -> t

(** [get params p] fetches the current value for parameter [p] *)
val get : t -> 'a param -> 'a
