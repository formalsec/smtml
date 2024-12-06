(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by the Smtml programmers *)

type t

type _ param =
  | Timeout : int param
    (** Specifies a timeout in miliseconds for each [check] call *)
  | Model : bool param  (** Turn model production on/off *)
  | Unsat_core : bool param  (** Turn unsatisfiable core on/off *)
  | Ematching : bool param  (** Turn ematching on/off *)
  | Parallel : bool param  (** Turn parallel mode on/off *)
  | Num_threads : int param
    (** Speficied the maximum number of threads to use in parallel mode *)

type param' = P : 'a param * 'a -> param'

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

val to_list : t -> param' list
