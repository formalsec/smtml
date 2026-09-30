open Crowbar
open Crowbar.Syntax

type t =
  | ALL  (** The logic that encompasses all theories. *)
  | QF_BV  (** Quantifier-free bitvector theory. *)
  | QF_BVFP  (** Quantifier-free bitvectors and floating-point arithmetic. *)
  | QF_FP  (** Quantifier-free floating-point arithmetic. *)
  | QF_LIA  (** Quantifier-free linear integer arithmetic. *)
  | ONLY_BOOL
  | LIA

let get_theory () =
  let+ i = range 7 in
  match i with
  | 0 -> ALL
  | 1 -> QF_BV
  | 2 -> QF_BVFP
  | 3 -> QF_FP
  | 4 -> QF_LIA
  | 5 -> ONLY_BOOL
  | 6 -> LIA
  | _ -> assert false

let has_bv t = match t with ALL | QF_BV | QF_BVFP -> true | _ -> false

let has_fp t = match t with ALL | QF_BVFP | QF_FP -> true | _ -> false

let has_qf_lia t = match t with ALL | QF_LIA -> true | _ -> false

let has_all t = match t with ALL -> true | _ -> false

let has_lia t = match t with ALL | LIA -> true | _ -> false

let print_theory = function
  | ALL -> Logs.app (fun m -> m "ALL...")
  | QF_BV -> Logs.app (fun m -> m "QF_BV...")
  | QF_BVFP -> Logs.app (fun m -> m "QF_BVFP...")
  | QF_FP -> Logs.app (fun m -> m "QF_FP...")
  | QF_LIA -> Logs.app (fun m -> m "QF_LIA...")
  | ONLY_BOOL -> Logs.app (fun m -> m "ONLY_BOOL...")
  | LIA -> Logs.app (fun m -> m "LIA...")
