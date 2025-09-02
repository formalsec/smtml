(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by the Smtml programmers *)

let run_and_time_call ~use f =
  let start = Unix.gettimeofday () in
  let result = f () in
  let stop = Unix.gettimeofday () in
  use (stop -. start);
  result

let rec pp_normalize fmt (hte : Expr.t) =
  match Expr.view hte with
  | Val v -> Value.pp fmt v
  | Ptr { base; offset } -> Fmt.pf fmt "(Ptr %a %a)" Bitvector.pp base pp_normalize offset
  | Symbol _ -> Fmt.pf fmt "symbol_"
  | List v -> Fmt.pf fmt "@[<hov 1>[%a]@]" (Fmt.list ~sep:Fmt.comma pp_normalize) v
  | App (s, v) ->
    Fmt.pf fmt "@[<hov 1>(%a@ %a)@]" Symbol.pp s (Fmt.list ~sep:Fmt.comma pp_normalize) v
  | Unop (ty, op, e) ->
    Fmt.pf fmt "@[<hov 1>(%a.%a@ %a)@]" Ty.pp ty Ty.Unop.pp op pp_normalize e
  | Binop (ty, op, e1, e2) ->
    Fmt.pf fmt "@[<hov 1>(%a.%a@ %a@ %a)@]" Ty.pp ty Ty.Binop.pp op pp_normalize e1 pp_normalize e2
  | Triop (ty, op, e1, e2, e3) ->
    Fmt.pf fmt "@[<hov 1>(%a.%a@ %a@ %a@ %a)@]" Ty.pp ty Ty.Triop.pp op pp_normalize e1 pp_normalize
      e2 pp_normalize e3
  | Relop (ty, op, e1, e2) ->
    Fmt.pf fmt "@[<hov 1>(%a.%a@ %a@ %a)@]" Ty.pp ty Ty.Relop.pp op pp_normalize e1 pp_normalize e2
  | Cvtop (ty, op, e) ->
    Fmt.pf fmt "@[<hov 1>(%a.%a@ %a)@]" Ty.pp ty Ty.Cvtop.pp op pp_normalize e
  | Naryop (ty, op, es) ->
    Fmt.pf fmt "@[<hov 1>(%a.%a@ (%a))@]" Ty.pp ty Ty.Naryop.pp op
      (Fmt.list ~sep:Fmt.comma pp_normalize)
      es
  | Extract (e, h, l) -> Fmt.pf fmt "@[<hov 1>(extract@ %a@ %d@ %d)@]" pp_normalize e l h
  | Concat (e1, e2) -> Fmt.pf fmt "@[<hov 1>(++@ %a@ %a)@]" pp_normalize e1 pp_normalize e2
  | Binder (b, vars, e) ->
    Fmt.pf fmt "@[<hov 1>(%a@ (%a)@ %a)@]" Binder.pp b (Fmt.list ~sep:Fmt.sp pp_normalize)
      vars pp_normalize e

let pp_list fmt (es : Expr.t list) = Fmt.list ~sep:Fmt.sp pp_normalize fmt es

let write =
  let log_path : Fpath.t option =
    let env_var = "QUERY_LOG_PATH" in
    match Bos.OS.Env.var env_var with
    | Some p -> Some (Fpath.v p)
    | None -> None
  in
  match log_path with
  | None -> fun _ _ -> ()
  | Some path ->
    let log_entries : (Expr.t list * int64) list ref = ref [] in
    let close () =
      let entries = List.rev !log_entries in
      let bytes = Marshal.to_string entries [] in
      match Bos.OS.File.write path bytes with
      | Ok () -> ()
      | Error (`Msg e) -> Fmt.failwith "Failed to write log: %s" e
    in
    at_exit close;
    Sys.set_signal Sys.sigterm (Sys.Signal_handle (fun _ -> close ()));
    (* write *)
    let mutex = Mutex.create () in
    fun assumptions time ->
      let entry = (assumptions, time) in
      Mutex.protect mutex (fun () -> log_entries := entry :: !log_entries)
