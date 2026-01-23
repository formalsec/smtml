(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by the Smtml programmers *)

include Prelude

module String = struct
  include String

  let hash x = String_hash.seeded_hash 0 x
end

module Int32 = struct
  include Int32

  (* As they are defined in the stdlib *)
  let hash x = Hashtbl.seeded_hash_param 10 100 0 x
end

module Int64 = struct
  include Int64

  (* As they are defined in the stdlib *)
  let hash x = Hashtbl.seeded_hash_param 10 100 0 x
end

module Option = struct
  include Option

  module Syntax = struct
    let ( let* ) v f = bind v f

    let ( let+ ) v f = map f v
  end
end

module Result = struct
  include Result

  module Syntax = struct
    let ( let* ) v f = Result.bind v f

    let ( let+ ) v f = Result.map f v
  end

  let rec list_iter f =
    let open Syntax in
    function
    | [] -> Ok ()
    | hd :: tl ->
      let* () = f hd in
      list_iter f tl

  let list_map f v =
    let open Syntax in
    let rec list_map_cps f v k =
      match v with
      | [] -> k (Ok [])
      | hd :: tl ->
        list_map_cps f tl (fun rest ->
          let* rest in
          let* hd' = f hd in
          k (Ok (hd' :: rest)) )
    in
    list_map_cps f v Fun.id

  let list_filter_map f v =
    let open Syntax in
    let rec list_filter_map_cps f v k =
      match v with
      | [] -> k (Ok [])
      | hd :: tl ->
        list_filter_map_cps f tl (fun rest ->
          let* rest in
          let* v = f hd in
          k (Ok (match v with None -> rest | Some v -> v :: rest)) )
    in
    list_filter_map_cps f v Fun.id
end
