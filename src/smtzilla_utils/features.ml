module FeatMap = struct
  include Map.Make (String)

  let find_def0 k m = match find_opt k m with Some n -> n | None -> 0
end
(* TODO: use ints or an ADT instead of strings for keys, though strings
         give a convenient practicality. *)

type t = int FeatMap.t

let empty = FeatMap.empty

type feat = string

let union m1 m2 =
  FeatMap.union
    (fun key v1 v2 ->
      match key with
      | "depth" -> Some (Int.max v1 v2) (* actually max_depth *)
      | _ -> Some (v1 + v2) )
    m1 m2

let incr_feat key m =
  FeatMap.update key (function None -> Some 1 | Some c -> Some (c + 1)) m

let get_feat key m = FeatMap.find_def0 key m

let add_time time m = FeatMap.add "time" time m

let add_depth depth m = FeatMap.add "depth" depth m

let get_depth m = FeatMap.find_def0 "depth" m

let add_nb_queries nb_exprs = FeatMap.add "nb_queries" nb_exprs

let add_mean_depth mean_depth = FeatMap.add "mean_depth" mean_depth

let rename_depth_to_max_depth m =
  FeatMap.add "max_depth"
    (match FeatMap.find_opt "depth" m with None -> assert false | Some v -> v)
    (FeatMap.remove "depth" m)
