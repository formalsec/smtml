let solver_time = ref 0.0

let time_call ~f ~accum =
  let start = Stdlib.Sys.time () in
  let ret = f () in
  accum := !accum +. (Stdlib.Sys.time () -. start);
  ret

let ( let+ ) o f = Option.map f o

module Make (M : Mappings_intf.S) = struct
  module O = M.Optimizer

  type t = M.optimize

  let create () : t = O.make ()
  let push (opt : t) : unit = O.push opt
  let pop (opt : t) : unit = O.pop opt
  let add (opt : t) (es : Expr.t list) : unit = O.add opt es

  let check (opt : t) =
    M.satisfiability (time_call ~f:(fun () -> O.check opt) ~accum:solver_time)

  let model opt =
    let+ model = O.model opt in
    M.values_of_model model

  let maximize (opt : t) (e : Expr.t) : Value.t option =
    ignore @@ O.maximize opt e;
    match check opt with
    | Mappings_intf.Satisfiable ->
      let+ model = O.model opt in
      M.value model e
    | _ -> None

  let minimize (opt : t) (e : Expr.t) : Value.t option =
    ignore @@ O.minimize opt e;
    match check opt with
    | Mappings_intf.Satisfiable ->
      let+ model = O.model opt in
      M.value model e
    | _ -> None
end

module Z3 = Make (Z3_mappings)
