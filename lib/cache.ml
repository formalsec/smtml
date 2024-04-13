include Cache_intf

module Strong : S = struct
  include Hashtbl.Make (struct
    type t = Expr.t list

    let equal es1 es2 = List.equal Expr.equal es1 es2

    let hash es = List.fold_left (fun acc e -> acc lxor Expr.hash e) 0 es
  end)
end
