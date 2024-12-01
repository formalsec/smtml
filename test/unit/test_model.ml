open Smtml

(* Test Model.to_json *)
let () =
  let x = Symbol.make Ty_int "x" in
  let y = Symbol.make Ty_real "y" in
  let z = Symbol.make Ty_bool "z" in
  let u = Symbol.make Ty_str "u" in
  let model : Model.t =
    let tbl = Hashtbl.create 16 in
    List.iter
      (fun ((s, v) : Symbol.t * Value.t) -> Hashtbl.replace tbl s v)
      [ (x, Int 1); (y, Real 2.0); (z, True); (u, Str "abc") ];
    tbl
  in
  let model_to_json = Model.to_json model in
  Format.printf "%a@." (Yojson.pretty_print ~std:true) model_to_json

(* Parsing *)

(* scfg *)
let () =
  let open Result in
  let model_str =
    {|
      model {
        symbol x_0 int 42
        symbol x_1 bool true
        symbol x_2 f32 42.42
      }
    |}
  in
  let model = Model.Parse.Scfg.from_string model_str in
  assert (match model with Ok _ -> true | _ -> false)
