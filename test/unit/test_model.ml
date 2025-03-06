open Smtml

let test_to_json () =
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

let test_of_json () =
  let open Result in
  let model_str =
    {|
      {
        "model" : {
          "x_0" : { "ty" : "int", "value" : 42 },
          "x_1" : { "ty" : "bool", "value" : true },
          "x_2" : { "ty" : "f32", "value" : 42.42 },
          "x_3" : { "ty" : "i64", "value" : -13333337 }
        }
      }
    |}
  in
  let model = Model.Parse.Json.from_string model_str in
  assert (match model with Ok _ -> true | _ -> false)

let test_rt_json () =
  let x = Symbol.make (Ty_bitv 32) "x" in
  let y = Symbol.make (Ty_bitv 64) "y" in
  let z = Symbol.make (Ty_fp 32) "y" in
  let orig_model : Model.t =
    let tbl = Hashtbl.create 16 in
    List.iter
      (fun ((s, v) : Symbol.t * Value.t) -> Hashtbl.replace tbl s v)
      [ (x, Num (I32 Int32.min_int))
      ; (y, Num (I64 Int64.max_int))
      ; (z, Num (F32 1084227584l))
      ];
    tbl
  in
  let json_model = Model.to_json_string orig_model in
  match Model.Parse.Json.from_string json_model with
  | Error (`Msg err) -> Fmt.failwith "%s" err
  | Ok model -> begin
    let x_val = Model.evaluate model x in
    let y_val = Model.evaluate model y in
    let z_val = Model.evaluate model z in
    match (x_val, y_val, z_val) with
    | Some x_val, Some y_val, Some z_val ->
      assert (
        Value.equal x_val (Num (I32 Int32.min_int))
        && Value.equal y_val (Num (I64 Int64.max_int))
        && Value.equal z_val (Num (F32 (Int32.bits_of_float 5.0))) )
    | _ -> assert false
  end

let test_json () =
  test_to_json ();
  test_of_json ();
  test_rt_json ()

let test_of_scfg () =
  let open Result in
  let model_str =
    {|
      model {
        symbol x_0 int 42
        symbol x_1 bool true
        symbol x_2 f32 42.42
        symbol x_3 i64 -13333337
      }
    |}
  in
  let model = Model.Parse.Scfg.from_string model_str in
  assert (match model with Ok _ -> true | _ -> false)

let test_scfg () = test_of_scfg ()

let () =
  test_json ();
  test_scfg ()
