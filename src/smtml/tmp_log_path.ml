

let log_path : out_channel option ref = ref None

let set path = log_path := Some (Out_channel.open_gen [ Open_creat; Open_append; Open_text ] 0o644 path)

let write s =
  match !log_path with
  | Some p -> Out_channel.output_string p s
  | None ->
    Fmt.failwith "Temporary log path not set"
