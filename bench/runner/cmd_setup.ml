let debug = true

let debug k = if debug then k Fmt.epr

let init ~datasets_dir =
  let open Result.Syntax in
  let* dir_exists = Bos.OS.Dir.create ~path:true datasets_dir in
  if not dir_exists then debug (fun epr -> epr "Datasets dir already exists@.");
  Ok ()

type conf = { datasets : Dataset.t list }

let parse_conf fpath =
  In_channel.with_open_text (Fpath.to_string fpath) @@ fun ic ->
  let datasets = Sexplib.Sexp.input_sexps ic |> List.map Dataset.of_sexp in
  { datasets }

let file_exists ?hash fpath =
  let open Result.Syntax in
  let* file_exists = Bos.OS.File.exists fpath in
  if not file_exists then Ok false
  else begin
    match hash with
    | None -> Ok true
    | Some hash ->
      let md5sum = Digest.MD5.(to_hex (file (Fpath.to_string fpath))) in
      if Digest.MD5.equal hash md5sum then Ok true
      else
        Error
          (`Msg
             (Fmt.str "%a: expected hash %s but got %s" Fpath.pp fpath hash
                md5sum ) )
  end

let curl url out_file = Bos.Cmd.(v "curl" % url % "--output" % p out_file)

let tar_extract archive output_dir =
  Bos.Cmd.(v "tar" % "-xf" % p archive % "-C" % p output_dir)

let setup ~datasets_dir { datasets } =
  let open Result.Syntax in
  Result.list_iter
    (fun { Dataset.name; url; md5sum } ->
      let this_dataset_dir = Fpath.(datasets_dir / name) in
      let* _ = Bos.OS.Dir.create this_dataset_dir in
      debug (fun epr -> epr "Downloading %s from %s@." name url);
      let out_file = Fpath.(this_dataset_dir // base (v url)) in
      let* file_exists = file_exists ~hash:md5sum out_file in
      if file_exists then begin
        debug (fun epr -> epr "Skipping: correct file exists@.");
        Ok ()
      end
      else begin
        debug (fun epr -> epr "Saving to %a@." Fpath.pp out_file);
        let* () = Bos.OS.Cmd.run (curl url out_file) in
        Bos.OS.Cmd.run (tar_extract out_file this_dataset_dir)
      end )
    datasets

let main ~datasets_dir ~file =
  let open Result.Syntax in
  debug (fun epr -> epr "Using config: '%a'@." Fpath.pp file);
  let* () = init ~datasets_dir in
  let conf = parse_conf file in
  setup ~datasets_dir conf
