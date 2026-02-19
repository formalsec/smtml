open Cmdliner
open Term.Syntax
open Rresult

let __REQFILE_NAME__ = "requirements.txt"

let __SCRIPT_NAME__ = "smtzilla.py"

let smtzilla_data_dirpath () =
  match Smtml_sites.Sites.data with
  | [ dirpath ] -> dirpath
  | _ ->
    Fmt.failwith
      "Expected one directory path in Smtzilla_utils.Sites.data, instead got: \
       %d values"
      (List.length Smtml_sites.Sites.data)

let python_script_path () =
  let python_script_path =
    String.concat "/" [ smtzilla_data_dirpath (); __SCRIPT_NAME__ ]
  in
  let res =
    Fpath.of_string python_script_path >>= Bos.OS.File.exists >>= fun exists ->
    if exists then Ok python_script_path
    else
      Fmt.error_msg "The python script file does not exist in: %s"
        python_script_path
  in
  match res with
  | Ok str -> str
  | Error (`Msg msg) -> Fmt.failwith "Error: %s" msg

let parse_file s =
  match Fpath.of_string s with
  | Error _ as e -> e
  | Ok path -> begin
    match Bos.OS.File.exists path with
    | Ok true -> Ok path
    | Ok false -> Fmt.error_msg "The file '%a' does not exist" Fpath.pp path
    | Error _ as e -> e
  end

let file_exists_conv = Arg.conv (parse_file, Fpath.pp)

let csv_file_exists_conv =
  let parse_csv_file s =
    match parse_file s with
    | Error _ as e -> e
    | Ok path -> begin
      match Fpath.has_ext "csv" path with
      | true -> Ok path
      | false -> Fmt.error_msg "File '%a' is not a CSV file" Fpath.pp path
    end
  in
  Arg.conv (parse_csv_file, Fpath.pp)

let existing_parent_dir_conv =
  let parse s =
    match Fpath.of_string s with
    | Error _ as e -> e
    | Ok path -> begin
      let dir, _ = Fpath.split_base path in
      match Bos.OS.Dir.exists dir with
      | Ok true -> Ok path
      | Ok false -> Fmt.error_msg "No parent directory for '%a'" Fpath.pp path
      | Error _ as e -> e
    end
  in
  Arg.conv (parse, Fpath.pp)

let debug =
  let doc = "Print debugging messages." in
  Arg.(value & flag & info [ "debug" ] ~doc)

let gradient_boost =
  let doc_gb = "Use the gradient-boost regressor. (Default)" in
  let doc_nogb = "Use the decision tree regressor." in
  Arg.(
    value
    & vflag true
        [ (true, info [ "gradient-boost" ] ~doc:doc_gb)
        ; (false, info [ "no-gradient-boost" ] ~doc:doc_nogb)
        ] )

let pp_stats =
  let doc = "Print statistics on queries." in
  Arg.(value & flag & info [ "pp-stats" ] ~doc)

let run_simulation =
  let doc = "Run simulation on trained model." in
  Arg.(value & flag & info [ "simulation" ] ~doc)

let marshalled_file =
  let doc = "Path to the file containing marshalled queries." in
  Arg.(
    required & pos 0 (some file_exists_conv) None (info [] ~doc ~docv:"INPUT") )

let output_csv =
  let doc =
    "Path to the output csv file in which to store the query features."
  in
  Arg.(
    required
    & pos 1 (some existing_parent_dir_conv) None (info [] ~doc ~docv:"CSV") )

let input_csv =
  let doc =
    "Path to the csv file that holds the query features (produced with the \
     `extract-features` command)."
  in
  Arg.(
    required & pos 0 (some csv_file_exists_conv) None (info [] ~doc ~docv:"CSV") )

let output_json =
  let doc = "Path to the JSON file to which the model will be exported." in
  Arg.(
    value
    & opt (some existing_parent_dir_conv) None
    & info [ "output" ] ~doc ~docv:"JSON" )

let run_regression ~debug ~gradient_boost ~pp_stats ~run_simulation ~output_json
  ~input_csv =
  let args = [ Fpath.to_string input_csv ] in
  let args =
    match output_json with
    | Some f -> "--export" :: Fpath.to_string f :: args
    | None -> args
  in
  let args = if run_simulation then "--simulation" :: args else args in
  let args = if pp_stats then "--pp-stats" :: args else args in
  let args =
    if gradient_boost then "--gradient-boost" :: args
    else "--no-gradient-boost" :: args
  in
  let args = if debug then "--debug" :: args else args in
  let cmd = Bos.Cmd.of_list ("python3" :: python_script_path () :: args) in
  Fmt.epr "Running: %a@." Bos.Cmd.pp cmd;
  match Bos.OS.Cmd.run cmd with
  | Ok () -> ()
  | Error (`Msg msg) ->
    Fmt.failwith
      "Run regression failed with error: %s\n\
       If the error is a python error, ensure that you have installed the \
       necessary python packages, which can be obtained with `smtml smtzilla \
       requirements`."
      msg

let extract_cmd =
  let extract_info =
    let doc =
      "Given a file containing marshalled smtml queries, extracts features \
       from those queries which are then used to create the decision tree."
    in
    Cmd.info "extract-features" ~doc
  in
  let extract =
    let+ marshalled_file
    and+ output_csv in
    Smtml.Feature_extraction.cmd marshalled_file output_csv
  in
  Cmd.v extract_info extract

let regression_cmd =
  let regression_info =
    let doc =
      "Given a CSV file with query features train the regression model"
    in
    Cmd.info "regression" ~doc
  in
  let regression =
    let+ debug
    and+ gradient_boost
    and+ pp_stats
    and+ run_simulation
    and+ output_json
    and+ input_csv in
    run_regression ~debug ~gradient_boost ~pp_stats ~run_simulation ~output_json
      ~input_csv
  in
  Cmd.v regression_info regression

let train_cmd =
  let train_info =
    let doc =
      "Given a marshalled file of queries, generate a CSV file with query \
       features, and train the regression model on it"
    in
    Cmd.info "train" ~doc
  in
  let train =
    let+ debug
    and+ gradient_boost
    and+ pp_stats
    and+ run_simulation
    and+ output_json
    and+ output_csv
    and+ marshalled_file in
    Smtml.Feature_extraction.cmd marshalled_file output_csv;
    run_regression ~debug ~gradient_boost ~pp_stats ~run_simulation ~output_json
      ~input_csv:output_csv
  in
  Cmd.v train_info train

let requirements_cmd =
  let requirements_info =
    let doc =
      "Print the list of requirement python packages for the smtzilla training \
       to work"
    in
    Cmd.info "requirements" ~doc
  in
  let pp_requirements () =
    let reqfile_path =
      String.concat "/" [ smtzilla_data_dirpath (); __REQFILE_NAME__ ]
    in
    let res =
      Fpath.of_string reqfile_path >>= Bos.OS.File.exists >>= fun exists ->
      if exists then
        Fpath.of_string reqfile_path >>= fun f ->
        Bos.OS.File.with_ic f
          (fun ic () -> Fmt.pr "%s" (In_channel.input_all ic))
          ()
      else
        Fmt.error_msg "The python requirements file does not exist in: %s"
          reqfile_path
    in
    match res with
    | Ok () -> ()
    | Error (`Msg msg) -> Fmt.failwith "Error: %s" msg
  in
  let requirements = Term.(const pp_requirements $ const ()) in
  Cmd.v requirements_info requirements
