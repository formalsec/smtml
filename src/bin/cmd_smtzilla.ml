open Cmdliner
open Term.Syntax
open Rresult

let script_name = R.failwith_error_msg (Fpath.of_string "smtzilla.py")

let smtzilla_data_dirpath () =
  match Smtml_sites.Sites.data with
  | [ dirpath ] -> Fpath.of_string dirpath
  | _ ->
    Fmt.error_msg
      "Expected one directory path in Smtzilla_utils.Sites.data, instead got: \
       %d values"
      (List.length Smtml_sites.Sites.data)

let python_script_path () =
  let python_script_path =
    smtzilla_data_dirpath () >>| fun dirpath -> Fpath.(dirpath // script_name)
  in
  let res =
    python_script_path >>= fun script_path ->
    Bos.OS.File.exists script_path >>= fun exists ->
    if exists then Ok script_path
    else
      Fmt.error_msg "The python script file does not exist in: %a" Fpath.pp
        script_path
  in
  R.failwith_error_msg res

let parse_file s =
  Fpath.of_string s >>= fun path ->
  Bos.OS.File.exists path >>= fun b ->
  if b then Ok path
  else Fmt.error_msg "The file '%a' does not exist" Fpath.pp path

let file_exists_conv = Arg.conv (parse_file, Fpath.pp)

let csv_file_exists_conv =
  let parse_csv_file s =
    Fpath.of_string s >>= fun path ->
    if Fpath.has_ext "csv" path then Ok path
    else Fmt.error_msg "File '%a' is not a CSV file" Fpath.pp path
  in
  Arg.conv (parse_csv_file, Fpath.pp)

let existing_parent_dir_conv =
  let parse s =
    Fpath.of_string s >>= fun path ->
    let dir, _ = Fpath.split_base path in
    Bos.OS.Dir.exists dir >>= fun b ->
    if b then Ok path
    else Fmt.error_msg "No parent directory for '%a'" Fpath.pp path
  in
  Arg.conv (parse, Fpath.pp)

let debug =
  let doc = "Print debugging messages." in
  Arg.(value & flag & info [ "debug" ] ~doc)

type predictor =
  | GradientBoost
  | DecisionTree

let predictor_conv =
  let parse s =
    match String.lowercase_ascii s with
    | "gradient-boost" | "gb" -> Ok GradientBoost
    | "decision-tree" | "dt" -> Ok DecisionTree
    | _ -> Error (`Msg (Printf.sprintf "Unknown model type: %s" s))
  in
  let print ppf = function
    | GradientBoost -> Format.fprintf ppf "gradient-boost"
    | DecisionTree -> Format.fprintf ppf "decision-tree"
  in
  Arg.conv (parse, print)

let gradient_boost =
  let doc =
    "Predictor kind, either a gradient boosting regressor (gradient-boost or \
     gb), which is the default, or a decision tree regressor (decision-tree or \
     dt)."
  in
  Arg.(value & opt predictor_conv GradientBoost & info [ "predictor" ] ~doc)

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
  let debug = Bos.Cmd.(if debug then v "--debug" else empty) in
  let gradient_boost =
    Bos.Cmd.(
      match gradient_boost with
      | GradientBoost -> v "--gradient-boost"
      | DecisionTree -> v "--no-gradient-boost" )
  in
  let pp_stats = Bos.Cmd.(if pp_stats then v "--pp-stats" else empty) in
  let run_simulation =
    Bos.Cmd.(if run_simulation then v "--simulation" else empty)
  in
  let export =
    Bos.Cmd.(
      match output_json with Some f -> v "--export" % p f | None -> empty )
  in
  let py_script_path = python_script_path () in
  let cmd =
    Bos.Cmd.(
      v "python3" % p py_script_path %% debug %% gradient_boost %% pp_stats
      %% run_simulation %% export % p input_csv )
  in
  Smtml.Log.debug (fun k -> k "Running: %a@." Bos.Cmd.pp cmd);
  match Bos.OS.Cmd.run cmd with
  | Ok () -> ()
  | Error (`Msg msg) ->
    Fmt.failwith
      "Running the python script failed with the error: %s\n\
       If the error is a python error, make sure that you have installed the \
       necessary python packages to run the script located at: %a."
      msg Fpath.pp py_script_path

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
