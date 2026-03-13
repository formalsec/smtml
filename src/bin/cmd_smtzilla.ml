open Cmdliner
open Term.Syntax
open Rresult
open Smtml
module IntSet = Set.Make (Int)

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

let dir_exists_conv =
  let parse s =
    Fpath.of_string s >>= fun path ->
    Bos.OS.Dir.exists path >>= fun b ->
    if b then Ok path
    else Fmt.error_msg "The directory '%a' does not exist" Fpath.pp path
  in
  Arg.conv (parse, Fpath.pp)

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

let pos_int =
  let parser x =
    match int_of_string_opt x with
    | Some i when i > 0 -> Ok i
    | None | Some _ -> Fmt.error_msg "Expected a positive integer"
  in
  Arg.conv (parser, Format.pp_print_int)

let n_estimators =
  let doc =
    "Number of estimators (must be > 0, used only for gradient-boost)."
  in
  Arg.(value & opt pos_int 5 & info [ "n-estimators" ] ~doc)

let max_depth =
  let doc = "Maximum depth of trees (must be > 0)." in
  Arg.(value & opt pos_int 5 & info [ "max-depth" ] ~doc)

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

let dest_dir =
  let doc = "Path to a directory in which to store the exported queries." in
  Arg.(required & pos 1 (some dir_exists_conv) None (info [] ~doc ~docv:"DIR"))

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

let output_json p =
  let doc = "Path to the JSON file to which the model will be exported." in
  Arg.(
    required
    & pos p (some existing_parent_dir_conv) None (info [] ~doc ~docv:"JSON") )

let set_debug debug =
  if debug then Logs.Src.set_level Smtml.Log.src (Some Logs.Debug);
  Logs.set_reporter @@ Logs.format_reporter ()

(* type annotation because otherwise the typechecker thinks ?status is ~status *)
let rec extract_queries
  (smt2pp :
       ?name:string
    -> ?logic:Logic.t
    -> ?status:[ `Sat | `Unknown | `Unsat ]
    -> Expr.t list Fmt.t ) destdir seen cnt l =
  match l with
  | [] -> Ok (seen, cnt)
  | (_, assertions, _, _, status) :: t ->
    (* TODO: do better than Hashtbl.hash *)
    let hash = Hashtbl.hash assertions in
    if IntSet.mem hash seen then extract_queries smt2pp destdir seen cnt t
    else
      let file_path = Fpath.(destdir / Fmt.str "query.%d.smt2" cnt) in
      Bos.OS.File.writef file_path "%a"
        (smt2pp ?name:None ?logic:None ~status)
        assertions
      >>= fun _ ->
      extract_queries smt2pp destdir (IntSet.add hash seen) (cnt + 1) t

let rec queries_from_ic smt2pp destdir seen cnt ic =
  let queries :
    (string * Expr.t list * bool * int64 * [ `Sat | `Unsat | `Unknown ]) list =
    Marshal.from_channel ic
  in
  extract_queries smt2pp destdir seen cnt queries >>= fun (seen, cnt) ->
  queries_from_ic smt2pp destdir seen cnt ic

let extract_queries (path : Fpath.t) (destdir : Fpath.t) =
  let (module M) = Solver_type.to_mappings Solver_type.Z3_solver in
  if not M.is_available then
    Fmt.failwith "Query extraction to smt file depends on Z3";
  let smt2pp = M.Smtlib.pp in
  let res =
    Bos.OS.File.with_ic path
      (fun ic seen ->
        try queries_from_ic smt2pp destdir seen 1 ic >>| fun _ -> ()
        with End_of_file ->
          Log.debug (fun k -> k "Finished reading results@.");
          Ok () )
      IntSet.empty
  in
  match Rresult.R.join res with
  | Ok () -> ()
  | Error (`Msg msg) ->
    Fmt.failwith "Failed to extract queries from %a\nBecause of: %s" Fpath.pp
      path msg

let extract_queries_cmd =
  let extract_info =
    let doc =
      "Given a file containing marshalled smtml queries, extracts the (unique) \
       queries into files in a given folder. The files are named \
       `query.n.smt2` where `n` is the query's number."
    in
    Cmd.info "extract-queries" ~doc
  in
  let extract =
    let+ marshalled_file
    and+ dest_dir in
    extract_queries marshalled_file dest_dir
  in
  Cmd.v extract_info extract

let run_regression ~debug ~gradient_boost ~n_estimators ~max_depth ~pp_stats
  ~run_simulation ~input_csv ~output_json =
  let debug = Bos.Cmd.(if debug then v "--debug" else empty) in
  let gradient_boost =
    Bos.Cmd.(
      match gradient_boost with
      | GradientBoost -> v "--gradient-boost"
      | DecisionTree -> v "--no-gradient-boost" )
  in
  let n_estimators =
    Bos.Cmd.(v "--n-estimators" % string_of_int n_estimators)
  in
  let max_depth = Bos.Cmd.(v "--max-depth" % string_of_int max_depth) in
  let pp_stats = Bos.Cmd.(if pp_stats then v "--pp-stats" else empty) in
  let run_simulation =
    Bos.Cmd.(if run_simulation then v "--simulation" else empty)
  in
  let py_script_path = python_script_path () in
  let cmd =
    Bos.Cmd.(
      v "python3" % p py_script_path %% debug %% gradient_boost %% n_estimators
      %% max_depth %% pp_stats %% run_simulation % p input_csv % p output_json )
  in
  Smtml.Log.debug (fun k -> k "Running: %a@." Bos.Cmd.pp cmd);
  match Bos.OS.Cmd.run cmd with
  | Ok () -> ()
  | Error (`Msg msg) ->
    Smtml.Log.err (fun k ->
      k
        "SMTZilla: Running the python script failed: \n\
         %s\n\
         If the error is a python error, make sure that you have installed the \
         necessary python packages to run the script located at: %a.@."
        msg Fpath.pp py_script_path );
    Fmt.failwith "Python script failure"

let extract_features_cmd =
  let extract_info =
    let doc =
      "Given a file containing marshalled smtml queries, extracts features \
       from those queries which are then used to create the decision tree."
    in
    Cmd.info "extract-features" ~doc
  in
  let extract =
    let+ debug
    and+ marshalled_file
    and+ output_csv in
    set_debug debug;
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
    and+ n_estimators
    and+ max_depth
    and+ pp_stats
    and+ run_simulation
    and+ input_csv
    and+ output_json = output_json 1 in
    set_debug debug;
    run_regression ~debug ~gradient_boost ~n_estimators ~max_depth ~pp_stats
      ~run_simulation ~output_json ~input_csv
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
    and+ n_estimators
    and+ max_depth
    and+ pp_stats
    and+ run_simulation
    and+ marshalled_file
    and+ output_csv
    and+ output_json = output_json 2 in
    set_debug debug;
    Smtml.Feature_extraction.cmd marshalled_file output_csv;
    run_regression ~debug ~gradient_boost ~n_estimators ~max_depth ~pp_stats
      ~run_simulation ~output_json ~input_csv:output_csv
  in
  Cmd.v train_info train
