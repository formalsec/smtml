type prover_name =
  | Z3
  | Bitwuzla
  | Cvc5
  | Colibri2

type prover =
  | Z3
  | Smtml of
      { name : prover_name
      ; mutable st : bool
      }

let prover_of_string str =
  match String.lowercase_ascii str with
  | "z3" -> Ok Z3
  | "smtml-bitwuzla" -> Ok (Smtml { name = Bitwuzla; st = false })
  | "smtml-cvc5" -> Ok (Smtml { name = Cvc5; st = false })
  | "smtml-colibri2" -> Ok (Smtml { name = Colibri2; st = false })
  | "smtml-z3" -> Ok (Smtml { name = Z3; st = false })
  | _ -> Error (`Msg (Fmt.str "%s: unknown prover" str))

let pp_prover_name fmt : prover_name -> unit = function
  | Z3 -> Fmt.string fmt "z3"
  | Bitwuzla -> Fmt.string fmt "bitwuzla"
  | Cvc5 -> Fmt.string fmt "cvc5"
  | Colibri2 -> Fmt.string fmt "colibri2"

let pp_prover fmt = function
  | Z3 -> Fmt.string fmt "z3"
  | Smtml { name; _ } -> Fmt.pf fmt "smtml-%a" pp_prover_name name

let prover_to_string = Fmt.str "%a" pp_prover

let is_available =
  let open Smtml in
  function
  | Z3 -> Solver_type.is_available Z3_solver
  | Smtml { name = Bitwuzla; _ } -> Solver_type.is_available Bitwuzla_solver
  | Smtml { name = Cvc5; _ } -> Solver_type.is_available Cvc5_solver
  | Smtml { name = Colibri2; _ } -> Solver_type.is_available Colibri2_solver
  | Smtml { name = Z3; _ } -> Solver_type.is_available Z3_solver

let cmd ?from_file prover files =
  match prover with
  | Z3 -> ("z3", Array.of_list @@ ("z3" :: files))
  | Smtml { name; st } ->
    ( "smtml"
    , let args =
        "--dry" :: "--mode" :: "incremental" :: "--solver"
        :: Fmt.str "%a" pp_prover_name name
        ::
        ( match from_file with
        | None -> files
        | Some file -> "--from-file" :: [ file ] )
      in
      Array.of_list
      @@ "smtml" :: "run"
         :: (if st then "--print-statistics" :: args else args) )

let dup2 ~src ~dst =
  Unix.dup2 src dst;
  Unix.close src

let limit_cpu time_limit =
  let time_limit = Some (Int64.of_int time_limit) in
  ExtUnix.Specific.setrlimit RLIMIT_CPU ~soft:time_limit ~hard:time_limit

let with_ic fd f =
  let ic = Unix.in_channel_of_descr fd in
  Fun.protect ~finally:(fun () -> In_channel.close ic) (fun () -> f ic)

let fork_and_run ?timeout ?from_file prover file =
  let stdout_read, stdout_write = Unix.pipe () in
  let stderr_read, stderr_write = Unix.pipe () in
  let prog, argv = cmd ?from_file prover file in
  let pid = Unix.fork () in
  if pid = 0 then begin
    Unix.close stdout_read;
    Unix.close stderr_read;
    dup2 ~src:stdout_write ~dst:Unix.stdout;
    dup2 ~src:stderr_write ~dst:Unix.stderr;
    Option.iter limit_cpu timeout;
    Unix.execvp prog argv
  end
  else begin
    Unix.close stdout_write;
    Unix.close stderr_write;
    let stdout = with_ic stdout_read In_channel.input_all in
    let stderr = with_ic stderr_read In_channel.input_all in
    let waited_pid, status, usage = ExtUnix.Specific.wait4 [] pid in
    assert (pid = waited_pid);
    (status, stdout, stderr, usage)
  end
