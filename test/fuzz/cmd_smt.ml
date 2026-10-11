open Crowbar

let run ~seed ~repeat ~randomness_file ~infinity ~disable_z3 ~disable_altergo
  ~disable_bitwuzla ~disable_cvc5 ~disable_colibri2 ~disable_smtzilla
  ~set_theory ~get_model =
  Logs.debug (fun m -> m "run...");
  let solvers =
    Solvers.init ~disable_z3 ~disable_bitwuzla ~disable_altergo ~disable_cvc5
      ~disable_colibri2 ~disable_smtzilla
  in
  let theory =
    match set_theory with Some t -> const t | None -> Theory.get_theory ()
  in
  let test =
    Crowbar.make_test ~name:"check expr"
      [ Gen.expr theory ]
      (fun expr ->
        Logs.debug (fun m -> m "generated: %a" Smtml.Typed.Bool.pp expr);
        let have_same_output =
          Solvers.check_same_output expr solvers get_model
        in
        Logs.debug (fun m ->
          m "solver have same output: %s"
            (if have_same_output then "OK" else "KO") );
        Crowbar.check have_same_output;
        Logs.debug (fun m -> m "done with current test") )
  in
  Crowbar.run_test ~seed ~repeat ~randomness_file ~infinity ~test
