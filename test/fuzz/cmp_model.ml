open Crowbar

let run ~seed ~repeat ~randomness_file ~infinity ~disable_z3 ~disable_altergo
  ~disable_bitwuzla ~disable_cvc5 ~disable_colibri2 ~disable_smtzilla
  ~set_theory ~cmp_model =
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
        (* Logs.debug (fun m -> m "generated: %a" Smtml.Typed.Bool.pp expr); *)
        if cmp_model then (
          Crowbar.check (Solvers.check_model expr solvers);
          Logs.debug (fun m -> m "done with current test") )
        else Logs.debug (fun m -> m "done with current test") )
  in
  Crowbar.run_test ~seed ~repeat ~randomness_file ~infinity ~test
