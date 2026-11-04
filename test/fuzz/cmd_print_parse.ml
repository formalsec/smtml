open Crowbar

let run_print_parse ~seed ~repeat ~set_theory ~randomness_file ~infinity =
  let theory =
    match set_theory with Some t -> const t | None -> Theory.get_theory ()
  in
  let test =
    Crowbar.make_test ~name:"print parse"
      [ Gen.expr theory ]
      (fun expr ->
        Logs.app (fun m -> m "generated: %a" Smtml.Typed.Bool.pp expr);
        let parse =
          Fmt.str "%a" Smtml.Expr.Printer.pp_expr
            (Smtml.Typed.Unsafe.unwrap expr)
        in
        let reparse = Smtml.Parse.Smtml.Expr.from_string parse in
        let bool =
          match reparse with
          | Result.Error (`Msg err) ->
            Logs.app (fun m -> m "erreur : %s" err);
            false
          | Result.Ok expr2 ->
            Smtml.Expr.equal (Smtml.Typed.Unsafe.unwrap expr) expr2
        in
        Crowbar.check bool;
        Logs.debug (fun m -> m "done with current test") )
  in
  Crowbar.run_test ~seed ~repeat ~test ~randomness_file ~infinity
