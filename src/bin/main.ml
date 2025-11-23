(***************************************************************************)
(* This file is part of the third-party OCaml library `smtml`.             *)
(* Copyright (C) 2023-2024 formalsec                                       *)
(*                                                                         *)
(* This program is free software: you can redistribute it and/or modify    *)
(* it under the terms of the GNU General Public License as published by    *)
(* the Free Software Foundation, either version 3 of the License, or       *)
(* (at your option) any later version.                                     *)
(*                                                                         *)
(* This program is distributed in the hope that it will be useful,         *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of          *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           *)
(* GNU General Public License for more details.                            *)
(*                                                                         *)
(* You should have received a copy of the GNU General Public License       *)
(* along with this program.  If not, see <https://www.gnu.org/licenses/>.  *)
(***************************************************************************)

let returncode =
  match Cmdliner.Cmd.eval_value Cli.commands with
  | Ok (`Help | `Version | `Ok ()) -> Cmdliner.Cmd.Exit.ok
  | Error e -> begin
    match e with
    | `Term -> Cmdliner.Cmd.Exit.some_error
    | `Parse -> Cmdliner.Cmd.Exit.cli_error
    | `Exn -> Cmdliner.Cmd.Exit.internal_error
  end

let () = exit returncode
