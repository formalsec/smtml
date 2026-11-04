open Crowbar
open Crowbar.Syntax

type t =
  { bv32_names : Smtml.Symbol.t Crowbar.gen
  ; bool_names : Smtml.Symbol.t Crowbar.gen
  ; float32_names : Smtml.Symbol.t Crowbar.gen
  ; int_names : Smtml.Symbol.t Crowbar.gen
  ; float64_names : Smtml.Symbol.t Crowbar.gen
  ; theory : Theory.t
  }

let letter : char Crowbar.gen =
  let+ i = range 26 in
  Char.chr (Char.code 'A' + i)

let gen_name : string Crowbar.gen =
  let* i = range 2 in
  let len = i + 1 in

  let rec build acc k =
    if k = len then
      let a = Array.of_list acc in
      const (String.init len (Array.get a))
    else
      let* c = letter in
      build (c :: acc) (succ k)
  in
  build [] 0

let symbol_gen ty names =
  names
  |> List.map (fun n -> Crowbar.const (Smtml.Symbol.make_const ty n))
  |> Crowbar.choose

let state theory =
  (* this should be updated when generating new king of symbols *)
  let aux theory =
    (* we need at least one element per bucket *)
    let bucket_number = 5 in
    let* name1 = gen_name in
    let* name2 = gen_name in
    let* name3 = gen_name in
    let* name4 = gen_name in
    let* name5 = gen_name in
    let* names = list gen_name in
    let names =
      ("a" ^ name1) :: ("b" ^ name2) :: ("c" ^ name3) :: ("d" ^ name4)
      :: ("e" ^ name5) :: names
    in
    let names = List.sort_uniq String.compare names in
    (* Printf.printf "après %d\n%!" __LINE__; *)

    (* we put one name in each bucket *)
    let bv32, bool, float32, int, float64, rest =
      match names with
      | a :: b :: c :: d :: e :: rest ->
        ([ a ], [ b ], [ c ], [ d ], [ e ], rest)
      | _ -> assert false
    in
    (* Printf.printf "après %d\n%!" __LINE__; *)
    (* we put all the remaining names in whatever bucket *)
    let* bv32, bool, float32, int, float64 =
      let rec loop ~bv32 ~bool ~float32 ~int ~float64 = function
        | [] -> const (bv32, bool, float32, int, float64)
        | n :: rest -> (
          let* i = range bucket_number in
          (* Printf.printf "après %d\n%!" __LINE__; *)
          match i with
          | 0 ->
            let bv32 = n :: bv32 in
            loop ~bv32 ~bool ~float32 ~int ~float64 rest
          | 1 ->
            let bool = n :: bool in
            loop ~bv32 ~bool ~float32 ~int ~float64 rest
          | 2 ->
            let float32 = n :: float32 in
            loop ~bv32 ~bool ~float32 ~int ~float64 rest
          | 3 ->
            let int = n :: int in
            loop ~bv32 ~bool ~float32 ~int ~float64 rest
          | 4 ->
            let float64 = n :: float64 in
            loop ~bv32 ~bool ~float32 ~int ~float64 rest
          | _ -> assert false )
      in
      (* Printf.printf "après %d\n%!" __LINE__; *)
      loop ~bv32 ~bool ~float32 ~int ~float64 rest
    in

    (*
      let pp_symbol_list ppf l =
        Fmt.pf ppf "%a"
          (Fmt.list ~sep:(fun ppf () -> Fmt.pf ppf " ") Fmt.string)
          l
      in
      Fmt.pr "BV32 symbols are: %a@\n" pp_symbol_list bv32;
      Fmt.pr "BOOL symbols are: %a@\n" pp_symbol_list bool;
      Fmt.pr "FP32 symbols are: %a@\n" pp_symbol_list float32;
*)
    (* Printf.printf "après %d\n%!" __LINE__; *)
    let* theory = theory in
    Theory.print_theory theory;
    const
      { bv32_names = symbol_gen (Smtml.Ty.Ty_bitv 32) bv32
      ; bool_names = symbol_gen Smtml.Ty.Ty_bool bool
      ; float32_names = symbol_gen (Smtml.Ty.Ty_fp 32) float32
      ; int_names = symbol_gen Smtml.Ty.Ty_int int
      ; float64_names = symbol_gen (Smtml.Ty.Ty_fp 64) float64
      ; theory
      }
  in
  aux theory
