type t =
  { name : string
  ; url : string
  ; md5sum : string
  }

let pp fmt { name; url; md5sum } =
  Fmt.pf fmt "@[<hov 1>{@ name@ =@ %a;@ url@ =@ %a;@ md5sum@ =@ %a@ }@]"
    Fmt.string name Fmt.string url Fmt.string md5sum

let of_sexp sexp =
  match sexp with
  | Sexplib.Sexp.List
      [ Atom "dataset"
      ; List [ Atom "name"; Atom name ]
      ; List [ Atom "url"; Atom url ]
      ; List [ Atom "md5sum"; Atom md5sum ]
      ] ->
    { name; url; md5sum }
  | _ -> Fmt.failwith "Unable to parse sexp: %a" Sexplib.Sexp.pp_hum sexp

let to_sexp { name; url; md5sum } =
  Sexplib.Sexp.(
    List
      [ Atom "dataset"
      ; List [ Atom "name"; Atom name ]
      ; List [ Atom "url"; Atom url ]
      ; List [ Atom "md5sum"; Atom md5sum ]
      ] )
