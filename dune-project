(lang dune 3.0)
(using menhir 2.1)

(name encoding)

(generate_opam_files true)

(source
 (github formalsec/encoding))

(authors "Filipe Marques <filipe.s.marques@tecnico.ulisboa.pt>")

(maintainers "Filipe Marques <filipe.s.marques@tecnico.ulisboa.pt>")

(license "GPL-3.0-only")

(package
 (name encoding)
 (synopsis "Smt encoding library")
 (description "An OCaml abstraction layer for constraint solvers.")
 (depends
   dune
   (ocaml (>= "5.1.0"))
   (z3 (and (>= "4.11.2") (< "4.13")))
   menhir
   (zarith (>= "1.5"))
   (ppx_inline_test :with-test)
   (odoc :with-doc)))
