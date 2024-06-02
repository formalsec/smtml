(lang dune 3.14)
(using menhir 2.1)

(name smtml)

(generate_opam_files true)

(source
 (github formalsec/smtml))

(authors "Filipe Marques <filipe.s.marques@tecnico.ulisboa.pt>")

(maintainers "Filipe Marques <filipe.s.marques@tecnico.ulisboa.pt>")

(documentation "https://formalsec.github.io/smtml/smtml/index.html")

(license "GPL-3.0-only")

(package
 (name smtml)
 (synopsis "A Front-end library for SMT solvers in OCaml")
 (description "A Multi Back-end Front-end for SMT Solvers in OCaml.")
 (depopts colibri2 (bitwuzla-cxx (>= "0.4.0")) cvc5)
 (depends
   dune
   (ocaml (>= "4.14.0"))
   ocaml_intrinsics
   (z3 (and (>= "4.12.2") (< "4.13")))
   (menhir
    (and
    :build
    (>= 20220210)))
   (cmdliner (>= "1.2.0"))
   (zarith (>= "1.5"))
   (odoc :with-doc)
   (hc (>= "0.3"))
   (bisect_ppx (and :with-test (>= "2.5.0") :dev))))
