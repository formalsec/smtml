(lang dune 3.10)

(using menhir 2.1)

(using mdx 0.2)

(name smtml)

(generate_opam_files true)

(source
 (github formalsec/smtml))

(authors
 "João Pereira <joaomhmpereira@tecnico.ulisboa.pt>"
 "Filipe Marques <filipe.s.marques@tecnico.ulisboa.pt>"
 "Hichem Rami Ait El Hara <hra@ocamlpro.com>"
 "Léo Andrès <contact@ndrs.fr>"
 "Arthur Carcano <arthur.carcano@ocamlpro.com>"
 "Pierre Chambart <pierre.chambart@ocamlpro.com>"
 "José Fragoso Santos <jose.fragoso@tecnico.ulisboa.pt>")

(maintainers "Filipe Marques <filipe.s.marques@tecnico.ulisboa.pt>")

(documentation "https://formalsec.github.io/smtml/smtml/index.html")

(license "MIT")

(package
 (name smtml)
 (synopsis "A Front-end library for SMT solvers in OCaml")
 (description "A Multi Back-end Front-end for SMT Solvers in OCaml.")
 ; Optional solver dependencies
 ; Change here sould be mirrored in "conflicts"
 (depopts
  z3
  colibri2
  bitwuzla-cxx
  cvc5)
 (conflicts
  (z3
   (or
    (< "4.12.2")
    (>= "4.14")))
   (bitwuzla-cxx
    (< "0.4.0"))
 )
 (depends
  dune
  (dolmen
   (= "0.10"))
  (dolmen_type
   (= "0.10"))
  (ocaml
   (>= "4.14.0"))
  (prelude
   (>= "0.3"))
  ocaml_intrinsics
  (fmt
   (>= "0.8.7"))
  (cmdliner
   (>= "1.2.0"))
  (zarith
   (>= "1.5"))
  (hc (>= "0.3"))
  (menhir
   (and
    :build
    (>= "20220210")))
  rusage
  (patricia-tree
   (>= "0.10.0"))
  (yojson
   (>= "1.6.0"))
  (odoc
   :with-doc)
  (sherlodoc
   :with-doc)
  (bisect_ppx
   (and
    :with-test
    (>= "2.5.0")))
  (benchpress
   (and
    :with-dev-setup
    (= "dev")))
  (cohttp
   :with-dev-setup)
  (cohttp-lwt-unix
   :with-dev-setup)
  (lwt
   :with-dev-setup)
  (tls-lwt
   :with-dev-setup)
  (mdx
   :with-test)
  (owl
   :with-dev-setup)
  (core_unix
   :with-dev-setup)))
