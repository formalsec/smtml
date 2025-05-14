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
 (synopsis "An SMT solver frontend for OCaml")
 (description "Smt.ml is an SMT solver frontend for OCaml that simplifies \
              integration with various solvers through a consistent interface. \
              Its parametric encoding facilitates the easy addition of new \
              solver backends, while optimisations like formula simplification, \
              result caching, and detailed error feedback enhance performance \
              and usability.")
 ; Optional solver dependencies
 ; Change here sould be mirrored in "conflicts"
 (depopts
  alt-ergo-lib
  bitwuzla-cxx
  colibri2
  cvc5
  z3)
 (conflicts
  (bitwuzla-cxx
   (< "0.6.0"))
  (alt-ergo-lib
   (< "2.6.2"))
  (z3
   (or
    (< "4.12.2")
    (>= "4.15"))))
 (depends
  bos
  (cmdliner
   (>= "1.3.0"))
  dune
  (dolmen
   (= "0.10"))
  (dolmen_type
   (= "0.10"))
  (dolmen_model
   (= "0.10"))
  (fmt
   (>= "0.8.7"))
  fpath
  (hc
   (>= "0.3"))
  (menhir
   (and
    :build
    (>= "20220210")))
  (ocaml
   (>= "4.14.0"))
  ocaml_intrinsics
  (patricia-tree
   (>= "0.11.0"))
  (prelude
   (>= "0.5"))
  rusage
  (scfg
   (>= "0.5"))
  (yojson
   (>= "1.6.0"))
  (zarith
   (>= "1.5"))
  ;; Doc dependencies
  (odoc
   :with-doc)
  (sherlodoc
   :with-doc)
  ;; Test dependencies
  (bisect_ppx
   (and
    :with-test
    (>= "2.5.0")))
  (ounit2 :with-test)
  ;; Dev dependencies
  (benchpress
   (and
    :with-dev-setup
    (= "dev")))
  (cohttp
   :with-dev-setup)
  (cohttp-lwt-unix
   :with-dev-setup)
  (extunix
   :with-dev-setup)
  (dune-glob
   :with-dev-setup)
  (lwt
   :with-dev-setup)
  (mdx
   :with-test)
  (owl
   :with-dev-setup)
  (tls-lwt
   :with-dev-setup)))
