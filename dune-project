(lang dune 3.0)
(using menhir 2.1)

(name encoding)

(generate_opam_files true)

(source
 (github wasp-platform/encoding))

(authors "WASP Platform")

(maintainers "WASP Platform")

(license "GPL-3.0-only")

(package
 (name encoding)
 (synopsis "Encoding library")
 (description "An OCaml abstraction layer for constraint solvers.")
 (depends
   dune
   (ocaml (>= "4.14.0"))
   (z3 (= "4.11.2"))
   (core (= "v0.15.1"))
   (core_unix (= "v0.15.1"))
   menhir
   ppx_inline_test
   ppx_expect))
