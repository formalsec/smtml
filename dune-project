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
   (z3 (and (>= "4.11.2") (< "4.13")))
   (core (and (>= "v0.15.1") (< "v0.16")))
   (core_unix (and (>= "v0.15.1") (< "v0.16")))
   menhir
   ppx_inline_test
   ppx_expect))
