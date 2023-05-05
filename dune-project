(lang dune 3.0)

(name encoding)

(generate_opam_files true)

(source
 (github wasp-platform/encoding))

(authors "WASP Platform")

(maintainers "WASP Platform")

(license LICENSE)

(package
 (name encoding)
 (synopsis "Encoding library")
 (description "An OCaml abstraction layer for constraint solvers.")
 (depends (ocaml (>= "4.14.0")) dune (z3 (= "4.11.2")) core ppx_inline_test))
