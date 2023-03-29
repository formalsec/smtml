(lang dune 3.0)

(name encoding)

(generate_opam_files true)

(source
 (github wasp-platform/encoding))

(license LICENSE)

(package
 (name encoding)
 (synopsis "Encoding library")
 (description "OCaml Z3 symbolic value encoding library")
 (depends ocaml dune base ppx_inline_tests))
