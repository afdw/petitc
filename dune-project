(lang dune 2.2)

(name petitc)

(generate_opam_files true)

(source
 (github afdw/petitc))

(authors "Anton Danilkin")

(maintainers "Anton Danilkin")

(using menhir 2.1)

(package
 (name petitc)
 (synopsis "Petit C compiler")
 (description "Petit C compiler")
 (depends ocaml dune ppx_inline_test menhir ppx_import ppx_deriving))
