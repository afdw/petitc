(lang dune 3.6)

(name petitc)

(generate_opam_files true)

(source
 (github afdw/petitc))

(authors "Anton Danilkin")

(maintainers "Anton Danilkin")

(package
 (name petitc)
 (synopsis "Petit C compiler")
 (description "Petit C compiler")
 (depends ocaml dune ppx_inline_test))
