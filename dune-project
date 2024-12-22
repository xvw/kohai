(lang dune 3.17)

(name kohai)
(version dev)
(generate_opam_files true)
(executables_implicit_empty_intf)

(source (github xvw/kohai))
(license MIT)
(authors "Xavier Van de Woestyne <xaviervdw@gmail.com>")
(maintainers "Xavier Van de Woestyne <xaviervdw@gmail.com>")

(package
 (name kohai)
 (allow_empty)
 (synopsis "TODO: to be done")
 (description "TODO: to be done")
 (depends
  (ocaml (>= 5.2.0))
  (ocamlformat :with-dev-setup)
  (ocp-indent :with-dev-setup)
  (merlin :with-dev-setup)
  (ocaml-lsp-server :with-dev-setup)
  (utop :with-dev-setup)))

(package
 (name rensai)
 (synopsis "TODO: to be done")
 (description "TODO: to be done")
 (depends
  (ocaml (>= 5.2.0))))
