(lang dune 3.17)

(name kohai)
(version dev)
(generate_opam_files true)
(executables_implicit_empty_intf)
(using mdx 0.4)

(source (github xvw/kohai))
(license MIT)
(authors "Xavier Van de Woestyne <xaviervdw@gmail.com>")
(maintainers "Xavier Van de Woestyne <xaviervdw@gmail.com>")

(package
 (name kohai-prelude)
 (synopsis "A small standard library extension")
 (description "Some tools for easing Kohai foundational development")
 (depends
  (ocaml (>= 5.2.0))
  (ppx_expect :with-test)))

(package
 (name kohai)
 (allow_empty)
 (synopsis "A small hand-crafted tool for entering activities")
 (description "A tool for collecting personal activity reports
(and an excuse to write OCaml and reinvent the wheel)")
 (depends
  (ocaml (>= 5.2.0))
  (ocamlformat :with-dev-setup)
  (ocp-indent :with-dev-setup)
  (merlin :with-dev-setup)
  (ocaml-lsp-server :with-dev-setup)
  (utop :with-dev-setup)))

(package
 (name rensai)
 (synopsis "Rensai can be used to describe generic encoders and decoders ")
 (description "By lifting heterogeneous data into a finite set of constructors,
it is possible to describe languages similar to JSON, TOML or S-Expression,
for which the encoding and validation functions become generic
(and interchangeable).")
 (depends
  (ocaml (>= 5.2.0))
  (kohai-prelude (= :version))
  fmt
  (mdx :with-test)))
