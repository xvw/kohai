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
  (ocaml (>= 5.3.0))
  (ppx_expect :with-test)
  (mdx :with-test)))

(package
 (name rensai)
 (synopsis "Rensai can be used to describe generic encoders and decoders")
 (description "By lifting heterogeneous data into a finite set of constructors,
it is possible to describe languages similar to JSON, TOML or S-Expression,
for which the encoding and validation functions become generic
(and interchangeable).")
 (depends
  (ocaml (>= 5.3.0))
  (kohai-prelude (= :version))
  (fmt (>= 0.9.0))
  (ppx_expect :with-test)
  (mdx :with-test)))

(package
 (name kohai-core)
 (synopsis "Tools from the heart of Kohai")
 (description "Set of concrete data structures and tools needed to
 model data in Kohai")
 (depends
  (ocaml (>= 5.3.0))
  (kohai-prelude (= :version))
  (rensai (= :version))
  (ppx_expect :with-test)
  (mdx :with-test)))
 
(package
 (name kohai)
 (allow_empty)
 (synopsis "A small hand-crafted tool for entering activities")
 (description "A tool for collecting personal activity reports
(and an excuse to write OCaml and reinvent the wheel)")
 (depends
  (ocaml (>= 5.3.0))
  (rensai (= :version))
  (yojson (>= 2.2.2))
  (fmt (>= 0.9.0))
  (logs (>= 0.7.0))
  (cmdliner (>= 1.3.0))
  (eio (>= 1.2))
  (eio_main (>= 1.2))
  (ocamlformat :with-dev-setup)
  (ocp-indent :with-dev-setup)
  (merlin :with-dev-setup)
  (ocaml-lsp-server :with-dev-setup)
  (utop :with-dev-setup)))
