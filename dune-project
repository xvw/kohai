(lang dune 3.17)

(name kohai)

(version dev)

(generate_opam_files true)

(executables_implicit_empty_intf)

(using menhir 3.0)

(using mdx 0.4)

(source
 (github xvw/kohai))

(license MIT)

(authors "Xavier Van de Woestyne <xaviervdw@gmail.com>")

(maintainers "Xavier Van de Woestyne <xaviervdw@gmail.com>")

(package
 (name rensai)
 (synopsis "Rensai can be used to describe generic encoders and decoders")
 (description
  "By lifting heterogeneous data into a finite set of constructors,\nit is possible to describe languages similar to JSON, TOML or S-Expression,\nfor which the encoding and validation functions become generic\n(and interchangeable).")
 (depends
  (yojson
   (>= 2.2.2))
  (ocaml
   (>= 5.3.0))
  menhir
  (fmt
   (>= 0.9.0))
  ppx_expect
  (mdx :with-test)))

(package
 (name kohai)
 (allow_empty)
 (synopsis "A small hand-crafted tool for entering activities")
 (description
  "A tool for collecting personal activity reports\n(and an excuse to write OCaml and reinvent the wheel)")
 (depends
  (ocaml
   (>= 5.3.0))
  re
  uri
  uuidm
  (rensai
   (= :version))
  ppx_expect
  (mdx :with-test)
  (yojson
   (>= 2.2.2))
  (fmt
   (>= 0.9.0))
  (logs
   (>= 0.7.0))
  (cmdliner
   (>= 1.3.0))
  (eio
   (>= 1.2))
  (eio_main
   (>= 1.2))
  (ocamlformat :with-dev-setup)
  (ocp-indent :with-dev-setup)
  (merlin :with-dev-setup)
  (ocaml-lsp-server :with-dev-setup)
  (utop :with-dev-setup)))