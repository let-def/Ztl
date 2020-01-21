(lang dune 1.11)
(generate_opam_files true)

(name ztl)
(source (github let-def/ztl))
(license MIT)
(authors "Frédéric Bour")
(maintainers "fred@tarides.com")

(package
 (name ztl)
 (synopsis "Z3 Type Layer: a typed interface to Z3")
 (description "TODO")
 (depends grenier dune))

