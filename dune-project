(lang dune 3.15)

(name connect_4)

(generate_opam_files true)

(source
 (github kiblitz/connect-4))

(authors "glee")

(maintainers "glee")

(license LICENSE)

(documentation https://github.com/kiblitz/connect-4)

(package
 (name connect_4)
 (synopsis "Connect 4 with a cli frontend")
 (description "Connect 4 with a cli frontend")
 (depends ocaml dune)
 (tags
  (cli game)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
