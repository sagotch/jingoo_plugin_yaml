(lang dune 3.0)

(name jingoo_plugin_yaml)

(generate_opam_files true)

(source (github sagotch/jingoo_plugin_yaml))

(authors "Julien Sagot <contact@sagotch.fr>")

(maintainers "Julien Sagot <contact@sagotch.fr>")

(license MIT)

(package
  (name jingoo_plugin_yaml)
  (synopsis "Convert jingoo values from/to YAML strings")
  (depends ocaml dune jingoo yaml)
  (allow_empty)
)
