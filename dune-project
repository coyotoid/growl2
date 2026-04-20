(lang dune 3.22)

(using menhir 3.0)

(name growl)

(generate_opam_files true)

(source
 (codeberg growl/growl))

(authors "Javier B. Torres <lobo@quiltro.org>")

(maintainers "Javier B. Torres <lobo@quiltro.org>")

(license LICENSE)

(documentation https://quiltro.org/projects/growl/)

(package
 (name growl)
 (synopsis "An experimental programming language")
 (description
  "An experimental, statically typed, stack-based programming language.")
 (depends ocaml)
 (tags (langdev)))
