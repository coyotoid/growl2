(lang dune 3.22)

(using menhir 3.0)

(name growl)

(generate_opam_files true)

(source
 (codeberg growl/growl))

(authors "Javier B. Torres <lobo@quiltro.org>")

(maintainers "Javier B. Torres <lobo@quiltro.org>")

(license BSD-3-Clause)

(documentation https://quiltro.org/projects/growl/)

(package
 (name growl)
 (synopsis "An experimental programming language")
 (description
  "Growl is an experimental, statically typed, stack-based programming language.")
 (depends
  ocaml
  (containers (>= 3.18))
  (fmt (>= 0.11.0))
  (cmdliner (>= 2.1.0))
  (hashcons (>= 1.4.0))
  (ppx_deriving (>= 6.1.1))
  (alcotest (and :with-test (>= 1.9.1))))
 (tags (langdev concatenative)))
