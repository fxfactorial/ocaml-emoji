(lang dune 3.2)
(using mdx 0.3)
(name emoji)
(explicit_js_mode)
(generate_opam_files true)
(license BSD-3-Clause)
(authors "Edgar Aroutiounian <edgar.factorial@gmail.com>")
(maintainers "Edgar Aroutiounian <edgar.factorial@gmail.com>" "Swrup <swrup@protonmail.com>")
(source (github fxfactorial/ocaml-emoji))
(package
  (name emoji)
  (synopsis "Use emojis by name")
  (description "OCaml library providing byte sequences of all the Unicode emoji characters and sequences sourced from unicode.org" )
  (tags (emoji unicode))
  (depends
    (ocaml (>= "4.04"))
    (mdx :with-test)
    (lwt :with-test)
    (tls-lwt :with-test)
    (cohttp-lwt-unix :with-test)
    (lambdasoup :with-test)
    (uutf :with-test)
  ))
