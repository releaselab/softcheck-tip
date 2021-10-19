(lang dune 2.8)

(name softcheck_tip)

(generate_opam_files true)

(source
 (uri "git+https://github.com/releaselab/softcheck-tip.git"))

(bug_reports "https://github.com/releaselab/softcheck-tip/issues")

(homepage "https://github.com/releaselab/softcheck-tip")

(license MIT)

(authors "Jo\195\163o Santos Reis <joao.reis@ubi.pt>")

(maintainers "Jo\195\163o Santos Reis <joao.reis@ubi.pt>")

(package
 (name softcheck_tip)
 (synopsis "An example implementation of TIP for the Softcheck platform")
 (depends
  (dune :build)
  softcheck
  softcheck-analysis
  menhir
  fmt
  cmdliner))

(using menhir 2.1)
