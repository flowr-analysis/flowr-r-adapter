#' flowr: Call flowR from R
#'
#' `flowr` is a thin, robust client for
#' [flowR](https://github.com/flowr-analysis/flowr), a static dataflow analyzer
#' and program slicer for R.  It talks to a flowR *server* over a simple,
#' newline-delimited JSON protocol and can obtain that server through several
#' interchangeable engines (the JS+wasm bundle shipped in the package, a
#' self-contained downloaded binary, a private Node.js install, or Docker), so
#' that in the common case `slice()`, `query()` and friends "just work" without
#' the user installing anything by hand.
#'
#' See `vignette("flowr")` to get started and `vignette("flowr-engines")` for
#' engine selection, the query API and the REPL.
#'
#' @keywords internal
"_PACKAGE"

# Internal, per-session package state (never written to by users directly).
# Holds the lazily created default session and the id counter.
.flowr_state <- new.env(parent = emptyenv())

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "flowr ", utils::packageVersion("flowr"),
    ": slice(code, criterion), flowr_overview(), query(), flowr_repl(). ",
    "flowr_install() adds the full engine; see vignette(\"flowr\")."
  )
}
