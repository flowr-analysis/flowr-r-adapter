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

# Emit a transient, informational message at most `flowr.message_limit` times
# per R session, keyed by `id`. The counter lives in `.flowr_state`, so it is
# session-only and resets in a fresh session (no state is written to disk). It
# is fully silenced by `options(flowr.quiet = TRUE)` or a limit of 0, and lets
# recurring hints (the startup banner, the "no binary installed" note) stop
# nagging once they have been seen. Never signals an error, so it is safe to
# call from `.onAttach` and hot paths.
.flowr_notify <- function(id, ..., startup = FALSE, limit = NULL) {
  tryCatch({
    if (isTRUE(flowr_option("quiet"))) {
      return(invisible(FALSE))
    }
    if (is.null(limit)) {
      limit <- flowr_option("message_limit")
    }
    limit <- suppressWarnings(as.integer(limit))
    if (length(limit) != 1L || is.na(limit) || limit <= 0L) {
      return(invisible(FALSE))
    }
    counts <- .flowr_state$msg_counts
    if (is.null(counts)) {
      counts <- list()
    }
    n <- (counts[[id]] %||% 0L) + 1L
    counts[[id]] <- n
    .flowr_state$msg_counts <- counts
    if (n > limit) {
      return(invisible(FALSE))
    }
    msg <- paste0(...)
    # If a transient progress line is still on screen (e.g. the engine is
    # starting), end it first so this message lands on its own clean line rather
    # than being glued onto "[flowr] starting the flowR engine ...".
    .flowr_progress_clear()
    if (isTRUE(startup)) packageStartupMessage(msg) else message(msg)
    invisible(TRUE)
  }, error = function(e) invisible(FALSE))
}

.onAttach <- function(libname, pkgname) {
  color <- .flowr_use_color()
  # One short banner line (coloured name + version so it reads clearly as info),
  # with the getting-started pointer on the same line. Shown only the *first*
  # load per session (limit 1) so it does not nag on every library(flowr).
  # Only point at the vignette when it is actually installed: installing from a
  # source dir (R CMD INSTALL ., RStudio "Install", rv, ...) does not build
  # vignettes, so referencing one unconditionally would send users to a
  # "vignette not found". Use `R CMD build` + install the tarball, or
  # devtools::install(build_vignettes = TRUE).
  has_vig <- isTRUE(tryCatch(
    "flowr" %in% utils::vignette(package = "flowr")$results[, "Item"],
    error = function(e) FALSE))
  guide <- if (has_vig) "vignette(\"flowr\")" else "?flowr"
  .flowr_notify(
    "startup-banner", startup = TRUE, limit = 1L,
    .flowr_ansi(paste0(
      "flowr ", utils::packageVersion("flowr"),
      " loaded. Getting started: ", guide, "."), "90", color)
  )
  # If no self-contained binary is present, say how to get one and why: flowr
  # still works via the shipped bundle, but that needs Node.js on the PATH.
  no_binary <- !isTRUE(tryCatch(.flowr_binary_installed(),
                                error = function(e) FALSE))
  if (no_binary) {
    .flowr_notify("no-binary", startup = TRUE, .flowr_no_binary_msg())
  }
}
