# Sessions --------------------------------------------------------------------
#
# A session bundles a started engine, an open socket, and a *bounded* cache of
# per-content analyses.  Keeping one warm session and reusing its server + cached
# analyses across calls is what makes repeated `slice()`/`query()` fast (no
# per-call process spawn, no re-analysis of unchanged code).  Every resource a
# session owns is released by `flowr_disconnect()` so nothing leaks.

#' Start a flowR engine and connect to it
#'
#' Obtains a running flowR server through the selected engine and opens a client
#' connection to it.  The returned session is passed to [slice()], [query()],
#' [flowr_repl()], etc., or left implicit (they use a lazily created default
#' session when none is supplied).
#'
#' @param engine Which engine to use: `"auto"` (default), `"bundled"` (the flowR
#'   JS+wasm shipped with the package, run on your Node), `"binary"`
#'   (self-contained, no Node), `"node"` or `"docker"`.
#' @param flowr_version flowR version to obtain (default `"2.11.1"`).
#' @param flowr_engine flowR parser engine, `"tree-sitter"` (default, needs no R)
#'   or `"r-shell"` (reuses the R on your `PATH`).
#' @param host,port Bind host (loopback) and preferred port for the spawned
#'   server; a free port is chosen automatically if the preferred one is taken.
#' @param ws Use a WebSocket transport instead of raw TCP.
#' @param quiet Suppress progress messages.
#' @return An object of class `flowr_session`.
#' @seealso [flowr_disconnect()], [slice()], [query()]
#' @export
#' @examples
#' \dontrun{
#' s <- flowr_connect()
#' slice("x <- 1\ny <- 2\nprint(x)", "3@x")
#' flowr_disconnect(s)
#' }
flowr_connect <- function(engine = flowr_option("engine"),
                          flowr_version = flowr_option("flowr_version"),
                          flowr_engine = flowr_option("flowr_engine"),
                          host = flowr_option("host"),
                          port = flowr_option("port"),
                          ws = flowr_option("ws"),
                          quiet = flowr_option("quiet")) {
  shown <- .flowr_progress_start("[flowr] starting the flowR engine ...")
  on.exit(.flowr_progress_clear(shown), add = TRUE)
  handle <- .flowr_start_engine(engine, flowr_version, flowr_engine, host, port, ws, quiet)

  # If connecting or the handshake fails, stop the engine we just started so no
  # process or connection is leaked.
  con <- tryCatch(.flowr_connect_socket(handle$host, handle$port),
                  error = function(e) { .flowr_stop_engine(handle); stop(e) })
  hello <- tryCatch(
    .flowr_parse(.flowr_read_message(con, .flowr_reader_for(con),
                                     flowr_option("connect_timeout"))),
    error = function(e) {
      tryCatch(.flowr_close(con), error = function(.) NULL)
      .flowr_stop_engine(handle)
      stop(e)
    }
  )
  .flowr_make_session(handle, con, hello)
}

# Build a flowr_session and make it the current default. Previously created
# sessions stay open (you may hold several and pass them via `session =`); each
# is closed by flowr_disconnect(), and any we started is killed on unload.
.flowr_make_session <- function(handle, con, hello) {
  session <- new.env(parent = emptyenv())
  session$con <- con
  session$handle <- handle
  session$hello <- hello
  session$versions <- hello$versions %||% list()
  session$cache <- new.env(parent = emptyenv())
  session$cache_order <- character(0)
  session$next_id <- 0L
  session$closed <- FALSE
  class(session) <- "flowr_session"
  .flowr_state$default <- session
  .flowr_log("connected: ", handle$provider, " @ ", handle$host %||% "-", ":",
             handle$port %||% "-", " (flowR ", session$versions$flowr %||% "?", ")")
  session
}

# Internal: build a session around an already-running server (used by tests to
# drive the client against a controllable mock; not a user-facing feature).
.flowr_connect_to <- function(host, port) {
  handle <- .flowr_new_handle("external", host, port, owns = FALSE)
  con <- .flowr_connect_socket(host, port)
  hello <- .flowr_parse(.flowr_read_message(con, .flowr_reader_for(con),
                                            flowr_option("connect_timeout")))
  .flowr_make_session(handle, con, hello)
}

# Open the client socket to a (freshly started) server with a short retry.
.flowr_connect_socket <- function(host, port, attempts = 25) {
  for (i in seq_len(attempts)) {
    con <- tryCatch(suppressWarnings(.flowr_open_socket(host, port)),
                    error = function(e) NULL)
    if (!is.null(con)) {
      return(con)
    }
    Sys.sleep(0.1)
  }
  .flowr_stop("could not connect to flowR server at ", host, ":", port)
}

#' Close the flowR session and release every resource it owns
#'
#' Closes the socket, drops the connection's read buffer, and stops the engine
#' process if the session started it. Safe to call more than once.
#'
#' @param session The session to close; defaults to the central session.
#' @return `TRUE`, invisibly.
#' @export
#' @examples
#' \dontrun{
#' slice("x <- 1\ncat(x)", "2@x")  # opens the shared session
#' flowr_disconnect()               # close it and stop the engine
#' }
flowr_disconnect <- function(session = .flowr_state$default) {
  if (is.null(session) || isTRUE(session$closed)) {
    return(invisible(TRUE))
  }
  tryCatch(.flowr_close(session$con), error = function(e) NULL)
  .flowr_stop_engine(session$handle)
  # drop cache contents so a lingering reference to the session frees memory
  rm(list = ls(session$cache), envir = session$cache)
  session$cache_order <- character(0)
  session$closed <- TRUE
  # if this was the default session, forget it
  if (identical(.flowr_state$default, session)) {
    .flowr_state$default <- NULL
  }
  invisible(TRUE)
}

#' @export
print.flowr_session <- function(x, ...) {
  st <- if (isTRUE(x$closed)) "closed" else "open"
  cat(sprintf("<flowr_session: %s via %s at %s:%s (flowR %s)>\n",
              st, x$handle$provider, x$handle$host, x$handle$port,
              x$versions$flowr %||% "?"))
  invisible(x)
}

#' Is this a flowR session?
#' @param x Object to test.
#' @return `TRUE`/`FALSE`.
#' @export
#' @examples
#' is_flowr_session(1)              # FALSE
#' \dontrun{
#' is_flowr_session(flowr_connect())
#' }
is_flowr_session <- function(x) inherits(x, "flowr_session")

# Default (implicit) session --------------------------------------------------

#' The central flowR session
#'
#' Returns the single shared session, creating one on first use. The high-level
#' helpers all use it, so `library(flowr); slice(code, criterion)` works with no
#' setup. Call [flowr_connect()] to (re)create it with a specific engine.
#'
#' @param ... Passed to [flowr_connect()] when the session is first created.
#' @return A `flowr_session`.
#' @export
#' @examples
#' \dontrun{
#' s <- flowr_default_session()     # the shared session (created on first use)
#' is_flowr_session(s)
#' }
flowr_default_session <- function(...) {
  s <- .flowr_state$default
  if (!is.null(s) && !isTRUE(s$closed)) {
    return(s)
  }
  flowr_connect(...)
}

# Resolve a session argument: NULL -> the central default; otherwise validate.
.flowr_resolve_session <- function(session) {
  if (is.null(session)) {
    return(flowr_default_session())
  }
  if (!is_flowr_session(session)) {
    .flowr_stop("`session` must be a flowr_session (from flowr_connect())")
  }
  if (isTRUE(session$closed)) {
    .flowr_stop("this flowr_session is closed")
  }
  session
}

.flowr_session_id <- function(session) {
  session$next_id <- session$next_id + 1L
  as.character(session$next_id)
}

# Analysis with a bounded, content-keyed cache --------------------------------

# Analyse R code once and cache it per content (the main performance lever): the
# server keeps the file under its filetoken, so repeated slice/query on the same
# content reuse it. Internal; the high-level helpers call this for you.
flowr_analyze <- function(code = NULL, file = NULL, files = NULL, cfg = FALSE,
                          session = NULL) {
  session <- .flowr_resolve_session(session)
  if (is.null(code) && is.null(file) && is.null(files)) {
    .flowr_stop("provide `code`, `file` or `files`")
  }
  paths <- if (!is.null(files)) normalizePath(files, mustWork = TRUE)
           else if (!is.null(file)) normalizePath(file, mustWork = TRUE)
           else NULL
  key <- if (!is.null(paths)) paste0("path:", paste(sort(paths), collapse = ";"))
         else paste0("code:", code)
  hit <- session$cache[[key]]
  if (!is.null(hit) && identical(hit$cfg, cfg)) {
    return(hit)
  }
  filetoken <- paste0("ft", .flowr_session_id(session))
  req <- if (!is.null(paths)) {
    list(type = "request-file-analysis", id = .flowr_session_id(session),
         filetoken = filetoken, filepath = if (length(paths) > 1) I(paths) else paths,
         format = "json", cfg = cfg)
  } else {
    list(type = "request-file-analysis", id = .flowr_session_id(session),
         filetoken = filetoken, content = code, format = "json", cfg = cfg)
  }
  res <- .flowr_request(session$con, req)
  entry <- list(filetoken = filetoken, analysis = res, cfg = cfg)
  .flowr_cache_put(session, key, entry)
  entry
}

.flowr_cache_put <- function(session, key, entry) {
  if (is.null(session$cache[[key]])) {
    session$cache_order <- c(session$cache_order, key)
  }
  session$cache[[key]] <- entry
  cap <- flowr_option("cache_size")
  while (length(session$cache_order) > cap) {
    old <- session$cache_order[[1]]
    session$cache_order <- session$cache_order[-1]
    if (!is.null(session$cache[[old]])) {
      rm(list = old, envir = session$cache)
    }
  }
  invisible(entry)
}

#' Clear a session's analysis cache
#' @param session A `flowr_session`; the central default is used when `NULL`.
#' @return `NULL`, invisibly.
#' @export
#' @examples
#' \dontrun{
#' flowr_clear_cache()              # drop cached analyses for the active session
#' }
flowr_clear_cache <- function(session = NULL) {
  session <- .flowr_resolve_session(session)
  rm(list = ls(session$cache), envir = session$cache)
  session$cache_order <- character(0)
  invisible(NULL)
}
