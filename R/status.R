# Status and debugging --------------------------------------------------------

# Emit a debug line when debug logging is on (option flowr.debug / FLOWR_DEBUG).
.flowr_log <- function(...) {
  if (isTRUE(flowr_option("debug"))) {
    message("[flowr] ", ...)
  }
}

# Time a command: returns a nullary function that, when called (typically from
# on.exit), prints the elapsed wall-clock time if option flowr.timing is on. It
# is reentrant-safe: only the outermost command reports, so wrappers that call
# other timed commands (e.g. inspect_project -> query) print a single line. A
# command may record flowR's own per-phase timings via .flowr_timing_detail();
# they are appended to the line so it is clear a slice entails parse/dataflow/etc.
.flowr_timer <- function(label) {
  if (isTRUE(.flowr_state$timing_active)) {
    return(function() invisible())
  }
  .flowr_state$timing_active <- TRUE
  .flowr_state$timing_detail <- NULL
  start <- Sys.time()
  function() {
    .flowr_state$timing_active <- NULL
    detail <- .flowr_state$timing_detail
    .flowr_state$timing_detail <- NULL
    if (isTRUE(flowr_option("timing"))) {
      ms <- as.numeric(difftime(Sys.time(), start, units = "secs")) * 1000
      line <- sprintf("[flowr] %s: %.0f ms", label, ms)
      if (!is.null(detail) && nzchar(detail)) {
        line <- paste0(line, "  [flowR: ", detail, "]")
      }
      message(line)
    }
  }
}

# Record flowR's per-phase timings (a named numeric of ms) for the current
# command, so the timing line can show the breakdown. No-op if timing is off.
.flowr_timing_detail <- function(phases) {
  if (!isTRUE(flowr_option("timing")) || length(phases) == 0) {
    return(invisible())
  }
  phases <- phases[!vapply(phases, is.null, logical(1))]
  .flowr_state$timing_detail <-
    paste(sprintf("%s %gms", names(phases), as.numeric(unlist(phases))), collapse = " + ")
  invisible()
}

# Pull flowR's analysis phase timings (parse, normalize, dataflow) from an
# analysis result, in order. Missing phases are dropped.
.flowr_analysis_phases <- function(analysis) {
  r <- analysis$results
  ph <- list()
  for (p in c("parse", "normalize", "dataflow")) {
    t <- r[[p]]$.meta$timing
    if (is.numeric(t) && length(t) == 1L) {
      ph[[p]] <- t
    }
  }
  ph
}

# Transient status line (e.g. while an engine starts). Only on an interactive
# terminal, and off under options(flowr.progress = FALSE) / flowr.quiet.
.flowr_progress_on <- function() {
  isTRUE(flowr_option("progress")) && !isTRUE(flowr_option("quiet")) &&
    interactive() && isTRUE(tryCatch(isatty(stdout()), error = function(e) FALSE))
}

.flowr_progress_start <- function(msg) {
  if (!.flowr_progress_on()) {
    return(FALSE)
  }
  cat(.flowr_ansi(msg, "90", .flowr_use_color()))
  utils::flush.console()
  TRUE
}

# Erase the transient line (carriage return + clear to end of line).
.flowr_progress_clear <- function(shown) {
  if (isTRUE(shown)) {
    cat("\r\x1b[K")
    utils::flush.console()
  }
  invisible(NULL)
}

#' Read the flowR server's log
#'
#' Returns the log output (stdout and stderr) of the flowR server backing a
#' session, mostly for debugging. For detailed flowR logging, start the server
#' with `flowr_set_config(verbose = TRUE)` (or `options(flowr.verbose = TRUE)`)
#' before the first call, then read it here.
#'
#' @param session A `flowr_session`; the active default session when `NULL`.
#' @param n Number of trailing lines to return (`Inf` for the whole log).
#' @return A `flowr_log` (a character vector of log lines); its `file` attribute
#'   is the log path. Printing shows the lines.
#' @seealso [flowr_status()], [flowr_config()]
#' @export
#' @examples
#' \dontrun{
#' flowr_set_config(verbose = TRUE)
#' slice("x <- 1\ncat(x)", "2@x")
#' flowr_log()
#' }
flowr_log <- function(session = NULL, n = 200L) {
  s <- session %||% .flowr_state$default
  if (is.null(s) || !isTRUE(is_flowr_session(s)) || isTRUE(s$closed)) {
    stop("no active flowR session; run a command or flowr_connect() first.",
         call. = FALSE)
  }
  log <- s$handle$log
  if (is.null(log) || is.na(log) || !file.exists(log)) {
    message("[flowr] no server log for this session (engine: ",
            s$handle$provider, ").")
    return(structure(character(0), class = "flowr_log", file = NA_character_))
  }
  lines <- readLines(log, warn = FALSE)
  if (is.finite(n)) {
    lines <- utils::tail(lines, n)
  }
  structure(lines, class = "flowr_log", file = log)
}

#' @export
print.flowr_log <- function(x, ...) {
  f <- attr(x, "file")
  if (length(x) == 0) {
    cat("<flowr_log: empty",
        if (!is.null(f) && !is.na(f)) paste0(" (", f, ")") else "", ">\n", sep = "")
    return(invisible(x))
  }
  cat(unclass(x), sep = "\n")
  cat("\n")
  invisible(x)
}

# A short "3 days ago" style age for a timestamp, or NA.
.flowr_age <- function(t) {
  if (is.null(t) || length(t) != 1L || is.na(t)) {
    return(NA_character_)
  }
  secs <- as.numeric(difftime(Sys.time(), t, units = "secs"))
  if (secs < 60) return("just now")
  units <- list(c(31536000, "year"), c(2592000, "month"), c(86400, "day"),
                c(3600, "hour"), c(60, "min"))
  for (u in units) {
    n <- floor(secs / as.numeric(u[[1]]))
    if (n >= 1) {
      return(sprintf("%d %s%s ago", n, u[[2]], if (n == 1 || u[[2]] == "min") "" else "s"))
    }
  }
  "just now"
}

#' Report flowr's status: engine, configuration and active session
#'
#' A human-friendly overview of which engine flowr is using (or would use), the
#' effective configuration, which engines are cached, the config file in effect,
#' and the active session, if any. For the raw configuration as a list see
#' [flowr_config()].
#'
#' @return An object of class `flowr_status` (invisibly printed as a report).
#' @seealso [flowr_config()]
#' @export
#' @examples
#' flowr_status()
flowr_status <- function() {
  cfg <- flowr_config()
  fv <- cfg$flowr_version
  s <- .flowr_state$default
  active <- if (!is.null(s) && isTRUE(is_flowr_session(s)) && !isTRUE(s$closed)) {
    list(provider = s$handle$provider, host = s$handle$host, port = s$handle$port,
         flowr = s$versions$flowr %||% NA, engine = s$versions$engine %||% NA,
         log = s$handle$log)
  } else {
    NULL
  }
  # when a session is live, ask flowR for the config it is actually running with
  backend <- NULL
  if (!is.null(active) && !is.null(s$con)) {
    # query() returns results keyed by query type; flowR's config-query result
    # wraps the settings under its own `config` field (next to `.meta`).
    fc <- tryCatch(query("flowr_status_probe <- 1", "config")$config, error = function(e) NULL)
    if (!is.null(fc$config)) {
      fc <- fc$config
    }
    if (!is.null(fc)) {
      backend <- list(
        r = s$versions$r %||% NA_character_,
        default_engine = fc$defaultEngine %||% NA_character_,
        plugins = if (is.null(fc$defaultPlugins)) NA_integer_ else length(fc$defaultPlugins),
        solvers = if (is.null(fc$solver)) NA_integer_ else length(fc$solver)
      )
    }
  }
  structure(
    list(
      version = as.character(utils::packageVersion("flowr")),
      config = cfg,
      config_file = flowr_config_file(),
      # readiness of every registered engine, derived from the registry so no
      # engine name is hardcoded here (engine knowledge stays in engine.R)
      engines = {
        specs <- .flowr_engine_specs()
        stats::setNames(
          lapply(specs, function(s) isTRUE(tryCatch(s$ready(fv), error = function(e) FALSE))),
          names(specs))
      },
      cache_dir = .flowr_cache_dir(),
      resolved_engine = tryCatch(.flowr_resolve_engine(cfg$engine, fv),
                                 error = function(e) NA_character_),
      binary_verification = .flowr_binary_verification(fv),
      binary_installed_at = .flowr_binary_mtime(fv),
      binary_hash = .flowr_binary_hash(fv),
      session = active,
      backend = backend
    ),
    class = "flowr_status"
  )
}

#' @export
print.flowr_status <- function(x, ...) {
  width <- min(getOption("width", 80L), 72L)
  color <- .flowr_use_color()
  yn <- function(b) if (isTRUE(b)) "yes" else "no"
  rule <- function(title = NULL) {
    if (is.null(title)) {
      cat(strrep("-", width), "\n", sep = "")
    } else {
      cat("-- ", title, " ", strrep("-", max(0, width - nchar(title) - 4)), "\n", sep = "")
    }
  }
  row <- function(label, value) cat(sprintf("  %-13s %s\n", label, value))

  cat(strrep("=", width), "\n", sep = "")
  title <- sprintf("flowR R adapter %s", x$version)
  cat("  ", title, sprintf("%*s\n", max(1L, width - nchar(title) - 9L), "status"), sep = "")

  rule("engine")
  if (!is.null(x$session)) {
    row("using", sprintf("%s  (flowR %s, %s)",
                         x$session$provider, x$session$flowr, x$session$engine))
    row("address", sprintf("%s:%s", x$session$host, x$session$port))
  } else {
    row("using", sprintf("none active yet -> '%s' starts on first call",
                         x$resolved_engine))
  }
  row("configured", x$config$engine)
  row("ready", paste(sprintf("%s %s", names(x$engines),
                             vapply(x$engines, yn, character(1))), collapse = " / "))
  # binary integrity: green if signature-verified, yellow if checksum-only (no
  # signature), red if unverified; with install age and the verified hash
  if (!is.na(x$binary_verification)) {
    v <- switch(x$binary_verification,
      signature = list("signature-verified", "32"),
      checksum  = list("checksum only (no signature)", "33"),
      list("unverified", "31"))
    age <- .flowr_age(x$binary_installed_at)
    val <- .flowr_ansi(v[[1]], v[[2]], color)
    if (!is.na(age)) {
      val <- paste0(val, .flowr_ansi(sprintf("  (installed %s)", age), "90", color))
    }
    row("binary", val)
    if (!is.na(x$binary_hash)) {
      row("sha256", .flowr_ansi(x$binary_hash, "90", color))
    }
  }

  rule("flowR")
  row("version", if (!is.null(x$session)) x$session$flowr else x$config$flowr_version)
  row("parser", x$config$flowr_engine)
  row("secure", yn(x$config$secure))
  if (!is.null(x$backend)) {
    row("R (flowR)", x$backend$r)
    row("flowR config", sprintf("default-engine %s, %s plugins, %s solvers",
                                x$backend$default_engine, x$backend$plugins, x$backend$solvers))
  }

  rule("runtime")
  row("debug", if (isTRUE(x$config$debug)) "on" else "off")
  row("timeouts", sprintf("connect %ss / request %ss",
                          x$config$connect_timeout, x$config$request_timeout))
  row("cache dir", x$cache_dir)
  row("config file", if (is.na(x$config_file)) "(none)" else x$config_file)
  if (!is.null(x$session) && !is.null(x$session$log) && !is.na(x$session$log)) {
    row("server log", x$session$log)
  }
  cat(strrep("=", width), "\n", sep = "")
  invisible(x)
}
