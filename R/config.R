# Configuration ---------------------------------------------------------------
#
# Every knob can be set in three ways, in decreasing precedence:
#   1. an explicit function argument,
#   2. an R option        `options(flowr.<name> = )`,
#   3. an environment var `FLOWR_<NAME>` (upper-cased),
# falling back to the built-in default below.  Keeping this in one place is what
# makes the engine and flowR version selectable without touching code.

.flowr_defaults <- list(
  engine          = "auto",        # auto | bundled | binary | node | docker
  flowr_version   = "2.11.1",      # flowR release to obtain / image tag
  flowr_engine    = "tree-sitter", # flowR parser engine: tree-sitter | r-shell
  host            = "127.0.0.1",
  port            = 1042L,          # used as-is for `remote`; a free port is
                                    # picked automatically for spawned servers
  ws              = FALSE,          # use a WebSocket instead of raw TCP
  connect_timeout = 30,             # seconds to wait for a spawned server
  request_timeout = 120,            # seconds to wait for a single response
  cache_size      = 32L,            # max analyses cached per session (bounded)
  binary_repo     = "flowr-analysis/flowr-r-adapter", # host of binary releases
  docker_image    = "eagleoutice/flowr",
  secure          = TRUE,           # hardened mode (default): tree-sitter only,
                                    # loopback-only remote, checksum-verified
                                    # binaries, no shell, no R interpreter for code
  debug           = FALSE,          # log engine/protocol activity to the console
  verify_signature = TRUE,          # verify a binary's signature (openssl, no
                                    # gpg needed) when a pinned key is shipped
  progress        = TRUE,           # transient "starting engine" hint (cleared)
  message_limit   = 3L,             # show each transient hint (startup banner,
                                    # "no binary installed", ...) at most this
                                    # many times per R session; 0 mutes them
  timing          = FALSE,          # print each command's wall-clock time
  verbose         = FALSE,          # run the flowR server/REPL with --verbose
  quiet           = FALSE,
  lint_rules      = character(0),   # active flowr_lint() rules; empty = flowR's
                                    # full default set (as the VS Code extension)
  lint_max        = 10L             # findings shown per rule unless full = TRUE
)

# Read a single configuration value, resolving (in order) an explicit `value`,
# the R option `flowr.<name>`, the environment variable `FLOWR_<NAME>`, the JSON
# config file, and finally the package default. Internal; users read settings
# via `flowr_config()` / `getOption()`.
flowr_option <- function(name, value = NULL) {
  if (!is.null(value)) {
    return(value)
  }
  if (!name %in% names(.flowr_defaults)) {
    .flowr_stop("unknown flowr option: ", name)
  }
  opt <- getOption(paste0("flowr.", name))
  if (!is.null(opt)) {
    return(opt)
  }
  env <- Sys.getenv(paste0("FLOWR_", toupper(name)), unset = NA_character_)
  if (!is.na(env) && nzchar(env)) {
    return(.flowr_coerce(env, .flowr_defaults[[name]]))
  }
  json <- .flowr_config_json()
  if (!is.null(json[[name]])) {
    val <- json[[name]]
    if (is.character(val) && !is.character(.flowr_defaults[[name]])) {
      return(.flowr_coerce(val, .flowr_defaults[[name]]))
    }
    return(val)
  }
  .flowr_defaults[[name]]
}

# JSON config file ------------------------------------------------------------
#
# Settings may also live in a `flowr.json` file whose keys are option names
# (e.g. {"engine": "binary", "flowr_version": "2.11.1", "secure": true}). It is
# looked up at `options(flowr.config_file=)`, then `$FLOWR_CONFIG`, then
# `flowr.json` in the working directory, then a per-user config file. Options
# and environment variables still take precedence over it.

#' Path of the active flowr JSON config file, if any
#'
#' @return The resolved config file path, or `NA` when none is found.
#' @export
#' @examples
#' flowr_config_file()   # path to the active flowr.json, or NA when none
flowr_config_file <- function() {
  p <- getOption("flowr.config_file")
  if (!is.null(p)) {
    return(p)
  }
  env <- Sys.getenv("FLOWR_CONFIG", unset = NA_character_)
  if (!is.na(env) && nzchar(env)) {
    return(env)
  }
  cwd <- file.path(getwd(), "flowr.json")
  if (file.exists(cwd)) {
    return(cwd)
  }
  user <- file.path(tools::R_user_dir("flowr", "config"), "config.json")
  if (file.exists(user)) {
    return(user)
  }
  NA_character_
}

# Load (and cache, invalidated by mtime) the JSON config file.
.flowr_config_json <- function() {
  path <- flowr_config_file()
  if (is.na(path) || !file.exists(path)) {
    return(list())
  }
  mtime <- file.info(path)$mtime
  if (!is.null(.flowr_state$config_json) &&
      identical(.flowr_state$config_json_path, path) &&
      identical(.flowr_state$config_json_mtime, mtime)) {
    return(.flowr_state$config_json)
  }
  parsed <- tryCatch(
    jsonlite::fromJSON(path, simplifyVector = TRUE),
    error = function(e) {
      .flowr_warn("could not read flowr config file ", path, ": ",
                  conditionMessage(e))
      list()
    }
  )
  if (!is.list(parsed)) {
    parsed <- as.list(parsed)
  }
  .flowr_state$config_json <- parsed
  .flowr_state$config_json_path <- path
  .flowr_state$config_json_mtime <- mtime
  parsed
}

# Coerce an environment-variable string to the type of the default value.
.flowr_coerce <- function(x, template) {
  if (is.logical(template)) {
    return(isTRUE(x %in% c("1", "true", "TRUE", "yes", "on")))
  }
  if (is.integer(template)) {
    return(as.integer(x))
  }
  if (is.numeric(template)) {
    return(as.numeric(x))
  }
  x
}

# Settings grouped for display. The "server" group only takes effect when flowr
# starts or looks up a flowR server; it is irrelevant for local analysis, so the
# printer marks it as such instead of implying a server is running.
.flowr_config_groups <- list(
  general = c("engine", "flowr_version", "flowr_engine", "secure", "verify_signature"),
  server  = c("host", "port", "ws", "connect_timeout", "request_timeout",
              "docker_image", "binary_repo"),
  runtime = c("cache_size", "progress", "message_limit", "timing", "verbose",
              "quiet", "debug", "lint_rules", "lint_max")
)

#' Inspect the effective flowr configuration
#'
#' Resolves every configuration knob to its currently effective value. Each knob
#' `<name>` is resolved, in decreasing precedence, from: a function argument, the
#' R option `options(flowr.<name> = )`, the environment variable `FLOWR_<NAME>`,
#' the JSON config file ([flowr_config_file()]), then the package default.
#'
#' Any of those set a value and are fully supported. `options(flowr.<name> = )`
#' is perfectly fine for interactive use; [flowr_set_config()] is a convenience
#' that does it for you (and can persist), and you may also assign into the
#' returned object (`cfg$engine <- "binary"`), which sets the option.
#'
#' @return A named list (class `flowr_config`) with every configuration value as
#'   currently resolved. Printing groups the values and marks the server-only
#'   ones (which apply only when flowr starts or connects to a flowR server) and
#'   the live transport. Assigning into it (`cfg$name <- value`) sets the option.
#' @seealso [flowr_set_config()], [flowr_status()]
#' @export
#' @examples
#' flowr_config()
#' flowr_config()$engine
#' \dontrun{
#' cfg <- flowr_config()
#' cfg$engine <- "binary"          # same as options(flowr.engine = "binary")
#' options(flowr.request_timeout = 300)
#' }
flowr_config <- function() {
  res <- lapply(names(.flowr_defaults), flowr_option)
  names(res) <- names(.flowr_defaults)
  structure(res, class = "flowr_config")
}

# How flowr talks to flowR right now. It runs flowR as a local server and
# connects over a socket, so this reports the socket mode and whether a server
# is currently connected (and via which engine), marking the live mode.
.flowr_transport <- function(x) {
  base <- if (isTRUE(x$ws)) "server (WebSocket, loopback)" else "server (TCP, loopback)"
  s <- .flowr_state$default
  if (!is.null(s) && isTRUE(is_flowr_session(s)) && !isTRUE(s$closed)) {
    paste0(base, " - connected via ", s$handle$provider,
           " at ", s$handle$host, ":", s$handle$port)
  } else {
    paste0(base, " - not connected (starts on first call)")
  }
}

# The engine in effect: the connected session's provider, else the engine `auto`
# would resolve to. Used to gray out settings the current engine does not use.
.flowr_active_engine <- function(x) {
  s <- .flowr_state$default
  if (!is.null(s) && isTRUE(is_flowr_session(s)) && !isTRUE(s$closed)) {
    return(s$handle$provider)
  }
  tryCatch(.flowr_resolve_engine(x$engine, flowr_option("flowr_version")),
           error = function(e) x$engine)
}

# Settings that only apply to one engine; grayed out when another engine is
# active (e.g. docker_image on the binary engine).
.flowr_engine_only <- c(docker_image = "docker", binary_repo = "binary")

#' @export
print.flowr_config <- function(x, ...) {
  color <- .flowr_use_color()
  width <- min(getOption("width", 80L), 72L)
  fmt <- function(v) {
    if (length(v) == 0) return("(flowR defaults)")           # e.g. empty lint_rules
    if (is.logical(v)) return(tolower(as.character(v)))
    paste(as.character(v), collapse = ", ")                  # join vector-valued knobs
  }
  active <- .flowr_active_engine(x)
  # a field is unused (dim) only if it is specific to a different engine
  unused <- function(k) {
    e <- unname(.flowr_engine_only[k])
    !is.na(e) && !identical(e, active)
  }
  head <- function(title) {
    cat(.flowr_ansi(paste0("-- ", title, " ", strrep("-", max(0, width - nchar(title) - 4))),
                    "2", color), "\n", sep = "")
  }
  row <- function(k, v, dim = FALSE) {
    cat(sprintf("  %-17s %s\n", k, if (dim) .flowr_ansi(fmt(v), "90", color) else fmt(v)))
  }
  cat(.flowr_ansi("flowr configuration", "1", color), "\n", sep = "")
  seen <- character(0)
  any_dim <- FALSE
  for (grp in names(.flowr_config_groups)) {
    keys <- intersect(.flowr_config_groups[[grp]], names(x))
    head(grp)
    if (grp == "general") {
      for (k in keys) row(k, x[[k]])
      row("transport", .flowr_transport(x))     # derived: marks that a server is used
    } else {
      for (k in keys) {
        d <- unused(k)
        any_dim <- any_dim || d
        row(k, x[[k]], dim = d)
      }
    }
    seen <- c(seen, keys)
  }
  rest <- setdiff(names(x), seen)                # never hide an unknown setting
  if (length(rest) > 0) {
    head("other")
    for (k in rest) row(k, x[[k]])
  }
  # how to change any of these
  hint <- paste0("change any: flowr_set_config(key = value), ",
                 "options(flowr.key = value) or cfg$key <- value")
  if (any_dim) {
    hint <- paste0(hint, "; grayed = unused by the '", active, "' engine")
  }
  cat(.flowr_ansi(hint, "90", color), "\n", sep = "")
  invisible(x)
}

#' @export
`$<-.flowr_config` <- function(x, name, value) {
  do.call(flowr_set_config, stats::setNames(list(value), name))
  NextMethod()
}

#' @export
`[[<-.flowr_config` <- function(x, name, value) {
  do.call(flowr_set_config, stats::setNames(list(value), name))
  NextMethod()
}

#' Change flowr configuration values
#'
#' Sets one or more configuration knobs for the current session (as R options)
#' and, optionally, persists them to the user JSON config file so they apply to
#' future sessions too. This is the convenient way to switch engine, flowR
#' version, timeouts and the like without editing files by hand.
#'
#' Equivalent lower-level forms all work and are fully supported: set the R
#' option directly with `options(flowr.<name> = value)`, the environment variable
#' `FLOWR_<NAME>`, or assign into the configuration object,
#' `cfg <- flowr_config(); cfg$engine <- "binary"` (which sets the option for
#' you).
#'
#' @param ... Named settings to change, e.g. `engine = "binary"`. Names must be
#'   valid flowr options (see [flowr_config()]).
#' @param persist Also write the settings to the user config file
#'   ([flowr_config_file()]), so they survive across sessions.
#' @return The updated configuration (a `flowr_config`), invisibly.
#' @seealso [flowr_config()], [flowr_status()]
#' @export
#' @examples
#' \dontrun{
#' flowr_set_config(engine = "binary", request_timeout = 300)
#' flowr_set_config(flowr_version = "2.12.0", persist = TRUE)
#' }
flowr_set_config <- function(..., persist = FALSE) {
  vals <- list(...)
  if (length(vals) == 0 || is.null(names(vals)) || any(!nzchar(names(vals)))) {
    .flowr_stop("supply named settings, e.g. flowr_set_config(engine = \"binary\")")
  }
  unknown <- setdiff(names(vals), names(.flowr_defaults))
  if (length(unknown) > 0) {
    .flowr_stop("unknown flowr setting(s): ", paste(unknown, collapse = ", "),
         "\n  known settings: ", paste(names(.flowr_defaults), collapse = ", "))
  }
  # coerce each value to the type of its default, so "300" and 300 both work
  vals <- Map(function(v, nm) {
    if (is.character(v) && !is.character(.flowr_defaults[[nm]])) .flowr_coerce(v, .flowr_defaults[[nm]]) else v
  }, vals, names(vals))
  opts <- vals
  names(opts) <- paste0("flowr.", names(vals))
  options(opts)
  if (isTRUE(persist)) {
    path <- .flowr_write_config(vals)
    if (!isTRUE(flowr_option("quiet"))) {
      message("[flowr] wrote ", length(vals), " setting(s) to ", path)
    }
  }
  invisible(flowr_config())
}

# Merge `vals` into the user JSON config file and return its path.
.flowr_write_config <- function(vals) {
  dir <- tools::R_user_dir("flowr", "config")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(dir, "config.json")
  current <- if (file.exists(path)) {
    tryCatch(jsonlite::fromJSON(path, simplifyVector = TRUE), error = function(e) list())
  } else {
    list()
  }
  if (!is.list(current)) {
    current <- as.list(current)
  }
  for (nm in names(vals)) {
    current[[nm]] <- vals[[nm]]
  }
  writeLines(jsonlite::toJSON(current, auto_unbox = TRUE, pretty = TRUE), path)
  # invalidate the in-memory cache so the new values resolve immediately
  .flowr_state$config_json <- NULL
  path
}
