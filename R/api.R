# High-level API --------------------------------------------------------------
#
# Ergonomic one-call helpers built on top of a (default or explicit) session.
# Each analyses the code once (cached) and then performs the requested
# operation, returning tidy R objects.

#' Slice R code with flowR
#'
#' Computes a program slice for one or more slicing criteria and returns the
#' reconstructed, sliced code together with the ids of the included nodes.
#'
#' @param code A string of R source code. Provide this, `file` or `folder`.
#' @param criterion Slicing criteria, a character vector such as `"3@x"`
#'   (line 3, variable `x`), `"7:3"` (line:column) or `"$42"` (a node id).
#' @param direction Slice `"backward"` (default: everything the criterion
#'   depends on) or `"forward"` (everything that depends on the criterion).
#' @param file Path to a single R file to analyse.
#' @param folder Path to a folder; all `.R` files in it (recursively) are
#'   analysed together for a multi-file slice. Needs a real engine.
#' @param cfg Also request the control-flow graph in the underlying analysis.
#' @param style Default print style for the returned slice: `"diff"`, `"gray"`
#'   or `"code"`. See [print.flowr_slice()].
#' @param session A `flowr_session` (from [flowr_connect()]); the central default
#'   session is used when `NULL`. Pass an explicit one to work with several at once.
#' @return A `flowr_slice` with `code` (reconstructed slice), `ids` (included
#'   node ids), `original` (the input source), `lines` (original line numbers in
#'   the slice), plus `criteria`, `direction` and the raw `response`. Printing
#'   shows a colored git-style diff by default; see [print.flowr_slice()].
#' @seealso [query()], [flowr_locations()]
#' @export
#' @examples
#' \dontrun{
#' flowr_install()   # once, to get a flowR engine
#' slice("x <- 1\ny <- 2\ncat(x)", "3@x")$code
#' }
slice <- function(code = NULL, criterion, direction = c("backward", "forward"),
                  file = NULL, folder = NULL, cfg = FALSE,
                  style = getOption("flowr.slice_style", "diff"), session = NULL) {
  done <- .flowr_timer("slice"); on.exit(done(), add = TRUE)
  direction <- match.arg(direction)
  style <- match.arg(style, c("diff", "gray", "code"))
  # validate before starting an engine, so bad calls fail fast
  if (missing(criterion) || length(criterion) == 0) {
    stop("provide a slicing `criterion`, e.g. \"3@x\"", call. = FALSE)
  }
  # accept a vector of criteria and flowR's ";"-separated form ("3@print;2@y"),
  # so both slice(code, c("3@print", "2@y")) and slice(code, "3@print;2@y") work
  criterion <- trimws(unlist(strsplit(as.character(criterion), ";", fixed = TRUE)))
  criterion <- criterion[nzchar(criterion)]
  if (length(criterion) == 0) {
    stop("provide a slicing `criterion`, e.g. \"3@x\"", call. = FALSE)
  }
  session <- .flowr_resolve_session(session)
  src <- .flowr_input_analysis(code, file, folder, cfg, session)
  an <- src$an
  original <- src$original
  # Slicing goes through the Query API's `static-slice` query (the current flowR
  # path); the legacy `request-slice` returns no results in flowR 2.11.1.
  qobj <- list(type = "static-slice", criteria = I(as.character(criterion)),
               direction = direction)
  req <- list(
    type = "request-query", id = .flowr_session_id(session),
    filetoken = an$filetoken, query = I(list(qobj))
  )
  res <- .flowr_request(session$con, req)
  ss <- res$results[["static-slice"]]$results
  entry <- if (!is.null(ss) && length(ss) > 0) ss[[1]] else list()
  ids <- entry$slice$result
  # flowR echoes an unresolved criterion back with an empty reconstruction rather
  # than failing, so treat empty reconstructed code as "matched nothing".
  if (length(ids) == 0 || !nzchar(trimws(entry$reconstruct$code %||% ""))) {
    warning("slice criteria matched nothing: ", paste(criterion, collapse = ", "),
            ".\n  Criteria look like \"line@name\", \"line:col\" or \"$id\"; ",
            "check the line/name exists.", call. = FALSE)
  }
  # record flowR's own phase timings so `flowr.timing` shows the breakdown
  .flowr_timing_detail(c(.flowr_analysis_phases(an$analysis),
                         slice = entry$slice$.meta$timing,
                         reconstruct = entry$reconstruct$.meta$timing))
  loc_map <- tryCatch(make_id_to_location_map(an$analysis$results$normalize$ast),
                      error = function(e) list())
  structure(
    list(
      code = entry$reconstruct$code,
      ids = ids,
      original = original,
      lines = .flowr_covered_lines(ids, loc_map),
      criteria = as.character(criterion),
      direction = direction,
      style = style,
      filetoken = an$filetoken,
      analysis = an$analysis,
      response = res
    ),
    class = "flowr_slice"
  )
}

# The root of the R project/package enclosing `start`. An R package (DESCRIPTION)
# or an RStudio project (.Rproj) marks a root; when nested we take the OUTERMOST
# such marker so a call from R/ or tests/ still analyses the whole project. Falls
# back to the enclosing git root, then to `start` itself if it holds R files.
# Returns NULL when nothing looks like an analysable root.
.flowr_project_root <- function(start = getwd()) {
  dir <- normalizePath(start, mustWork = FALSE)
  markers <- character(0)     # R-project roots seen, innermost first
  gitroot <- NULL
  repeat {
    if (file.exists(file.path(dir, "DESCRIPTION")) ||
        length(list.files(dir, pattern = "\\.Rproj$")) > 0) {
      markers <- c(markers, dir)
    }
    if (is.null(gitroot) && file.exists(file.path(dir, ".git"))) {
      gitroot <- dir
    }
    parent <- dirname(dir)
    if (identical(parent, dir)) {
      break
    }
    dir <- parent
  }
  if (length(markers) > 0) {
    return(markers[[length(markers)]])       # outermost R-project marker
  }
  if (!is.null(gitroot)) {
    return(gitroot)
  }
  start <- normalizePath(start, mustWork = FALSE)
  if (length(list.files(start, pattern = "\\.[rR]$")) > 0) {
    return(start)
  }
  NULL
}

# A short, human label for an auto-selected root ("R package 'x'", "R project").
.flowr_root_label <- function(root) {
  desc <- file.path(root, "DESCRIPTION")
  if (file.exists(desc)) {
    pkg <- tryCatch(read.dcf(desc, fields = "Package")[1, 1], error = function(e) NA)
    if (!is.na(pkg)) {
      return(sprintf("R package '%s'", pkg))
    }
  }
  "R project"
}

# Fill in the "no input given => current project folder" default, so the commands
# work with no arguments inside a project. Reports the auto-selected root once
# (unless quiet). Returns the (possibly updated) folder; errors if none resolves.
.flowr_default_source <- function(code, file, folder) {
  if (!is.null(code) || !is.null(file) || !is.null(folder)) {
    return(folder)
  }
  folder <- .flowr_project_root()
  if (is.null(folder)) {
    stop("provide `code`, `file` or `folder` (or call from inside an R project ",
         "with .R files or a DESCRIPTION)", call. = FALSE)
  }
  if (!isTRUE(flowr_option("quiet"))) {
    message("[flowr] auto-selected root: ", .flowr_root_label(folder), " at ", folder)
  }
  folder
}

# Resolve code/file/folder into a (cached) analysis, defaulting to the current R
# package folder when none is given. Returns list(an, original, folder).
# Requires a real engine for a folder. `session` must already be resolved.
.flowr_input_analysis <- function(code, file, folder, cfg, session) {
  # a folder may arrive via file =
  if (is.null(folder) && !is.null(file) && dir.exists(file)) {
    folder <- file
    file <- NULL
  }
  folder <- .flowr_default_source(code, file, folder)
  if (!is.null(folder)) {
    files <- list.files(folder, pattern = "\\.[rR]$", recursive = TRUE, full.names = TRUE)
    if (length(files) == 0) {
      stop("no .R files found in ", folder, call. = FALSE)
    }
    list(an = flowr_analyze(files = files, cfg = cfg, session = session),
         original = NULL, folder = folder)
  } else {
    original <- if (!is.null(code)) code
                else if (!is.null(file)) paste(readLines(file, warn = FALSE), collapse = "\n")
                else NULL
    list(an = flowr_analyze(code = code, file = file, cfg = cfg, session = session),
         original = original, folder = NULL)
  }
}

# Original line numbers covered by a set of node ids.
.flowr_covered_lines <- function(ids, loc_map) {
  lines <- integer(0)
  for (id in ids) {
    loc <- loc_map[[paste0(id)]]
    if (!is.null(loc) && length(loc) >= 3) {
      lines <- c(lines, as.integer(loc[[1]]):as.integer(loc[[3]]))
    }
  }
  sort(unique(lines))
}

# Decide whether to emit ANSI colour: option flowr.color wins, then NO_COLOR,
# then an interactive, non-dumb terminal.
.flowr_use_color <- function() {
  opt <- getOption("flowr.color")
  if (!is.null(opt)) {
    return(isTRUE(opt))
  }
  if (nzchar(Sys.getenv("NO_COLOR")) || !interactive() ||
      identical(Sys.getenv("TERM"), "dumb")) {
    return(FALSE)
  }
  isTRUE(tryCatch(isatty(stdout()), error = function(e) FALSE))
}

.flowr_ansi <- function(text, code, use) {
  if (isTRUE(use)) paste0("\x1b[", code, "m", text, "\x1b[0m") else text
}

# TRUE if the terminal is known to render clickable OSC 8 hyperlinks. Option
# flowr.hyperlinks overrides; otherwise we recognise the common capable ones so
# we never underline/advertise a link the terminal cannot follow.
.flowr_hyperlinks_supported <- function() {
  opt <- getOption("flowr.hyperlinks")
  if (!is.null(opt)) {
    return(isTRUE(opt))
  }
  if (identical(Sys.getenv("RSTUDIO"), "1")) {
    return(TRUE)
  }
  if (Sys.getenv("TERM_PROGRAM") %in% c("iTerm.app", "WezTerm", "vscode", "ghostty", "Hyper")) {
    return(TRUE)
  }
  if (nzchar(Sys.getenv("KITTY_WINDOW_ID")) || nzchar(Sys.getenv("WT_SESSION"))) {
    return(TRUE)
  }
  vte <- suppressWarnings(as.integer(Sys.getenv("VTE_VERSION", "0")))
  !is.na(vte) && vte >= 5000                # GNOME Terminal and other VTE >= 0.50
}

# Wrap text in an OSC 8 terminal hyperlink. Only when we are emitting ANSI, a
# url is given, and the terminal is known to make it clickable.
.flowr_hyperlink <- function(text, url, use) {
  if (!isTRUE(use) || is.null(url) || !nzchar(url) || !.flowr_hyperlinks_supported()) {
    return(text)
  }
  paste0("\x1b]8;;", url, "\x1b\\", text, "\x1b]8;;\x1b\\")
}

# 1-based, per-line inclusive column ranges of the tokens each criterion points
# to, so the slice printer can underline them.
.flowr_underline_ranges <- function(x, lines) {
  res <- list()
  add <- function(line, start, end) {
    if (line >= 1 && line <= length(lines) && end >= start) {
      res[[as.character(line)]] <<- c(res[[as.character(line)]], list(c(start, end)))
    }
  }
  loc_map <- NULL
  for (cr in trimws(as.character(x$criteria))) {
    if (grepl("^[0-9]+@", cr)) {                         # line@variable
      l <- as.integer(sub("@.*$", "", cr))
      var <- sub("^[0-9]+@", "", cr)
      if (l >= 1 && l <= length(lines)) {
        pat <- paste0("\\b\\Q", var, "\\E\\b")
        p <- regexpr(pat, lines[[l]], perl = TRUE)
        if (p > 0) add(l, p, p + attr(p, "match.length") - 1L)
      }
    } else if (grepl("^[0-9]+:[0-9]+$", cr)) {            # line:column
      l <- as.integer(sub(":.*$", "", cr))
      col <- as.integer(sub("^[0-9]+:", "", cr))
      if (l >= 1 && l <= length(lines)) {
        m <- regexpr("^[A-Za-z0-9._]+", substring(lines[[l]], col))
        add(l, col, col + (if (m > 0) attr(m, "match.length") else 1L) - 1L)
      }
    } else if (grepl("^\\$", cr)) {                      # $node-id
      if (is.null(loc_map)) {
        loc_map <- tryCatch(make_id_to_location_map(x$analysis$results$normalize$ast),
                            error = function(e) list())
      }
      loc <- loc_map[[sub("^\\$", "", cr)]]
      if (!is.null(loc) && length(loc) >= 4 && loc[[1]] == loc[[3]]) {
        add(as.integer(loc[[1]]), as.integer(loc[[2]]), as.integer(loc[[4]]))
      }
    }
  }
  res
}

# Wrap the given column ranges of `text` in ANSI underline.
.flowr_apply_underline <- function(text, ranges) {
  ranges <- ranges[order(vapply(ranges, `[`, numeric(1), 1))]
  out <- ""
  pos <- 1L
  for (r in ranges) {
    s <- max(pos, r[1])
    e <- min(nchar(text), r[2])
    if (e < s) next
    out <- paste0(out, substr(text, pos, s - 1L), "\x1b[4m", substr(text, s, e), "\x1b[24m")
    pos <- e + 1L
  }
  paste0(out, substr(text, pos, nchar(text)))
}

#' Print a slice as a diff or with unused lines dimmed
#'
#' @param x A `flowr_slice`.
#' @param style `"diff"`: the original with the lines dropped by slicing shown
#'   as red `-` removals and the kept lines as plain context. `"gray"`: the full
#'   original with non-slice lines dimmed. `"code"`: just the reconstructed
#'   slice. Defaults to the style chosen in [slice()], else
#'   `getOption("flowr.slice_style", "diff")`.
#' @param color Emit ANSI colour. Defaults to on for interactive terminals only;
#'   force with `color = TRUE`/`FALSE`, `options(flowr.color=)` or `NO_COLOR`.
#' @param ... Ignored.
#' @return `x`, invisibly.
#' @export
print.flowr_slice <- function(x, style = x$style %||% getOption("flowr.slice_style", "diff"),
                              color = .flowr_use_color(), ...) {
  style <- match.arg(style, c("diff", "gray", "code"))
  if (is.null(x$original) || is.null(x$lines) || style == "code") {
    if (!is.null(x$code)) {
      cat(x$code)
      if (!endsWith(x$code, "\n")) cat("\n")
    }
    return(invisible(x))
  }
  lines <- strsplit(x$original, "\n", fixed = TRUE)[[1]]
  keep <- seq_along(lines) %in% x$lines
  pct <- if (length(lines) > 0) round(100 * sum(keep) / length(lines)) else 0
  header <- sprintf("%s | %s | %d/%d (%d%%) lines",
                    paste(x$criteria, collapse = ", "), x$direction,
                    sum(keep), length(lines), pct)
  cat(.flowr_ansi(header, "1", color), "\n", sep = "")
  ul <- if (isTRUE(color)) .flowr_underline_ranges(x, lines) else list()
  for (i in seq_along(lines)) {
    txt <- lines[[i]]
    rng <- ul[[as.character(i)]]
    if (!is.null(rng)) {
      txt <- .flowr_apply_underline(txt, rng)   # underline the criterion token
    }
    if (style == "gray") {
      cat(if (keep[i]) txt else .flowr_ansi(txt, "90", color), "\n", sep = "")
    } else if (keep[i]) {
      cat("  ", txt, "\n", sep = "")                              # context
    } else {
      cat(.flowr_ansi(paste0("- ", txt), "31", color), "\n", sep = "")  # removed
    }
  }
  invisible(x)
}

#' Run flowR queries
#'
#' Sends one or more queries through flowR's Query API. `query` may be a
#' character vector of query type names (e.g. `"dependencies"`), a single query
#' object (a named list with a `type`), or a list of such objects.
#'
#' @inheritParams slice
#' @param query The query or queries to run. See *Details* for the shape.
#' @return The query results, a named list keyed by query type.
#' @details Supported query types in flowR 2.11.1 include `dependencies`,
#'   `call-context`, `dataflow`, `df-shape`, `static-slice`, `id-map`,
#'   `normalized-ast`, `linter`, `location-map`, `call-graph`, and more. Pass
#'   extra arguments by using the object form, e.g.
#'   `query(code, list(type = "static-slice", criteria = I("3@x")))`.
#' @seealso [flowr_overview()], [dataflow()]
#' @export
query <- function(code = NULL, query, file = NULL, folder = NULL, cfg = FALSE, session = NULL) {
  done <- .flowr_timer("query"); on.exit(done(), add = TRUE)
  qs <- .flowr_normalize_query(query)
  session <- .flowr_resolve_session(session)
  an <- .flowr_input_analysis(code, file, folder, cfg, session)$an
  req <- list(
    type = "request-query", id = .flowr_session_id(session),
    filetoken = an$filetoken, query = I(qs)
  )
  res <- .flowr_request(session$con, req)
  .flowr_timing_detail(.flowr_analysis_phases(an$analysis))
  res$results
}

# Accept "type", c("a","b"), list(type=..), or list(list(type=..), ..).
.flowr_normalize_query <- function(query) {
  if (is.character(query)) {
    return(lapply(query, function(t) list(type = t)))
  }
  if (is.list(query) && !is.null(query$type)) {
    return(list(query))
  }
  if (is.list(query)) {
    return(query)
  }
  stop("`query` must be a query type name, a query object, or a list of them",
       call. = FALSE)
}

# Thin wrapper around the `dependencies` query (libraries, sources, reads,
# writes). Internal: users get this, enriched and ready to slice, via
# flowr_overview(); the raw query is still available through query().
#' @noRd
dependencies <- function(code = NULL, file = NULL, folder = NULL, session = NULL) {
  query(code = code, file = file, folder = folder,
        query = "dependencies", session = session)$dependencies
}

#' Retrieve the dataflow information for R code
#'
#' @inheritParams slice
#' @return The `dataflow` query result.
#' @export
dataflow <- function(code = NULL, file = NULL, folder = NULL, session = NULL) {
  done <- .flowr_timer("dataflow"); on.exit(done(), add = TRUE)
  query(code = code, file = file, folder = folder,
        query = "dataflow", session = session)$dataflow
}

#' Inspect an R project or package
#'
#' Runs flowR's `project` query and reports the files it analysed together with
#' a breakdown of their roles (source, test, vignette, data, description, ...).
#' Called with no `code`/`file`/`folder` inside an R package, it inspects the
#' current package.
#'
#' @inheritParams slice
#' @param with_dataflow Also include dataflow information in the result.
#' @return A `flowr_project`: the `files` analysed and their `roleCounts`.
#' @seealso [flowr_overview()], [query()]
#' @export
#' @examples
#' \dontrun{
#' inspect_project()                 # the package in the working directory
#' inspect_project(folder = "R")     # a specific folder
#' }
inspect_project <- function(code = NULL, file = NULL, folder = NULL,
                            with_dataflow = FALSE, session = NULL) {
  done <- .flowr_timer("inspect_project"); on.exit(done(), add = TRUE)
  qobj <- list(type = "project", withDf = isTRUE(with_dataflow))
  res <- query(code = code, file = file, folder = folder, query = qobj,
               session = session)$project
  structure(
    list(files = res$files %||% list(),
         roleCounts = res$roleCounts %||% list(),
         raw = res),
    class = "flowr_project"
  )
}

#' @export
print.flowr_project <- function(x, color = .flowr_use_color(), ...) {
  nf <- length(x$files)
  cat(.flowr_ansi(sprintf("project | %d file%s", nf, if (nf == 1) "" else "s"),
                  "1", color), "\n", sep = "")
  # roles with a non-zero count, most frequent first
  roles <- x$roleCounts
  counts <- vapply(roles, function(v) as.integer(v %||% 0L), integer(1))
  counts <- counts[counts > 0]
  if (length(counts) > 0) {
    counts <- counts[order(-counts)]
    parts <- sprintf("%s %d", names(counts), counts)
    cat("  ", .flowr_ansi(paste(parts, collapse = "   "), "2", color), "\n", sep = "")
  }
  for (f in x$files) {
    cat("  ", .flowr_ansi(as.character(f), "90", color), "\n", sep = "")
  }
  invisible(x)
}

#' Execute a flowR REPL command, or drop into an interactive REPL
#'
#' With an `expression`, runs a single REPL expression or command (commands are
#' prefixed with `:`, e.g. `":help"`) and returns its streamed output.
#' With no `expression` in an interactive session, opens flowR's own REPL with
#' native history (up-arrow) and tab completion via [flowr_console()]. Set
#' `native = FALSE` to use the socket-relayed line loop over the current session
#' instead. Type R code or `:`-commands until you enter `:quit`.
#'
#' @param expression The REPL expression/command to run, or `NULL` to start an
#'   interactive REPL.
#' @param ansi Keep ANSI colour codes in the output (default `FALSE`).
#' @param session A `flowr_session`; the central default is used when `NULL`.
#' @param r_access For the interactive REPL, allow running R code you type (via
#'   the r-shell engine and `--r-session-access`). On by default since you drive
#'   the REPL; set `FALSE` for a flowR-only shell.
#' @param native For the interactive REPL, use flowR's own console (history and
#'   tab completion). `FALSE` uses the socket-relayed line loop over the current
#'   session (the "messaging" REPL), which streams each response through flowr.
#' @return For a single command, a `flowr_repl` with the combined `output` and
#'   the raw `messages`; for the interactive REPL, `invisible(NULL)`.
#' @export
#' @examples
#' \dontrun{
#' flowr_repl(":help")           # one command
#' flowr_repl()                   # interactive native REPL (R execution on)
#' flowr_repl(native = FALSE)     # socket-relayed line loop instead
#' }
flowr_repl <- function(expression = NULL, ansi = FALSE, session = NULL,
                       r_access = TRUE, native = TRUE) {
  if (is.null(expression)) {
    if (!interactive()) {
      stop("flowr_repl() needs an `expression` in a non-interactive session",
           call. = FALSE)
    }
    # Prefer flowR's own REPL: it has real history (up-arrow) and tab completion,
    # which the socket-relayed line loop (R's readline) cannot provide. You drive
    # this REPL, so R execution is on by default. native = FALSE keeps the
    # socket-relayed loop over the current session.
    eng <- tryCatch(.flowr_resolve_engine(flowr_option("engine"),
                                          flowr_option("flowr_version")),
                    error = function(e) NA_character_)
    if (isTRUE(native) && !is.na(eng) &&
        !is.null(.flowr_engine_specs()[[eng]]$console)) {
      return(flowr_console(r_access = r_access))
    }
    return(.flowr_repl_console(.flowr_resolve_session(session), ansi))
  }
  done <- .flowr_timer("repl"); on.exit(done(), add = TRUE)
  session <- .flowr_resolve_session(session)
  req <- list(
    type = "request-repl-execution", id = .flowr_session_id(session),
    expression = expression, ansi = ansi
  )
  msgs <- .flowr_request_stream(session$con, req, end_type = "end-repl-execution")
  out <- vapply(msgs, function(m) m$result %||% "", character(1))
  structure(
    list(
      output = paste(out, collapse = ""),
      stream = vapply(msgs, function(m) m$stream %||% "stdout", character(1)),
      messages = msgs
    ),
    class = "flowr_repl"
  )
}

#' @export
print.flowr_repl <- function(x, ...) {
  cat(x$output)
  if (!endsWith(x$output, "\n")) cat("\n")
  invisible(x)
}

# Interactive REPL loop: read a line, run it, print flowR's streamed output.
.flowr_repl_console <- function(session, ansi) {
  message("flowR REPL (", session$versions$flowr %||% "?", "). ",
          "Enter R code or :commands (e.g. :help). ",
          "Empty line or :quit to exit.")
  repeat {
    line <- readline("flowR> ")
    if (!nzchar(trimws(line)) || trimws(line) %in% c(":quit", ":q", "q")) {
      break
    }
    out <- tryCatch(
      flowr_repl(line, ansi = ansi),
      error = function(e) {
        message("! ", conditionMessage(e))
        NULL
      }
    )
    if (!is.null(out)) {
      cat(out$output)
      if (!endsWith(out$output, "\n")) cat("\n")
    }
  }
  invisible(NULL)
}

#' Open flowR's own interactive console
#'
#' Hands terminal control to flowR's native command-line REPL, with its own tab
#' completion, history and `:`-commands. Unlike [flowr_repl()] (which relays one
#' line at a time over the socket), this launches flowR as a normal interactive
#' process with inherited stdin/stdout, so it is the full flowR shell. It returns
#' when you leave the flowR REPL (`:quit`).
#'
#' Available for the `binary`, `bundled` and `node` engines (the `docker` engine
#' has no terminal handoff; use [flowr_repl()] there). Needs an interactive
#' terminal.
#'
#' @param engine Which engine's flowR CLI to launch. Defaults to the configured
#'   engine, resolving `auto` the same way a session would.
#' @param flowr_version flowR version to use.
#' @param r_access Allow the console to execute arbitrary R code (flowR's
#'   `--r-session-access`). Since you are already in R, this is the easy way to
#'   turn the flowR REPL into one that also runs R. It uses the r-shell engine
#'   and, being an explicit opt-in, overrides secure mode for this console only.
#' @return The flowR process exit status, invisibly.
#' @seealso [flowr_repl()]
#' @export
#' @examples
#' \dontrun{
#' flowr_console()                # a full flowR shell with native completion
#' flowr_console(r_access = TRUE) # ... that can also execute R code
#' }
flowr_console <- function(engine = flowr_option("engine"),
                          flowr_version = flowr_option("flowr_version"),
                          r_access = FALSE) {
  if (!interactive()) {
    stop("flowr_console() needs an interactive terminal; use flowr_repl() otherwise.",
         call. = FALSE)
  }
  eng <- .flowr_resolve_engine(engine, flowr_version)
  spec <- .flowr_engine_specs()[[eng]]
  if (is.null(spec$console)) {
    stop("flowr_console() is not available for the '", eng, "' engine ",
         "(no terminal handoff); use flowr_repl().", call. = FALSE)
  }
  # r_access runs R you type, so it needs the r-shell engine and the
  # --r-session-access flag. You are driving the REPL, so this is on by default.
  fe <- flowr_option("flowr_engine")
  extra <- character(0)
  if (isTRUE(r_access)) {
    fe <- "r-shell"
    extra <- "--r-session-access"
  } else if (isTRUE(flowr_option("secure")) && identical(fe, "r-shell")) {
    stop("secure mode forbids the r-shell engine; use flowr_engine = \"tree-sitter\", ",
         "or flowr_console(r_access = TRUE) to deliberately allow R execution.",
         call. = FALSE)
  }
  spec$ensure(flowr_version, flowr_option("quiet"))
  cc <- spec$console(flowr_version, fe)
  message("[flowr] flowR console (", eng, ", flowR ", flowr_version,
          if (isTRUE(r_access)) ", R execution on" else "",
          "); type :quit to exit.")
  # inherit this process's stdio so flowR's readline/completion drive the tty
  invisible(system2(cc$cmd, c(cc$args, extra)))
}

#' Re-run a flowr command whenever a file changes
#'
#' Watches `file` and re-runs `action` each time its contents change, printing
#' the result, until you interrupt it (Ctrl-C / Esc). This brings flowR's
#' `watch://` REPL input to the R API for any command. In flowR's own console
#' ([flowr_console()]) you can equivalently pass `watch://<path>` in place of
#' `file://<path>` to a `:`-command (e.g. `:slice watch://script.R`).
#'
#' @param file Path to the R file to watch.
#' @param action A function of one argument (the file path), run on every change.
#'   Defaults to printing [flowr_overview()] of the file. For slicing, pass e.g.
#'   `function(f) print(slice(file = f, criterion = "5@x"))`.
#' @param interval Polling interval in seconds.
#' @return Does not return normally; runs until interrupted.
#' @seealso [flowr_console()], [slice()], [flowr_overview()]
#' @export
#' @examples
#' \dontrun{
#' flowr_watch("script.R")   # re-print the overview on every save
#' flowr_watch("script.R", function(f) print(slice(file = f, criterion = "8@result")))
#' }
flowr_watch <- function(file, action = function(f) print(flowr_overview(file = f)),
                        interval = 0.5) {
  if (!interactive()) {
    stop("flowr_watch() needs an interactive session (it loops until interrupted).",
         call. = FALSE)
  }
  if (!is.function(action)) {
    stop("`action` must be a function of one argument (the file path)", call. = FALSE)
  }
  file <- normalizePath(file, mustWork = TRUE)
  message("[flowr] watching ", file, " - press Ctrl-C / Esc to stop ...")
  last <- ""
  repeat {
    info <- tryCatch(file.info(file), error = function(e) NULL)
    sig <- if (is.null(info)) "" else paste0(info$mtime, ":", info$size)
    if (!identical(sig, last)) {
      last <- sig
      cat("\n")
      tryCatch(action(file), error = function(e) message("! ", conditionMessage(e)))
    }
    Sys.sleep(interval)
  }
}

#' Map the node ids of a slice (or analysis) to source locations
#'
#' @param x A `flowr_slice`, or an analysis AST as returned within one.
#' @return A named list mapping node id (as string) to a `[line1, col1, line2,
#'   col2]` location.
#' @export
flowr_locations <- function(x) {
  ast <- if (inherits(x, "flowr_slice")) x$analysis$results$normalize$ast else x
  make_id_to_location_map(ast)
}

#' Source locations of the nodes contained in a slice
#'
#' Resolves the node ids of a slice to their `[line1, col1, line2, col2]` source
#' locations (node ids without a location, such as built-in functions, are
#' dropped). This is the engine-agnostic core used by front-ends (e.g. the
#' RStudio addin) to highlight a slice.
#'
#' @param slice A `flowr_slice` from [slice()].
#' @return A list of numeric length-4 location vectors.
#' @export
flowr_slice_locations <- function(slice) {
  if (!inherits(slice, "flowr_slice")) {
    stop("`slice` must be a flowr_slice", call. = FALSE)
  }
  location_map <- tryCatch(flowr_locations(slice), error = function(e) list())
  out <- list()
  for (id in slice$ids) {
    loc <- location_map[[paste0(id)]]
    if (!is.null(loc)) {
      out[[length(out) + 1L]] <- loc
    }
  }
  out
}

#' Build a slicing criterion for a cursor position
#'
#' Given a 1-based cursor `line`/`column` in the `lines` of a script, returns a
#' `"line:column"` criterion anchored at the start of the token under the cursor
#' --- the form flowR expects. Front-ends use this to turn an editor selection
#' into a slicing criterion.
#'
#' @param line 1-based line of the cursor.
#' @param column 1-based column of the cursor.
#' @param lines Character vector of the script's lines.
#' @return A criterion string such as `"4:1"`.
#' @export
#' @examples
#' flowr_criterion_at(1, 6, c("value <- 1"))
flowr_criterion_at <- function(line, column, lines) {
  text <- if (line >= 1 && line <= length(lines)) lines[[line]] else ""
  i <- as.integer(column)
  while (i > 1 && grepl("[A-Za-z0-9._]", substr(text, i - 1L, i - 1L))) {
    i <- i - 1L
  }
  paste0(line, ":", i)
}
