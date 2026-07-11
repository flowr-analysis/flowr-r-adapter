# Linting ---------------------------------------------------------------------
#
# A thin, tidy wrapper over flowR's `linter` query. flowR returns its findings
# grouped by rule, each finding carrying a source location and rule-specific
# fields; we flatten that into one row per finding with the same column names
# `lintr` uses (filename, line_number, column_number, type, message, line,
# linter), so the result drops straight into lintr-based tooling and reports.

# flowR rules that report style/'.info' issues rather than likely defects; used
# to fill lintr's `type` column. Everything not listed defaults to "warning".
.flowr_lint_style_rules <- c("naming-convention", "software-has-license",
                             "software-has-tests", "roxygen-arguments")

.flowr_lint_type <- function(rule) {
  if (rule %in% .flowr_lint_style_rules) "style" else "warning"
}

# A human-readable message for a finding: flowR's own `message` when present,
# else a friendly per-rule sentence, else a generic dump of the scalar fields so
# an unknown/newer rule still produces something useful rather than blank.
.flowr_lint_message <- function(rule, x) {
  if (!is.null(x$message) && nzchar(x$message)) {
    return(as.character(x$message))
  }
  msg <- switch(rule,
    "absolute-file-paths"        = sprintf("absolute file path `%s` should be relative or configurable", x$filePath %||% "?"),
    "file-path-validity"         = sprintf("file path `%s` may not exist at run time", x$filePath %||% "?"),
    "unused-definitions"         = sprintf("`%s` is assigned but never used", x$variableName %||% "?"),
    "naming-convention"          = sprintf("`%s` does not follow the %s convention (detected %s)",
                                           x$name %||% "?", x$expectedCasing %||% "expected", x$detectedCasing %||% "?"),
    "deprecated-functions"       = sprintf("`%s` is deprecated", x[["function"]] %||% x$name %||% "?"),
    "dead-code"                  = "dead code: this statement can never be reached",
    "useless-loop"               = "this loop has no effect",
    "seeded-randomness"          = "randomness used without a fixed seed (results are not reproducible)",
    "undefined-symbol"           = sprintf("`%s` is used but never defined", x$name %||% x$variableName %||% "?"),
    "no-leaked-credentials"      = "a credential may be leaked here",
    "dataframe-access-validation" = "data-frame access may be out of bounds",
    "network-functions"          = "network access",
    "stop-call"                  = "stop() call reachable here",
    "problematic-inputs"         = "problematic input value",
    NULL
  )
  if (!is.null(msg)) {
    return(msg)
  }
  # generic fallback: the informative scalar fields, minus structural ones
  drop <- c("loc", "certainty", "quickFix", "involvedId")
  flds <- x[setdiff(names(x), drop)]
  flds <- flds[vapply(flds, function(v) is.atomic(v) && length(v) == 1L, logical(1))]
  if (length(flds)) {
    paste0(rule, ": ", paste(sprintf("%s=%s", names(flds), unlist(flds)), collapse = ", "))
  } else {
    rule
  }
}

# Turn flowR's `results` (named-by-rule) into a lintr-shaped data.frame.
.flowr_lint_rows <- function(results, code, file) {
  code_lines <- if (!is.null(code)) strsplit(code, "\n", fixed = TRUE)[[1]] else NULL
  pos <- function(v) if (length(v) == 1L && !is.na(v) && v > 0L) as.integer(v) else NA_integer_
  rows <- list()
  for (rule in names(results)) {
    for (x in (results[[rule]]$results %||% list())) {
      loc <- x$loc
      sl <- pos(as.integer(loc[[1]])); sc <- pos(as.integer(loc[[2]]))
      locpath <- if (length(loc) >= 5L) as.character(loc[[5]]) else NA_character_
      filename <- if (!is.null(file)) file
                  else if (is.null(code) && !is.na(locpath)) locpath   # file/folder run
                  else "<text>"                                        # inline code
      line_txt <- if (!is.null(code_lines) && !is.na(sl) && sl <= length(code_lines)) {
        code_lines[[sl]]
      } else {
        NA_character_
      }
      rows[[length(rows) + 1L]] <- data.frame(
        filename      = filename,
        line_number   = sl,
        column_number = sc,
        type          = .flowr_lint_type(rule),
        message       = .flowr_lint_message(rule, x),
        line          = line_txt,
        linter        = rule,
        certainty     = x$certainty %||% NA_character_,
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(rows) == 0L) {
    return(data.frame(
      filename = character(0), line_number = integer(0), column_number = integer(0),
      type = character(0), message = character(0), line = character(0),
      linter = character(0), certainty = character(0), stringsAsFactors = FALSE))
  }
  out <- do.call(rbind, rows)
  # deterministic order: by file, then position
  out[order(out$filename, out$line_number, out$column_number, out$linter,
            method = "radix"), , drop = FALSE]
}

# Render findings in the machine-readable formats jarl (https://jarl.etiennebacher.com)
# emits, so flowr's linter output is interchangeable with jarl's in CI and editors:
#   "jarl"   jarl's native JSON: {diagnostics:[{message:{name,body,suggestion},
#            filename, location:{row,column}}], errors:[]}
#   "sarif"  SARIF 2.1.0, the tool-agnostic standard jarl also emits
#   "github" GitHub Actions annotation commands (mirrors jarl's github format)
.flowr_lint_render <- function(x, format) {
  n <- nrow(x)
  if (format == "github") {
    cmd <- c(error = "error", warning = "warning", style = "notice")
    lines <- vapply(seq_len(n), function(i) {
      row <- if (is.na(x$line_number[i])) 1L else x$line_number[i]
      col <- if (is.na(x$column_number[i])) 1L else x$column_number[i]
      sprintf("::%s title=flowr (%s),file=%s,line=%d,col=%d::%s:%d:%d: %s %s",
              cmd[[x$type[i]]] %||% "warning", x$linter[i], x$filename[i], row, col,
              x$filename[i], row, col, x$linter[i], x$message[i])
    }, character(1))
    return(structure(lines, class = "flowr_lints_text"))
  }
  if (format == "jarl") {
    diags <- lapply(seq_len(n), function(i) list(
      message  = list(name = x$linter[i], body = x$message[i], suggestion = NULL),
      filename = x$filename[i],
      location = list(row = x$line_number[i], column = x$column_number[i])
    ))
    return(jsonlite::toJSON(list(diagnostics = diags, errors = list()),
                            auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null"))
  }
  # sarif
  level <- c(error = "error", warning = "warning", style = "note")
  results <- lapply(seq_len(n), function(i) {
    physical <- list(artifactLocation = list(uri = x$filename[i]))
    if (!is.na(x$line_number[i])) {
      physical$region <- list(
        startLine = x$line_number[i],
        startColumn = if (is.na(x$column_number[i])) 1L else x$column_number[i])
    }
    list(ruleId = x$linter[i], level = unname(level[x$type[i]]) %||% "warning",
         message = list(text = x$message[i]),
         locations = list(list(physicalLocation = physical)))
  })
  sarif <- list(
    `$schema` = "https://json.schemastore.org/sarif-2.1.0.json",
    version = "2.1.0",
    runs = list(list(
      tool = list(driver = list(
        name = "flowr",
        informationUri = "https://github.com/flowr-analysis/flowr",
        rules = lapply(unique(x$linter), function(r) list(id = r)))),
      results = results)))
  jsonlite::toJSON(sarif, auto_unbox = TRUE, pretty = TRUE, na = "null")
}

#' Lint R code with flowR
#'
#' Runs flowR's linter and returns its findings. By default (`format =
#' "data.frame"`) the result is a tidy data frame with the same column names as
#' `lintr` (`filename`, `line_number`, `column_number`, `type`, `message`,
#' `line`, `linter`) plus a flowR-specific `certainty`, so it drops straight into
#' lintr-based reporting. The other formats emit exactly what
#' [jarl](https://jarl.etiennebacher.com) produces --- its native `"jarl"` JSON,
#' `"sarif"` (SARIF 2.1.0), or `"github"` Actions annotations --- so flowr's
#' output is interchangeable with jarl's in CI and editors. flowR's linter covers
#' issues such as absolute file paths, unused definitions, naming conventions,
#' deprecated and network functions, unseeded randomness, dead code and possible
#' leaked credentials.
#'
#' With no `code`/`file`/`folder`, lints the current R project (the working
#' directory's package or `.R` files), like the other flowr commands.
#'
#' @inheritParams slice
#' @param format Output format: `"data.frame"` (default, lintr-compatible),
#'   `"jarl"` (jarl's native JSON), `"sarif"` (SARIF 2.1.0), or `"github"`
#'   (GitHub Actions annotation lines).
#' @return For `format = "data.frame"`, a `flowr_lints` data frame (one row per
#'   finding; the full flowR response kept in its `raw` attribute; printing shows
#'   a compact, grouped report). For the other formats, the serialized output (a
#'   JSON string, or a character vector of annotation lines).
#' @seealso [query()], [slice()], [inspect_project()]
#' @inheritSection slice Analysing the current project
#' @export
#' @examples
#' \dontrun{
#' code <- "setwd('/tmp/x')\nunused_var <- 1"
#' lint(code)                       # lintr-compatible data frame
#' lint()                           # the current project
#' subset(lint(code), type == "warning")
#'
#' # jarl-compatible machine-readable output for CI
#' lint(code, format = "sarif")
#' cat(lint(code, format = "github"), sep = "\n")
#' }
lint <- function(code = NULL, file = NULL, folder = NULL, session = NULL,
                 format = c("data.frame", "jarl", "sarif", "github")) {
  format <- match.arg(format)
  done <- .flowr_timer("lint"); on.exit(done(), add = TRUE)
  res <- query(code = code, file = file, folder = folder,
               query = "linter", session = session)
  linter <- res$linter %||% list(results = list())
  rows <- .flowr_lint_rows(linter$results %||% list(), code = code, file = file)
  lints <- structure(rows, class = c("flowr_lints", "data.frame"), raw = linter)
  if (format == "data.frame") lints else .flowr_lint_render(lints, format)
}

#' @export
print.flowr_lints_text <- function(x, ...) {
  cat(unclass(x), sep = "\n")
  invisible(x)
}

#' @export
print.flowr_lints <- function(x, ...) {
  color <- .flowr_use_color()
  n <- nrow(x)
  if (n == 0L) {
    cat(.flowr_ansi("flowr: no lint findings.", "32", color), "\n", sep = "")
    return(invisible(x))
  }
  hdr <- sprintf("flowr: %d lint finding%s", n, if (n == 1L) "" else "s")
  cat(.flowr_ansi(hdr, "1", color), "\n", sep = "")
  tcol <- c(error = "31", warning = "33", style = "36")
  for (i in seq_len(n)) {
    where <- if (is.na(x$line_number[i])) x$filename[i]
             else sprintf("%s:%s:%s", x$filename[i], x$line_number[i], x$column_number[i])
    tag <- .flowr_ansi(format(x$type[i], width = 7), tcol[[x$type[i]]] %||% "0", color)
    cat(sprintf("  %s %s  %s ", tag, where, x$message[i]),
        .flowr_ansi(paste0("[", x$linter[i], "]"), "90", color), "\n", sep = "")
  }
  invisible(x)
}

#' @export
as.data.frame.flowr_lints <- function(x, ...) {
  # a plain data.frame view for lintr-style tooling (drops the S3 subclass)
  structure(x, class = "data.frame", raw = NULL)
}
