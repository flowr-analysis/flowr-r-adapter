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
        fixable       = length(x$quickFix %||% list()) > 0L,
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(rows) == 0L) {
    return(data.frame(
      filename = character(0), line_number = integer(0), column_number = integer(0),
      type = character(0), message = character(0), line = character(0),
      linter = character(0), certainty = character(0), fixable = logical(0),
      stringsAsFactors = FALSE))
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
#' `line`, `linter`) plus flowR-specific `certainty` and `fixable` columns, so it
#' drops straight into lintr-based reporting. The other formats emit exactly what
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
#' @param rules Which linter rules to run, as a character vector of rule names
#'   (e.g. `c("absolute-file-paths", "unused-definitions")`). `NULL` (default)
#'   uses the active set from `options(flowr.lint_rules = )` /
#'   [flowr_set_config()], which itself defaults to flowR's full rule set --- the
#'   same rules the flowR VS Code extension enables. See the *Linting* section of
#'   `vignette("flowr-engines")` for the rule list and configuration.
#' @param full Show every finding when printing. `FALSE` (default) prints at
#'   most `getOption("flowr.lint_max")` (10) findings per rule, with the full
#'   count in `(N)` and a `... N more` line; the returned data frame always
#'   contains all findings regardless.
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
#' flowr_lint(code)                 # lintr-compatible data frame
#' flowr_lint()                     # the current project
#' subset(flowr_lint(code), type == "warning")
#'
#' # pick which rules run (per call, or as a persistent default)
#' flowr_lint(code, rules = c("absolute-file-paths", "unused-definitions"))
#' flowr_set_config(lint_rules = c("absolute-file-paths", "seeded-randomness"))
#'
#' # jarl-compatible machine-readable output for CI
#' flowr_lint(code, format = "sarif")
#' cat(flowr_lint(code, format = "github"), sep = "\n")
#' }
flowr_lint <- function(code = NULL, file = NULL, folder = NULL, session = NULL,
                       rules = NULL, full = FALSE,
                       format = c("data.frame", "jarl", "sarif", "github")) {
  format <- match.arg(format)
  done <- .flowr_timer("lint"); on.exit(done(), add = TRUE)
  session <- .flowr_resolve_session(session)
  # Analyse once (this also resolves + announces the project root), so we can
  # report flowR's per-phase progress and reuse the cached analysis for a fix.
  an <- .flowr_input_analysis(code, file, folder, cfg = FALSE, session = session)$an
  phases <- .flowr_analysis_phases(an$analysis)
  .flowr_timing_detail(phases)
  if (!isTRUE(flowr_option("quiet")) && length(phases)) {
    message("[flowr] ", paste(sprintf("%s %gms", names(phases),
            as.numeric(unlist(phases))), collapse = ", "), "; running linter ...")
  }
  qobj <- list(type = "linter")
  active <- rules %||% flowr_option("lint_rules")   # empty -> flowR's default set
  if (length(active)) qobj$rules <- as.list(as.character(active))
  req <- list(type = "request-query", id = .flowr_session_id(session),
              filetoken = an$filetoken, query = I(list(qobj)))
  linter <- .flowr_request(session$con, req)$results$linter %||% list(results = list())
  rows <- .flowr_lint_rows(linter$results %||% list(), code = code, file = file)
  lints <- structure(rows, class = c("flowr_lints", "data.frame"),
                     raw = linter, source = .flowr_source_label(code, file, folder),
                     input = list(code = code, file = file, folder = folder),
                     full = isTRUE(full))
  if (format == "data.frame") lints else .flowr_lint_render(lints, format)
}

#' @export
print.flowr_lints_text <- function(x, ...) {
  cat(unclass(x), sep = "\n")
  invisible(x)
}

# The flowR wiki page documenting a linter rule. The wiki uses stable, encoded
# titles: "absolute-file-paths" -> ".../wiki/%5BLinting-Rule%5D-Absolute-File-Paths".
.flowr_rule_doc_url <- function(rule) {
  words <- strsplit(rule, "-", fixed = TRUE)[[1]]
  title <- paste(toupper(substring(words, 1, 1)), substring(words, 2), sep = "", collapse = "-")
  paste0("https://github.com/flowr-analysis/flowr/wiki/%5BLinting-Rule%5D-", title)
}

# A clickable file:line:col location (OSC 8 link to the file on capable
# terminals); left as plain text for inline code or when links are unsupported.
.flowr_lint_loc <- function(file, line, col, color, link) {
  loc <- if (is.na(line)) file else sprintf("%s:%s:%s", file, line, col)
  if (link && !identical(file, "<text>") && file.exists(file)) {
    url <- paste0("file://", normalizePath(file, mustWork = FALSE))
    return(.flowr_hyperlink(loc, url, color))
  }
  loc
}

#' @export
print.flowr_lints <- function(x, ...) {
  color <- .flowr_use_color()
  link <- color && .flowr_hyperlinks_supported()
  n <- nrow(x)
  src <- attr(x, "source")
  where <- if (!is.null(src)) paste0(" in ", src) else ""
  if (n == 0L) {
    cat(.flowr_ansi(paste0("flowr: no lint findings", where), "32", color), "\n", sep = "")
    return(invisible(x))
  }
  # process summary: total + how findings break down by severity.
  tcol <- c(error = "31", warning = "33", style = "36")
  ord <- intersect(c("error", "warning", "style"), unique(x$type))
  brk <- paste(vapply(ord, function(t) paste0(sum(x$type == t), " ", t), character(1)),
               collapse = ", ")
  hdr <- sprintf("flowr: %d lint finding%s%s (%s)",
                 n, if (n == 1L) "" else "s", where, brk)
  cat(.flowr_ansi(hdr, "1", color), "\n", sep = "")

  # group findings by rule: one rule header (linking to its flowR docs), then its
  # occurrences beneath it, so repeated warnings collapse instead of flooding.
  # Unless full = TRUE, cap each rule at flowr.lint_max rows (the header still
  # shows the full count) and note how many were hidden.
  maxper <- if (isTRUE(attr(x, "full"))) Inf else max(1L, as.integer(flowr_option("lint_max")))
  for (rule in unique(x$linter)) {
    idx <- which(x$linter == rule)
    sev <- x$type[idx[1]]
    nfix <- sum(x$fixable[idx])
    label <- .flowr_hyperlink(rule, .flowr_rule_doc_url(rule), link)
    tag <- .flowr_ansi(format(sev, width = 7), tcol[[sev]] %||% "0", color)
    fixnote <- if (nfix > 0) .flowr_ansi(sprintf("  %d fixable", nfix), "32", color) else ""
    cat(sprintf("  %s %s %s%s\n", tag,
                .flowr_ansi(label, "1", color),
                .flowr_ansi(sprintf("(%d)", length(idx)), "90", color), fixnote))
    shown <- if (length(idx) > maxper) idx[seq_len(maxper)] else idx
    for (i in shown) {
      loc <- .flowr_lint_loc(x$filename[i], x$line_number[i], x$column_number[i], color, link)
      mark <- if (isTRUE(x$fixable[i])) .flowr_ansi(" [fix]", "32", color) else ""
      cat("      ", .flowr_ansi(loc, "90", color), "  ", x$message[i], mark, "\n", sep = "")
    }
    if (length(idx) > length(shown)) {
      cat("      ", .flowr_ansi(sprintf("... %d more (full = TRUE to show all)",
                                        length(idx) - length(shown)), "90", color), "\n", sep = "")
    }
  }
  if (any(x$fixable)) {
    cat(.flowr_ansi(sprintf("-> %d fixable; apply with flowr_lint_fix()", sum(x$fixable)),
                    "90", color), "\n", sep = "")
  }
  invisible(x)
}

#' @export
as.data.frame.flowr_lints <- function(x, ...) {
  # a plain data.frame view for lintr-style tooling (drops the S3 subclass)
  structure(x, class = "data.frame", raw = NULL, source = NULL)
}

# Quick fixes --------------------------------------------------------------

# Pull every quick fix out of a raw `linter` result: a flat list of edits, each
# with its file (loc[[5]], or NA for inline code), 1-based inclusive char range
# (sl, sc, el, ec), the replacement text ("" for a removal) and a description.
.flowr_collect_fixes <- function(results) {
  out <- list()
  for (rule in names(results)) {
    for (x in (results[[rule]]$results %||% list())) {
      for (qf in (x$quickFix %||% list())) {
        loc <- qf$loc
        out[[length(out) + 1L]] <- list(
          rule = rule,
          file = if (length(loc) >= 5L) as.character(loc[[5]]) else NA_character_,
          sl = as.integer(loc[[1]]), sc = as.integer(loc[[2]]),
          el = as.integer(loc[[3]]), ec = as.integer(loc[[4]]),
          replacement = qf$replacement %||% "",
          description = qf$description %||% paste0(rule, " fix"))
      }
    }
  }
  out
}

# Apply a set of edits to a single text (one string with '\n' separators).
# Edits use 1-based inclusive [line,col] ranges; we splice by absolute character
# offset from the end backwards so earlier offsets stay valid, and skip any edit
# that overlaps one already applied.
.flowr_apply_fixes_to_text <- function(text, edits) {
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  if (length(lines) == 0L) lines <- ""
  # Columns are treated as R character counts. This matches flowR for ASCII /
  # BMP text; a non-BMP character (e.g. an emoji) before a fixed token could
  # shift the offset if flowR counts columns in UTF-16 code units, so quick-fix
  # application on such lines is best-effort.
  prefix <- c(0L, cumsum(nchar(lines) + 1L))   # chars before each 1-based line
  idx <- function(line, col) prefix[line] + col
  spans <- lapply(edits, function(e) list(
    start = idx(e$sl, e$sc), end = idx(e$el, e$ec), rep = e$replacement))
  spans <- spans[order(vapply(spans, function(s) s$start, 0), decreasing = TRUE)]
  s <- text
  guard <- Inf
  applied <- 0L
  for (sp in spans) {
    if (sp$end >= guard) next                  # overlaps a later edit; skip
    if (identical(substr(s, sp$start, sp$end), sp$rep)) next  # no-op fix; ignore
    s <- paste0(substr(s, 1L, sp$start - 1L), sp$rep, substr(s, sp$end + 1L, nchar(s)))
    guard <- sp$start
    applied <- applied + 1L
  }
  list(text = s, applied = applied)
}

#' Apply flowR's linter quick fixes
#'
#' Applies the automatic fixes flowR attaches to some findings (currently
#' absolute file paths, unused definitions and naming conventions) --- the same
#' quick fixes the flowR VS Code extension offers. When linting **files** or a
#' **folder**, the fixes are written back to those files in place (use
#' `preview = TRUE` to get the fixed text without touching disk). When linting a
#' snippet via `code`, the fixed code is returned as a string.
#'
#' Pass a [flowr_lint()] result as `code` to reuse it directly --- no
#' re-analysis, so applying fixes right after linting is instant.
#'
#' @inheritParams flowr_lint
#' @param code A string of R source, or a [flowr_lint()] result to reuse (see
#'   *Details*). See [flowr_lint()] for `file`/`folder`/`rules`.
#' @param preview If `TRUE`, do not write files; return the fixed text instead
#'   (a character vector named by file).
#' @return Invisibly: the fixed code string for `code` input; for file/folder
#'   input, a named integer of fixes applied per file, or --- when
#'   `preview = TRUE` --- the fixed text of each file. Findings without a quick
#'   fix are left untouched.
#' @seealso [flowr_lint()]
#' @inheritSection slice Analysing the current project
#' @export
#' @examples
#' \dontrun{
#' flowr_lint_fix("x = 1")                       # -> "x <- 1" (returned)
#' flowr_lint_fix(file = "analysis.R")           # rewrites the file in place
#' cat(flowr_lint_fix(file = "analysis.R", preview = TRUE))  # dry run
#'
#' l <- flowr_lint(file = "analysis.R")          # lint once ...
#' flowr_lint_fix(l)                              # ... then fix instantly (reused)
#' }
flowr_lint_fix <- function(code = NULL, file = NULL, folder = NULL, session = NULL,
                           rules = NULL, preview = FALSE) {
  # pass a flowr_lint() result as `code` to reuse it (no re-analysis, instant);
  # otherwise lint the given code/file/folder first.
  if (inherits(code, "flowr_lints")) {
    lints <- code
    inp <- attr(lints, "input") %||% list()
    code <- inp$code; file <- inp$file; folder <- inp$folder
  } else {
    lints <- flowr_lint(code = code, file = file, folder = folder,
                        session = session, rules = rules)
  }
  fixes <- .flowr_collect_fixes(attr(lints, "raw")$results %||% list())
  quiet <- isTRUE(flowr_option("quiet"))
  if (length(fixes) == 0L) {
    if (!quiet) message("[flowr] no quick fixes available for these findings")
    return(invisible(if (!is.null(code)) code else character(0)))
  }
  # inline code: no file to write, so return the fixed source
  if (!is.null(code) && is.null(file) && is.null(folder)) {
    res <- .flowr_apply_fixes_to_text(code, fixes)
    if (!isTRUE(flowr_option("quiet"))) {
      message("[flowr] applied ", res$applied, " quick fix",
              if (res$applied == 1L) "" else "es", " to the supplied code")
    }
    return(res$text)
  }
  # file/folder: group edits by their file and rewrite each in place
  withfile <- Filter(function(f) !is.na(f$file) && nzchar(f$file), fixes)
  if (length(withfile) == 0L) {
    if (!quiet) message("[flowr] fixes are available but carry no file location to write to")
    return(invisible(stats::setNames(integer(0), character(0))))
  }
  byfile <- split(withfile, vapply(withfile, function(f) f$file, character(1)))
  applied <- integer(0)
  previews <- character(0)
  for (path in names(byfile)) {
    # read the exact bytes (readLines + paste would drop a trailing newline) and
    # write back with cat (writeLines would add one), so files that had no final
    # newline are not gratuitously changed outside the fixed region.
    orig <- readChar(path, file.info(path)$size, useBytes = FALSE)
    res <- .flowr_apply_fixes_to_text(orig, byfile[[path]])
    if (isTRUE(preview)) {
      previews[[path]] <- res$text
    } else {
      cat(res$text, file = path)
    }
    applied[[path]] <- res$applied
  }
  if (!quiet) {
    verb <- if (isTRUE(preview)) "would apply" else "applied"
    message("[flowr] ", verb, " ", sum(applied), " quick fix",
            if (sum(applied) == 1L) "" else "es", " across ", length(applied), " file(s)")
  }
  # preview returns the fixed text (per file); a real run returns the counts.
  if (isTRUE(preview)) invisible(previews) else invisible(applied)
}
