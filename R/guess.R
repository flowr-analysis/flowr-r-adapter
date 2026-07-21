# Dependency version guessing --------------------------------------------------
#
# flowR 2.13 added a proper solver over the version space (bounded by R release
# dates) that guesses the version range each dependency of a project must
# satisfy, combining what is *declared* (DESCRIPTION/lockfile/transitive
# constraints) with what the code actually calls, matched against the
# signature database (a parameter a function only gained in some version
# raises the lower bound). This wraps flowR's `guess-dep-versions` query.

# One evidence source's ANSI colour, matching flowR's own REPL legend.
.flowr_guess_evidence_colors <- c(
  declared = "36", transitive = "34", signature = "35", date = "33",
  "base-r" = "32", available = "37", indirect = "31"
)

# Every evidence source flowR's guess-dep-versions query understands, so a
# typo in `disable_evidence` is caught here instead of round-tripping to flowR.
.flowr_guess_evidence_sources <- c("declared", "transitive", "signature", "date",
                                   "base-r", "available", "indirect")

# `signature` evidence (a parameter only present from some version) needs the
# "history" sigdb -- "current" only has the latest version, so without it this
# evidence source can never fire. Interactive sessions get offered the install
# once per session; others just get a one-line note and proceed regardless.
.flowr_guess_check_history <- function(version) {
  if ("history" %in% .flowr_sigdb_scopes_installed(version) ||
      isTRUE(.flowr_state$history_sigdb_asked)) {
    return(invisible(NULL))
  }
  .flowr_state$history_sigdb_asked <- TRUE
  have <- .flowr_sigdb_scopes_installed(version)
  have_txt <- if (length(have) == 0) "none installed" else paste(have, collapse = ", ")
  if (!.flowr_interactive()) {
    if (!isTRUE(flowr_option("quiet"))) {
      message("[flowr] version guesses from usage need the \"history\" signature ",
              "database (you have: ", have_txt, "); install with ",
              "flowr_install(sigdb = \"history\").")
    }
    return(invisible(NULL))
  }
  message("[flowr] version guesses from usage need the \"history\" signature ",
          "database (~35 MB, past package versions); you have: ", have_txt)
  ans <- tolower(trimws(.flowr_readline("Install it now? [Y/n] ")))
  if (ans %in% c("n", "no")) {
    return(invisible(NULL))
  }
  tryCatch(
    .flowr_ensure_sigdb(version, quiet = FALSE, scopes = "history"),
    error = function(e) .flowr_warn("could not install the \"history\" signature ",
                                    "database: ", conditionMessage(e))
  )
  invisible(NULL)
}

#' Guess the version range a project's dependencies must satisfy
#'
#' Wraps flowR's `guess-dep-versions` query: for every dependency a project
#' declares or calls, combines the constraints it *declares* (a `DESCRIPTION`
#' range, an `rproject.toml`/lockfile pin, and their transitive requirements)
#' with what the code actually *does* (which package functions it calls and
#' with which arguments, matched against the signature database --- a named
#' argument a function only gained in some version raises the lower bound).
#' An optional `date` caps every guess to releases available at that point in
#' time, and base-R packages are additionally bounded by the assumed/declared
#' R version.
#'
#' Needs a signature database installed (see [flowr_install()]); without one,
#' the result carries no dependencies and `$message` explains why. Usage-based
#' evidence also needs the `"history"` set specifically (`"current"` only
#' carries the latest version of each package): missing it, an interactive
#' session is offered the install once; others get a one-line note instead.
#'
#' @inheritParams slice
#' @param packages Restrict the guess to these packages; omit to guess for
#'   every declared and used dependency.
#' @param date Only consider versions released on or before this day
#'   (`"YYYY.MM.DD"`, also `"YYYY"` or `"YYYY.MM"`).
#' @param clean Ignore the project's declared constraints (`DESCRIPTION`
#'   ranges, lockfile pins, transitive requirements); guess purely from code
#'   usage and the date/R bounds. Sugar for `disable_evidence = c("declared",
#'   "transitive")`.
#' @param disable_evidence Exclude these evidence sources from consideration
#'   entirely (flowR >= 2.13.1): any of `"declared"`, `"transitive"`,
#'   `"signature"`, `"date"`, `"base-r"`, `"available"`, `"indirect"`.
#' @param max_candidates Cap the number of candidate versions listed per
#'   dependency in the result (flowR default 16).
#' @param max_iterations Bound both fixpoint loops (mutual transitive
#'   refinement and arc consistency).
#' @param explode Also explode the guessed space into concrete per-dependency
#'   version assignments (see `$assignments` in the return value).
#' @param explode_order With `explode`, iterate each dependency's versions
#'   newest-first (default) or oldest-first.
#' @param explode_prefer With `explode`, a named character vector of a version
#'   to prefer per dependency when it survives the constraints, e.g.
#'   `c(dplyr = "1.1.4")`.
#' @param explode_limit With `explode`, cap the number of assignments produced.
#' @return A `flowr_guess`: `$dependencies` (a data frame, one row per
#'   dependency, with list-columns `candidates`, `declared_constraints`,
#'   `linked_with` and `evidence`), plus `$date_cutoff`, `$r_version`,
#'   `$runnable_combinations`/`$possible_combinations`, `$linked_groups`,
#'   `$assignments` (with `explode`) and `$message`.
#' @seealso [flowr_install()], [query()]
#' @inheritSection slice Analysing the current project
#' @export
#' @examples
#' \dontrun{
#' flowr_install()          # once, to get the signature database
#' flowr_guess_versions()   # the R package/project in the working directory
#' flowr_guess_versions(packages = "dplyr", date = "2023.01")
#' flowr_guess_versions(disable_evidence = "signature")   # declared/usage only
#' }
flowr_guess_versions <- function(code = NULL, file = NULL, folder = NULL,
                                 packages = NULL, date = NULL, clean = FALSE,
                                 disable_evidence = NULL,
                                 max_candidates = NULL, max_iterations = NULL,
                                 explode = FALSE,
                                 explode_order = c("newest", "oldest"),
                                 explode_prefer = NULL, explode_limit = NULL,
                                 session = NULL) {
  done <- .flowr_timer("guess_versions"); on.exit(done(), add = TRUE)
  clean <- .flowr_flag(clean, "clean")
  explode <- .flowr_flag(explode, "explode")
  explode_order <- match.arg(explode_order)
  if (!is.null(disable_evidence)) {
    bad <- setdiff(disable_evidence, .flowr_guess_evidence_sources)
    if (length(bad) > 0) {
      .flowr_stop("unknown evidence source(s) in `disable_evidence`: ", paste(bad, collapse = ", "),
           "\n  pick from: ", paste(.flowr_guess_evidence_sources, collapse = ", "))
    }
  }
  if (!"signature" %in% disable_evidence) {
    .flowr_guess_check_history(flowr_option("flowr_version"))
  }

  qobj <- list(type = "guess-dep-versions")
  if (!is.null(packages)) qobj$packages <- I(as.character(packages))
  if (!is.null(date)) qobj$date <- as.character(date)
  if (clean) qobj$clean <- TRUE
  if (!is.null(disable_evidence)) qobj$disabled <- I(as.character(disable_evidence))
  if (!is.null(max_candidates)) qobj$maxCandidates <- as.integer(max_candidates)
  if (!is.null(max_iterations)) qobj$maxIterations <- as.integer(max_iterations)
  if (explode) {
    eobj <- list(order = explode_order)
    if (!is.null(explode_prefer)) {
      if (is.null(names(explode_prefer)) || any(!nzchar(names(explode_prefer)))) {
        .flowr_stop("`explode_prefer` must be named, e.g. c(dplyr = \"1.1.4\")")
      }
      eobj$prefer <- as.list(explode_prefer)
    }
    if (!is.null(explode_limit)) eobj$limit <- as.integer(explode_limit)
    qobj$explode <- eobj
  }

  session <- .flowr_resolve_session(session)
  res <- query(code = code, file = file, folder = folder, query = qobj,
              session = session)[["guess-dep-versions"]]
  structure(.flowr_guess_build(res), class = "flowr_guess")
}

# Turn the raw `guess-dep-versions` query result into a tidy flowr_guess.
.flowr_guess_build <- function(res) {
  deps <- res$dependencies %||% list()
  n <- length(deps)
  chr <- function(fn) {
    if (n == 0) return(character(0))
    vapply(deps, function(d) { v <- fn(d); if (is.null(v)) NA_character_ else as.character(v) }, character(1))
  }
  lgl <- function(fn) {
    if (n == 0) return(logical(0))
    vapply(deps, function(d) { v <- fn(d); if (is.null(v)) NA else isTRUE(v) }, logical(1))
  }
  int <- function(fn) {
    if (n == 0) return(integer(0))
    vapply(deps, function(d) { v <- fn(d); if (is.null(v)) NA_integer_ else as.integer(v) }, integer(1))
  }
  df <- data.frame(
    package = chr(function(d) d$package),
    base = lgl(function(d) d$base),
    used = lgl(function(d) d$used),
    range = chr(function(d) d$range),
    min_version = chr(function(d) d$minVersion),
    max_version = chr(function(d) d$maxVersion),
    candidate_count = int(function(d) d$candidateCount),
    total_versions = int(function(d) d$totalVersions),
    unsatisfiable = lgl(function(d) d$unsatisfiable),
    truncated = lgl(function(d) d$truncated),
    stringsAsFactors = FALSE
  )
  df$candidates <- lapply(deps, function(d) unlist(d$candidates %||% list()))
  df$declared_constraints <- lapply(deps, function(d) unlist(d$declaredConstraints %||% list()))
  df$linked_with <- lapply(deps, function(d) unlist(d$linkedWith %||% list()))
  df$evidence <- lapply(deps, function(d) d$evidence %||% list())

  list(
    dependencies = df,
    date_cutoff = res$dateCutoff %||% NA_character_,
    r_version = res$rVersion %||% NA_character_,
    version_selection = res$versionSelection %||% NA_character_,
    runnable_combinations = res$runnableCombinations %||% NA_real_,
    possible_combinations = res$possibleCombinations %||% NA_real_,
    linked_groups = lapply(res$linkedGroups %||% list(), unlist),
    assignments = lapply(res$assignments %||% list(), function(a) unlist(a$versions)),
    message = res$message,
    raw = res
  )
}

#' @export
print.flowr_guess <- function(x, color = .flowr_use_color(), ...) {
  df <- x$dependencies
  n <- nrow(df)
  head <- sprintf("guess-dep-versions | %d dependenc%s", n, if (n == 1) "y" else "ies")
  if (!is.na(x$r_version)) head <- paste0(head, sprintf("  (R %s)", x$r_version))
  if (!is.na(x$date_cutoff)) head <- paste0(head, sprintf("  up to %s", x$date_cutoff))
  cat(.flowr_ansi(head, "1", color), "\n", sep = "")
  if (!is.null(x$message)) {
    cat("  ", .flowr_ansi(x$message, "31", color), "\n", sep = "")
  }
  if (n == 0) {
    if (is.null(x$message)) cat("  (no dependencies found)\n")
    return(invisible(x))
  }
  if (!is.na(x$runnable_combinations) && !is.na(x$possible_combinations) &&
      x$possible_combinations > 0) {
    pct <- 100 * x$runnable_combinations / x$possible_combinations
    cat("  ", sprintf("runnable combinations: %s / %s (%s%%)",
                      format(x$runnable_combinations, big.mark = ",", scientific = FALSE),
                      format(x$possible_combinations, big.mark = ",", scientific = FALSE),
                      if (pct < 10) sprintf("%.1f", pct) else round(pct)),
        "\n", sep = "")
  }
  for (grp in x$linked_groups) {
    cat("  ", .flowr_ansi(paste0("linked: ", paste(grp, collapse = " + ")), "90", color),
        "\n", sep = "")
  }
  for (i in seq_len(n)) {
    row <- df[i, ]
    linked <- row$linked_with[[1]]
    tag <- if (isTRUE(row$base)) " [base]"
           else if (length(linked) > 0) paste0(" [linked: ", paste(linked, collapse = ", "), "]")
           else ""
    note <- if (!isTRUE(row$base) && identical(row$used, FALSE)) " (not called)" else ""
    # flowR doesn't always set `unsatisfiable` (e.g. a `date` cutoff), but 0
    # candidates is just as dead an end
    no_candidates <- !is.na(row$candidate_count) && row$candidate_count == 0
    range <- if (isTRUE(row$unsatisfiable)) {
      .flowr_ansi("unsatisfiable", "31", color)
    } else if (no_candidates) {
      .flowr_ansi(paste0(row$range, "  (no version satisfies this)"), "31", color)
    } else {
      .flowr_ansi(row$range, "1", color)
    }
    count <- if (!is.na(row$total_versions)) sprintf("%d/%d versions", row$candidate_count, row$total_versions) else sprintf("%d candidate(s)", row$candidate_count)
    cat(sprintf("  %s%s%s  %s  ", row$package, tag, note, range),
        .flowr_ansi(paste0("(", count, ")"), "90", color), "\n", sep = "")
    ev <- row$evidence[[1]]
    if (length(ev) > 0) {
      sources <- vapply(ev, function(e) e$source %||% "?", character(1))
      counts <- table(sources)
      summary <- paste(vapply(names(counts), function(s) {
        .flowr_ansi(sprintf("%sx%d", s, as.integer(counts[[s]])),
                    .flowr_guess_evidence_colors[[s]] %||% "90", color)
      }, character(1)), collapse = "  ")
      cat("     ", summary, "\n", sep = "")
    }
  }
  if (length(x$assignments) > 0) {
    cat(.flowr_ansi(sprintf("assignments: %d", length(x$assignments)), "1", color), "\n", sep = "")
    first <- x$assignments[[1]]
    cat("  preferred: ", paste(sprintf("%s@%s", names(first), first), collapse = ", "), "\n", sep = "")
  }
  invisible(x)
}
