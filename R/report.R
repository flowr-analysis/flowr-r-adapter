.flowr_repo_url <- "https://github.com/flowr-analysis/flowr-r-adapter"

# Installed versions (from the local package db) of flowr's own dependencies,
# read from its DESCRIPTION so the list never goes stale. "?" if not installed.
.flowr_pkg_db_versions <- function() {
  fields <- utils::packageDescription("flowr", fields = c("Imports", "Suggests"))
  deps <- unlist(strsplit(paste(stats::na.omit(fields), collapse = ","), ","))
  deps <- trimws(sub("\\(.*\\)", "", deps))                # drop version constraints
  deps <- setdiff(deps[nzchar(deps)], c("R", "utils", "tools", "methods", "stats"))
  if (length(deps) == 0) {
    return("")
  }
  ver <- vapply(sort(deps), function(p) {
    tryCatch(as.character(utils::packageVersion(p)), error = function(e) "?")
  }, character(1))
  paste(sprintf("%s %s", names(ver), ver), collapse = ", ")
}

.flowr_report_meta <- function() {
  s <- .flowr_state$default
  engine <- if (!is.null(s) && isTRUE(is_flowr_session(s)) && !isTRUE(s$closed)) {
    sprintf("%s (flowR %s, %s)", s$handle$provider, s$versions$flowr %||% "?",
            s$versions$engine %||% "?")
  } else {
    sprintf("not started (configured: %s)", flowr_option("engine"))
  }
  plat <- tryCatch(.flowr_platform()$key, error = function(e) "unknown")
  paste(
    sprintf("- flowr (R adapter): %s", utils::packageVersion("flowr")),
    sprintf("- engine: %s", engine),
    sprintf("- flowR target version: %s", flowr_option("flowr_version")),
    sprintf("- R: %s", R.version.string),
    sprintf("- platform: %s (%s %s)", plat, Sys.info()[["sysname"]], Sys.info()[["release"]]),
    sprintf("- sandbox mode (tree-sitter, no shell/eval): %s", flowr_option("secure")),
    sprintf("- package db: %s", .flowr_pkg_db_versions()),
    sep = "\n"
  )
}

.flowr_issue_url <- function(title, body, labels = NULL) {
  enc <- function(x) utils::URLencode(x, reserved = TRUE)
  url <- sprintf("%s/issues/new?title=%s&body=%s", .flowr_repo_url, enc(title), enc(body))
  if (!is.null(labels)) url <- paste0(url, "&labels=", enc(labels))
  url
}

# Resolve a browser: the user's getOption("browser") / $R_BROWSER wins; otherwise
# auto-detect a system opener. Returns NULL when none can be found.
.flowr_browser <- function() {
  opt <- getOption("browser")
  if (is.function(opt) || (is.character(opt) && length(opt) >= 1L && any(nzchar(opt)))) {
    return(opt)
  }
  env <- Sys.getenv("R_BROWSER", "")
  if (nzchar(env) && !env %in% c("false", "none")) {
    return(env)
  }
  cands <- if (identical(Sys.info()[["sysname"]], "Darwin")) {
    "open"
  } else {
    c("xdg-open", "www-browser", "x-www-browser", "firefox",
      "google-chrome", "chromium-browser", "chromium")
  }
  for (cnd in cands) {
    p <- Sys.which(cnd)
    if (nzchar(p)) {
      return(unname(p))
    }
  }
  NULL
}

.flowr_open_url <- function(url) {
  if (interactive()) {
    opened <- tryCatch({
      if (.Platform$OS.type == "windows") {
        message("Opening your web browser ...")
        utils::browseURL(url)               # uses the default handler on Windows
      } else {
        br <- .flowr_browser()
        if (is.null(br)) .flowr_stop("no browser found")
        message("Opening your web browser ...")
        utils::browseURL(url, browser = br)
      }
      TRUE
    }, error = function(e) FALSE)
    if (isTRUE(opened)) {
      return(invisible(url))
    }
  }
  message("Could not open a browser (set options(browser=) to choose one).\n",
          "Open this URL to continue:\n  ", url)
  invisible(url)
}

#' Report a flowr bug on GitHub
#'
#' Opens your web browser at a prefilled GitHub issue with a bug-report template
#' and your environment (flowr and flowR versions, active engine, R version and
#' platform). Non-interactive sessions get the URL printed instead of opening a
#' browser.
#'
#' @param description Optional text describing the bug.
#' @return The issue URL, invisibly.
#' @seealso [flowr_feedback()], [flowr_status()]
#' @export
#' @examples
#' \dontrun{
#' flowr_bug_report("slice() returns empty code for a nested function")
#' }
flowr_bug_report <- function(description = "") {
  body <- paste0(
    "**Describe the bug**\n", description, "\n\n",
    "**To reproduce**\n\n\n",
    "**Expected behaviour**\n\n\n",
    "**Environment**\n", .flowr_report_meta(), "\n"
  )
  .flowr_open_url(.flowr_issue_url("[Bug] ", body, "bug"))
}

# The shared flowR feedback form (also used by the VS Code extension).
.flowr_feedback_url <- paste0(
  "https://docs.google.com/forms/d/e/",
  "1FAIpQLScKFhgnh9LGVU7QzqLvFwZe1oiv_5jNhkIO-G-zND0ppqsMxQ/viewform")

#' Send feedback about flowr
#'
#' Opens the shared flowR feedback form (the same one the VS Code extension
#' uses) in your web browser. Non-interactive sessions get the URL printed
#' instead of opening a browser.
#'
#' @return The form URL, invisibly.
#' @seealso [flowr_bug_report()]
#' @export
#' @examples
#' \dontrun{
#' flowr_feedback()
#' }
flowr_feedback <- function() {
  .flowr_open_url(.flowr_feedback_url)
}
