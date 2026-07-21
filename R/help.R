# Signature-backed help --------------------------------------------------------
#
# flowr_help() looks up a function or package definition the way `?fun`/help()
# does, but straight from flowR's signature database (see ?flowr_install):
# every CRAN package's exports, decoded from source, so a function resolves
# whether or not the package is actually installed locally.

# TRUE if `x` uses a glob wildcard (`*`, `?`), mirroring flowR's own matcher.
.flowr_has_glob <- function(x) {
  !is.null(x) && grepl("[*?]", x)
}

# Split a `help()`-style topic into package/function parts: "pkg::fn" splits
# outright; an explicit `package` pairs with `topic` as the function; a bare
# topic with no package is a function name (mirrors help()'s own default: an
# unqualified topic is looked up as a function/topic, not a package -- to see
# a package overview you pass `package = `, exactly as `help(package = )`).
.flowr_help_parse <- function(topic, package) {
  if (!is.null(topic) && grepl("::", topic, fixed = TRUE)) {
    parts <- strsplit(topic, "::", fixed = TRUE)[[1]]
    pkg <- if (nzchar(parts[1])) parts[1] else package
    # NA (not NULL) marks a "pkg::" with no function name after it
    fn <- if (length(parts) > 1 && nzchar(parts[2])) parts[2] else NA_character_
    return(list(package = pkg, fn = fn))
  }
  if (!is.null(package)) {
    return(list(package = package, fn = topic))
  }
  list(package = NULL, fn = topic)
}

# A callable-looking preview string, e.g. "ggplot(data = NULL, mapping = aes())".
.flowr_help_signature_str <- function(name, params) {
  if (length(params) == 0) {
    return(paste0(name, "()"))
  }
  parts <- vapply(params, function(p) {
    if (!is.null(p$default) && nzchar(p$default)) paste0(p$name, " = ", p$default) else as.character(p$name)
  }, character(1))
  paste0(name, "(", paste(parts, collapse = ", "), ")")
}

#' Look up a function or package the way `?`/`help()` does, backed by flowR's signature database
#'
#' Resolves `topic` against flowR's signature database (see *The signature
#' database* in [flowr_install()]) instead of the local library, so it works
#' for any CRAN package's exports whether or not that package is installed ---
#' the parameters and defaults of a function, whether it is exported/deprecated,
#' its S3 dispatch, source location, and CRAN-mirror/rdrr.io documentation
#' links.
#'
#' An unqualified `topic` (e.g. `"filter"`) is looked up as a **function**
#' across every loaded package, exactly like `help("filter")` searches the
#' attached search path --- pass `package` to see a package overview instead
#' (like `help(package = "dplyr")`), or qualify the topic (`"dplyr::filter"`)
#' to go straight to one package's version. A topic found in exactly one
#' package resolves directly to its full signature; several hits are listed
#' (like `apropos()`) so you can narrow with `package`.
#'
#' Needs a signature database installed (see [flowr_install()]); errors with
#' that hint if none is mounted.
#'
#' @param topic A function name (optionally `"pkg::fn"`), or `NULL` with
#'   `package` set for a package overview. Glob wildcards (`*`, `?`) search.
#' @param package Restrict to (or, with no `topic`, summarize) this package.
#'   Glob wildcards search package names.
#' @param version A version spec: an exact version, a glob (`"3.*"`), a semver
#'   range (`">=3.0.0"`), or a release-date bound (`"<=2026"`, `">=2021.05"`).
#' @param parameters Keep only functions with a parameter matching every one of
#'   these names (glob wildcards allowed, position-independent).
#' @param required_parameters Keep only functions with exactly this many
#'   required (no-default) parameters, excluding `...`.
#' @param call_graph For a single resolved function, also compute its
#'   transitive call graph as a mermaid.live link.
#' @param session A `flowr_session`; the active default session is used when
#'   `NULL` (one is started on first use).
#' @return A `flowr_help`, printed as a signature/package/match listing.
#' @seealso [flowr_install()], [query()]
#' @export
#' @examples
#' \dontrun{
#' flowr_install()                    # once, to get the signature database
#' flowr_help("dplyr::filter")        # a specific package's function
#' flowr_help("pivot_longer")         # bare topic: searched across packages
#' flowr_help(package = "ggplot2")    # package overview, like help(package=)
#' }
flowr_help <- function(topic = NULL, package = NULL, version = NULL,
                       parameters = NULL, required_parameters = NULL,
                       call_graph = FALSE, session = NULL) {
  done <- .flowr_timer("help"); on.exit(done(), add = TRUE)
  call_graph <- .flowr_flag(call_graph, "call_graph")
  parsed <- .flowr_help_parse(topic, package)
  pkg <- parsed$package
  fn <- parsed$fn
  if (identical(fn, NA_character_)) {
    .flowr_stop("`topic` \"", topic, "\" is missing a function name after \"::\"")
  }
  if (is.null(pkg) && is.null(fn)) {
    .flowr_stop("provide a `topic` (function name, optionally \"pkg::fn\") or a `package`")
  }
  # bare function name -> search every package, like help("fn")
  wildcard_search <- is.null(pkg) && !is.null(fn)

  qobj <- list(type = "signature")
  if (wildcard_search) {
    qobj$package <- "*"
    qobj[["function"]] <- fn
  } else {
    if (!is.null(pkg)) qobj$package <- pkg
    if (!is.null(fn)) qobj[["function"]] <- fn
  }
  if (!is.null(version)) qobj$version <- version
  if (!is.null(parameters)) qobj$parameters <- I(as.character(parameters))
  if (!is.null(required_parameters)) qobj$requiredParameters <- as.integer(required_parameters)
  if (call_graph) qobj$callGraph <- TRUE

  session <- .flowr_resolve_session(session)
  # flowR rejects empty content; "1" is a placeholder, no real code needed
  res <- query(code = "1", query = qobj, session = session)$signature

  if (identical(res$packageCount, 0L)) {
    .flowr_stop("no signature database is loaded; flowr_help() needs it. ",
         "Run flowr_install() once (see ?flowr_install), then try again.")
  }

  # `[[`, not `$`: `$` partial-matches, e.g. res$package would silently
  # resolve to packageCount when `package` is absent.
  matches <- res[["matches"]]
  # a lone non-glob hit over "*" resolves straight to the full function view
  if (wildcard_search && !.flowr_has_glob(fn) && !is.null(matches) &&
      length(matches) == 1L) {
    hit <- matches[[1]]
    return(flowr_help(topic = fn, package = hit$package, version = version,
                      parameters = parameters, required_parameters = required_parameters,
                      call_graph = call_graph, session = session))
  }

  kind <- if (!is.null(res[["function"]])) "function"
          else if (!is.null(matches)) "matches"
          else if (!is.null(res[["packages"]])) "packages"
          else if (!is.null(res[["package"]])) "package"
          else if (!is.null(res[["message"]])) "not_found"
          else "summary"

  structure(list(kind = kind, query = qobj, result = res), class = "flowr_help")
}

# One "name (params)" line for a function view, with the exported/deprecated/
# forced/S3 tags a reader of `?fn` would expect help() itself to show.
.flowr_help_print_function <- function(fn, color) {
  cat(.flowr_ansi(.flowr_help_signature_str(paste0(fn$package, "::", fn$name), fn$parameters),
                  "1", color), "\n", sep = "")
  tags <- c(
    if (!isTRUE(fn$exported)) "not exported",
    if ("deprecated" %in% (fn$properties %||% list())) "deprecated",
    if (isTRUE(fn$s3generic)) "S3 generic"
  )
  # `[[`: `$s3method` partial-matches the plural `$s3methods`
  s3method <- fn[["s3method"]]
  if (!is.null(s3method)) {
    tags <- c(tags, sprintf("S3 method of %s::%s", s3method$package, s3method$generic))
  }
  if (length(tags) > 0) {
    cat("  ", .flowr_ansi(paste(tags, collapse = ", "), "33", color), "\n", sep = "")
  }
  if (!is.null(fn$version)) {
    cat("  ", .flowr_ansi(sprintf("%s %s", fn$package, fn$version), "90", color), "\n", sep = "")
  }
  if (length(fn$parameters) > 0) {
    cat(.flowr_ansi("arguments", "1", color), "\n", sep = "")
    for (p in fn$parameters) {
      req <- if (isTRUE(p$required)) "required" else if (!is.null(p$default)) paste0("= ", p$default) else "optional"
      forced <- if (isTRUE(p$forced)) " (forced)" else ""
      cat(sprintf("  %-20s %s\n", p$name, .flowr_ansi(paste0(req, forced), "90", color)))
    }
  }
  if (isTRUE(fn$s3generic) && length(fn$s3methods) > 0) {
    cat(.flowr_ansi("S3 methods", "1", color), "\n", sep = "")
    cat("  ", paste(unlist(fn$s3methods), collapse = ", "), "\n", sep = "")
  }
  if (!is.null(fn$file)) {
    loc <- sprintf("  %s%s", fn$file, if (!is.null(fn$line)) paste0(":", fn$line) else "")
    url <- fn$sourceUrl %||% fn$docUrl
    cat(.flowr_ansi(.flowr_hyperlink(loc, url, color), "90", color), "\n", sep = "")
  }
  if (!is.null(fn$docUrl)) {
    cat("  ", .flowr_ansi(.flowr_hyperlink(fn$docUrl, fn$docUrl, color), "4;90", color), "\n", sep = "")
  }
  if (!is.null(fn$callGraph)) {
    cat("  ", .flowr_ansi(.flowr_hyperlink("call graph", fn$callGraph, color), "4;90", color), "\n", sep = "")
  }
}

.flowr_help_print_package <- function(pkg, color) {
  head <- sprintf("%s %s", pkg$name, pkg$version)
  if (!is.null(pkg$releaseDate)) head <- paste0(head, "  (", pkg$releaseDate, ")")
  cat(.flowr_ansi(head, "1", color), "\n", sep = "")
  kind <- if (isTRUE(pkg$base)) "base R" else if (isTRUE(pkg$cran)) "CRAN" else "unknown source"
  cat("  ", kind, sprintf(", %d export%s (%d function%s, %d deprecated)",
                          pkg$exportsTotal, if (pkg$exportsTotal == 1) "" else "s",
                          pkg$functionCount, if (pkg$functionCount == 1) "" else "s",
                          length(pkg$deprecated)), "\n", sep = "")
  if (length(pkg$dependencies) > 0) {
    dn <- vapply(pkg$dependencies, function(d) d$name, character(1))
    cat("  ", .flowr_ansi(paste("depends on:", paste(utils::head(dn, 10L), collapse = ", "),
                                if (length(dn) > 10L) "..." else ""), "90", color), "\n", sep = "")
  }
  url <- pkg$cranPage %||% pkg$repoUrl
  if (!is.null(url)) {
    cat("  ", .flowr_ansi(.flowr_hyperlink(url, url, color), "4;90", color), "\n", sep = "")
  }
}

#' @export
print.flowr_help <- function(x, color = .flowr_use_color(), ...) {
  res <- x$result
  switch(x$kind,
    "function" = .flowr_help_print_function(res[["function"]], color),
    "package"  = {
      .flowr_help_print_package(res$package, color)
      if (!is.null(res$message)) {
        cat(.flowr_ansi(res$message, "31", color), "\n", sep = "")
        if (length(res$suggestions) > 0) {
          cat("  did you mean: ", paste(unlist(res$suggestions), collapse = ", "), "?\n", sep = "")
        }
      }
    },
    "matches" = {
      n <- length(res$matches)
      if (n == 0) {
        # zero matches can mean "filtered out", not "doesn't exist" -- say which
        filters <- c(
          if (length(x$query[["parameters"]]) > 0) sprintf("parameter%s %s",
              if (length(x$query[["parameters"]]) == 1) "" else "s",
              paste(x$query[["parameters"]], collapse = ", ")),
          if (!is.null(x$query[["requiredParameters"]])) sprintf(
              "%d required parameter(s)", x$query[["requiredParameters"]])
        )
        msg <- if (length(filters) > 0) {
          sprintf("no function named '%s' matches %s",
                  x$query[["function"]] %||% "?", paste(filters, collapse = " and "))
        } else {
          sprintf("no function named '%s' found in the signature database",
                  x$query[["function"]] %||% "?")
        }
        cat(.flowr_ansi(msg, "31", color), "\n", sep = "")
        return(invisible(x))
      }
      cat(.flowr_ansi(sprintf("%d match%s", n, if (n == 1) "" else "es"), "1", color), "\n", sep = "")
      for (m in res$matches) {
        ver <- if (!is.null(m$version)) paste0("@", m$version) else ""
        cat(sprintf("  %s::%s%s", m$package, m$name, ver))
        if (!isTRUE(m$exported)) cat(.flowr_ansi("  (not exported)", "33", color))
        cat("\n")
      }
      if (isTRUE(res$truncated)) {
        cat(.flowr_ansi(sprintf("  ... truncated (searched %s); narrow with `package =`",
                                res$searched %||% "?"), "90", color), "\n", sep = "")
      }
    },
    "packages" = {
      n <- length(res$packages)
      cat(.flowr_ansi(sprintf("%d package%s", n, if (n == 1) "" else "s"), "1", color), "\n", sep = "")
      for (p in res$packages) {
        kind <- if (isTRUE(p$base)) "base" else if (isTRUE(p$cran)) "CRAN" else "?"
        cat(sprintf("  %-20s %-5s %s\n", p$name, kind, p$latest %||% ""))
      }
    },
    "not_found" = {
      cat(.flowr_ansi(res$message %||% "not found", "31", color), "\n", sep = "")
      if (length(res$suggestions) > 0) {
        cat("  did you mean: ", paste(unlist(res$suggestions), collapse = ", "), "?\n", sep = "")
      }
    },
    {
      cat(.flowr_ansi(sprintf("signature database: %d package%s across %d source%s",
                              res$packageCount, if (identical(res$packageCount, 1L)) "" else "s",
                              res$sourceCount, if (identical(res$sourceCount, 1L)) "" else "s"),
                      "1", color), "\n", sep = "")
      for (d in res$databases %||% list()) {
        cat(sprintf("  %-10s v%s  (%s)\n", d$scope, d$version, d$date))
      }
    }
  )
  invisible(x)
}
