# Installation ----------------------------------------------------------------
#
# flowR engines are downloaded on demand into a per-user cache. To comply with
# CRAN policy, nothing is ever downloaded without the user's action or consent:
# `flowr_install()` is an explicit request, and the on-demand path used by
# `flowr_connect()` asks for confirmation in interactive sessions and refuses
# (with instructions) otherwise.

#' Install a flowR engine into the user cache
#'
#' Downloads and caches a flowR engine so it can be started by [flowr_connect()].
#' The cache lives under `tools::R_user_dir("flowr", "cache")`. Downloaded
#' binaries are verified against the checksums in the package manifest before use.
#'
#' @section The signature database:
#' The engine arrives with flowR's signature database, so `library()` and `::`
#' exports resolve straight away; without it flowR still analyses code, it just
#' cannot resolve package exports. It is a separate, platform-independent
#' download shared by every engine, so it is skipped when already present, and a
#' failure to obtain it is a warning rather than an error.
#'
#' `sigdb` selects which sets to fetch. They are non-redundant and mount
#' together, so this picks what to download rather than choosing between
#' overlapping bundles:
#'
#' \describe{
#'   \item{`"base"`}{~1 MB. Base R only.}
#'   \item{`"current"`}{~24 MB. Base R plus every current CRAN package. The
#'     default, and enough for almost everything.}
#'   \item{`"history"`}{~35 MB. Past package versions. This *supplements*
#'     `"current"` (it holds no base-R signatures of its own), so add it
#'     alongside, not instead.}
#' }
#'
#' Also `"all"` and `"none"`. In an interactive session, if you pass no `sigdb`
#' and have not set the `flowr.sigdb` option, you are asked which to fetch ---
#' the sets differ by ~35 MB, so it is worth a question. Passing `sigdb`, or
#' setting the option (e.g. `options(flowr.sigdb = "none")`), settles it and is
#' never asked about again; unattended sessions never prompt and take
#' `"current"`.
#'
#' The engine and the database are obtained independently: an already-installed
#' engine is left alone, so `flowr_install(sigdb = "history")` just adds that set,
#' and `force = TRUE` re-fetches both. Remove the database with
#' [flowr_uninstall()].
#'
#' @param version flowR version to install (default `"2.12.3"`).
#' @param engine `"binary"` (a self-contained executable, no Node/Docker needed),
#'   `"node"` (the flowR npm package run with a system or private Node.js) or
#'   `"docker"` (pull the flowR docker image).
#' @param sigdb Signature-database sets to install with the engine: `"base"`,
#'   `"current"`, `"history"`, `"all"`, or `"none"`. Defaults to the
#'   `flowr.sigdb` option (`"current"`). See *The signature database*.
#' @param quiet Suppress progress messages.
#' @param force Reinstall even if already present.
#' @return `TRUE`, invisibly.
#' @seealso [flowr_uninstall()], [flowr_status()]
#' @export
#' @examples
#' \dontrun{
#' flowr_install()                    # binary engine + signature database
#' flowr_install(sigdb = "none")      # engine only
#' flowr_install(sigdb = "all")       # ... with every database set
#' flowr_install(sigdb = "history")   # add a set to an installed engine
#' flowr_install(engine = "docker")   # pull the flowR docker image
#' }
flowr_install <- function(version = flowr_option("flowr_version"),
                          engine = c("binary", "node", "docker"),
                          sigdb = flowr_option("sigdb"),
                          quiet = FALSE, force = FALSE) {
  engine <- match.arg(engine)
  # Ask which database sets to fetch only when nobody has said: an explicit
  # `sigdb`, or a configured `flowr.sigdb`, is taken as given. Validates (and so
  # rejects a bad set) before anything is downloaded.
  scopes <- .flowr_resolve_sigdb(
    sigdb, ask = missing(sigdb) && !.flowr_option_is_set("sigdb"),
    version = version, force = force
  )
  if (engine == "binary") {
    if (force) {
      unlink(.flowr_binary_dir(version, .flowr_platform()$key), recursive = TRUE)
    }
    if (!.flowr_binary_installed(version)) {
      .flowr_install_binary(version, quiet = quiet)
    } else if (!quiet) {
      message("[flowr] flowR ", version, " binary already installed in ",
              .flowr_binary_dir(version, .flowr_platform()$key),
              " (use force = TRUE to reinstall)")
    }
  } else if (engine == "docker") {
    if (force || !.flowr_docker_installed(version)) {
      .flowr_install_docker(version, quiet = quiet)
    }
  } else {
    if (force) {
      unlink(.flowr_node_dir(version), recursive = TRUE)
    }
    if (!.flowr_node_installed(version)) {
      .flowr_install_node_flowr(version, quiet = quiet)
    }
  }
  # The database is engine-independent, so every engine gets it; `force` re-fetches
  # it too, since it is part of what was just (re)installed.
  .flowr_ensure_sigdb(version, quiet = quiet, scopes = scopes, force = force)
  invisible(TRUE)
}

#' Is a flowR engine installed?
#'
#' Reports engine readiness from the engine registry, so it stays correct as
#' engines are added. Use `"all"` to see every engine at once.
#'
#' Note that `flowr_is_installed("any")` (the default) is `TRUE` whenever *any*
#' engine is ready, including the `bundled` flowR that ships inside the package
#' (which only needs Node.js on your PATH). So it can report `TRUE` even when no
#' self-contained binary has been downloaded; in that bundled-only case it also
#' emits a one-line note pointing at [flowr_install()]. Ask specifically with
#' `flowr_is_installed("binary")` if you mean the downloaded binary.
#'
#' @param engine One engine (`"binary"`, `"bundled"`, `"node"`, `"docker"`),
#'   `"any"` (is at least one ready?), or `"all"` (a named logical for every
#'   engine).
#' @param version flowR version to check.
#' @return For a single engine or `"any"`, a single `TRUE`/`FALSE`. For `"all"`,
#'   a named logical vector, one entry per registered engine.
#' @seealso [flowr_install()], [flowr_status()]
#' @export
#' @examples
#' flowr_is_installed("binary")   # is the downloaded binary ready?
#' flowr_is_installed("all")      # readiness of every engine
#' flowr_is_installed()           # is any engine ready at all?
flowr_is_installed <- function(engine = c("any", "all", "binary", "bundled", "node", "docker"),
                               version = flowr_option("flowr_version")) {
  engine <- match.arg(engine)
  specs <- .flowr_engine_specs()
  ready <- function(nm) isTRUE(tryCatch(specs[[nm]]$ready(version), error = function(e) FALSE))
  if (engine == "all") {
    return(vapply(stats::setNames(names(specs), names(specs)), ready, logical(1)))
  }
  if (engine == "any") {
    vals <- vapply(stats::setNames(names(specs), names(specs)), ready, logical(1))
    # Note when the only thing making this TRUE is the shipped bundle (which
    # needs Node.js): the answer is "yes", but there is no self-contained binary.
    others <- setdiff(names(vals), "bundled")
    if (isTRUE(vals[["bundled"]]) && !any(vals[others])) {
      .flowr_notify(
        "installed-bundled-only",
        "flowr_is_installed(): ready via the bundled engine only (needs Node.js). ",
        "No downloaded binary is installed; run flowr_install() for one."
      )
    }
    return(any(vals))
  }
  ready(engine)
}

# Version checks --------------------------------------------------------------
#
# Knowing about a newer release is a nicety, never a requirement, so everything
# here fails soft: offline, DNS failure, a rate-limited API or a garbage reply
# all collapse to NULL. Nothing signals, nothing warns, and a short timeout caps
# the wait -- R's global `timeout` defaults to 60s, which would otherwise stall a
# session on a black-holed connection. NULL means "no idea", and the caller then
# says nothing at all.

# Fetch and parse JSON, or NULL.
.flowr_fetch_json <- function(url, timeout = 3) {
  tryCatch({
    old <- getOption("timeout")
    on.exit(options(timeout = old), add = TRUE)
    options(timeout = timeout)
    tmp <- tempfile(fileext = ".json")
    on.exit(unlink(tmp), add = TRUE)
    suppressWarnings(utils::download.file(url, tmp, quiet = TRUE, mode = "wb"))
    if (!file.exists(tmp) || !isTRUE(file.info(tmp)$size > 0)) {
      return(NULL)
    }
    jsonlite::fromJSON(tmp)
  }, error = function(e) NULL)
}

# A single version string out of a reply, or NULL when it is not one.
.flowr_one_version <- function(x) {
  if (is.character(x) && length(x) == 1L && nzchar(x)) sub("^v", "", x) else NULL
}

# The latest published flowR version, from the npm registry; NULL if it cannot
# be determined (e.g. offline). Used by flowr_update() to check for updates.
.flowr_latest_version <- function() {
  .flowr_one_version(
    .flowr_fetch_json("https://registry.npmjs.org/@eagleoutice/flowr/latest")$version)
}

# The latest published version of this package, from its GitHub releases. The
# engine-binary releases are deliberately not marked "latest" (see the binaries
# workflow), so this finds the adapter's own `v<version>` release.
.flowr_latest_adapter_version <- function() {
  url <- sprintf("https://api.github.com/repos/%s/releases/latest",
                 flowr_option("binary_repo"))
  .flowr_one_version(.flowr_fetch_json(url)$tag_name)
}

# What is out of date, as `name = c(have, latest)`; empty when everything is
# current OR nothing could be checked -- the two are deliberately indistinguish-
# able to callers, so being offline reads as "nothing to say" rather than an
# error. Asked at most once per session (the answer cannot meaningfully change
# within one), and not at all when `flowr.check_updates` is off, so an offline or
# air-gapped user never pays for a lookup they did not ask for.
.flowr_updates <- function() {
  if (!isTRUE(flowr_option("check_updates"))) {
    return(list())
  }
  if (!is.null(.flowr_state$updates)) {
    return(.flowr_state$updates)
  }
  out <- tryCatch({
    res <- list()
    newer <- function(have, latest) {
      !is.null(latest) && isTRUE(tryCatch(utils::compareVersion(latest, have) > 0,
                                          error = function(e) FALSE))
    }
    have_flowr <- flowr_option("flowr_version")
    new_flowr <- .flowr_latest_version()
    if (newer(have_flowr, new_flowr)) {
      res[["flowR"]] <- c(have_flowr, new_flowr)
    }
    have_pkg <- as.character(utils::packageVersion("flowr"))
    new_pkg <- .flowr_latest_adapter_version()
    if (newer(have_pkg, new_pkg)) {
      res[["flowr"]] <- c(have_pkg, new_pkg)
    }
    res
  }, error = function(e) list())
  .flowr_state$updates <- out
  out
}

#' Update flowR, or check for a newer version
#'
#' With no `version`, checks the flowR release feed and reports whether a newer
#' version than the one you currently target is available (it does not download
#' anything). With an explicit `version`, switches to that release for
#' subsequent sessions and installs its engine. The shipped `bundled` engine is
#' pinned to the packaged flowR, so installs use the downloadable `binary`
#' (default) or the `node` engine.
#'
#' @param version flowR version to switch to, e.g. `"2.12.0"`. `NULL` (default)
#'   only checks for a newer version.
#' @param engine Engine to install and use for it: `"binary"`, `"node"` or
#'   `"docker"`.
#' @param quiet Suppress progress messages.
#' @return The version string: the latest available when only checking, or the
#'   newly targeted version when switching. Returned invisibly.
#' @seealso [flowr_install()], [flowr_status()]
#' @export
#' @examples
#' \dontrun{
#' flowr_update()                   # is there a newer flowR?
#' flowr_update("2.12.0")           # switch to a newer flowR
#' flowr_update("2.10.0", "node")  # or an older one, via Node
#' }
flowr_update <- function(version = NULL, engine = c("binary", "node", "docker"),
                         quiet = FALSE) {
  engine <- match.arg(engine)
  current <- flowr_option("flowr_version")
  if (is.null(version)) {
    latest <- .flowr_latest_version()
    if (is.null(latest)) {
      message("[flowr] could not determine the latest flowR version (offline?). ",
              "You are targeting ", current, ".")
      return(invisible(current))
    }
    if (utils::compareVersion(latest, current) > 0) {
      message("[flowr] a newer flowR is available: ", current, " -> ", latest,
              ".\n  Install it with  flowr_update(\"", latest, "\").")
    } else {
      message("[flowr] flowR ", current, " is up to date (latest published: ",
              latest, ").")
    }
    return(invisible(latest))
  }
  if (!is.character(version) || length(version) != 1L || !nzchar(version)) {
    .flowr_stop("`version` must be a single flowR version string, e.g. \"2.12.0\"")
  }
  flowr_disconnect()                       # drop any session on the old version
  flowr_install(version = version, engine = engine, quiet = quiet)
  # target this version + engine for subsequent sessions (bundled is pinned, so
  # auto is not used here)
  options(flowr.flowr_version = version, flowr.engine = engine)
  message("[flowr] now targeting flowR ", version, " via the ", engine, " engine.")
  invisible(version)
}

# Normalise an `engine` selection for flowr_uninstall(): "all"/"none" or any of
# the removable engines. `bundled` ships inside the package and docker has its
# own flag, so neither is selectable here.
.flowr_removable_engines <- c("binary", "node")
.flowr_uninstall_engines <- function(x) {
  if (is.null(x) || isFALSE(x) || length(x) == 0L) {
    return(character(0))
  }
  x <- trimws(unlist(strsplit(as.character(x), "[,[:space:]]+")))
  x <- x[nzchar(x)]
  if (length(x) == 0L || any(x %in% c("none", "off"))) {
    return(character(0))
  }
  if (any(x == "all")) {
    return(.flowr_removable_engines)
  }
  bad <- setdiff(x, .flowr_removable_engines)
  if (length(bad) > 0L) {
    .flowr_stop("cannot uninstall engine(s): ", paste(bad, collapse = ", "),
         "\n  pick from: ", paste(.flowr_removable_engines, collapse = ", "),
         ", or \"all\" / \"none\"",
         "\n  (the bundled engine ships in the package; use docker = TRUE for the image)")
  }
  .flowr_removable_engines[.flowr_removable_engines %in% x]
}

#' Remove cached flowR engines and signature databases
#'
#' Removes downloaded engines and the signature database from the cache. By
#' default everything goes; `engine` and `sigdb` select what to remove, mirroring
#' [flowr_install()], so you can drop one piece and keep the rest.
#'
#' The docker image is left in place unless `docker = TRUE` (removing it needs
#' the docker daemon). The `bundled` engine cannot be removed: flowR's JS+wasm
#' bundle ships inside the package, so `flowr_is_installed()` can stay `TRUE`
#' after uninstalling.
#'
#' @param version A version to remove, or `NULL` for every cached version.
#' @param engine Engines to remove: `"all"` (default), `"none"`, `"binary"` or
#'   `"node"` (or a vector of them).
#' @param sigdb Signature-database sets to remove: `"all"` (default), `"none"`,
#'   or any of `"base"`, `"current"`, `"history"`. See [flowr_install()].
#' @param docker Also remove the flowR docker image for `version` (best effort).
#' @param quiet Suppress the summary message.
#' @return `TRUE`, invisibly.
#' @seealso [flowr_install()]
#' @export
#' @examples
#' \dontrun{
#' flowr_uninstall()                              # engines + database
#' flowr_uninstall(engine = "none", sigdb = "history")  # just that database set
#' flowr_uninstall(sigdb = "none")                # engines only, keep the database
#' flowr_uninstall(docker = TRUE)                 # also remove the pulled image
#' }
flowr_uninstall <- function(version = NULL, engine = "all", sigdb = "all",
                            docker = FALSE, quiet = FALSE) {
  engines <- .flowr_uninstall_engines(engine)
  scopes <- .flowr_sigdb_scopes(sigdb)
  everything <- setequal(engines, .flowr_removable_engines) &&
    setequal(scopes, .flowr_sigdb_all_scopes)
  # whole cache in one go when nothing is being kept, so no stray file survives
  if (is.null(version) && everything) {
    unlink(.flowr_cache_dir(), recursive = TRUE)
  } else {
    versions <- if (is.null(version)) .flowr_cached_versions() else version
    for (v in versions) {
      if ("binary" %in% engines) {
        unlink(.flowr_binary_dir(v, .flowr_platform()$key), recursive = TRUE)
      }
      if ("node" %in% engines) {
        unlink(.flowr_node_dir(v), recursive = TRUE)
      }
      if (setequal(scopes, .flowr_sigdb_all_scopes)) {
        unlink(.flowr_sigdb_dir(v), recursive = TRUE)
      } else {
        # the sets share a directory but are file-prefixed, so one can go alone
        for (s in scopes) {
          unlink(Sys.glob(file.path(.flowr_sigdb_dir(v), paste0(s, ".*"))))
        }
      }
    }
  }
  if (isTRUE(docker) && !is.null(version) && .flowr_docker_installed(version)) {
    tryCatch(sys::exec_wait("docker", c("rmi", .flowr_docker_image(version)),
                            std_out = FALSE, std_err = FALSE),
             error = function(e) NULL)
  }
  if (!quiet && !isTRUE(flowr_option("quiet"))) {
    what <- c(if (length(engines) > 0L) paste0(paste(engines, collapse = " + "), " engine(s)"),
              if (length(scopes) > 0L) paste0("sigdb (", paste(scopes, collapse = ", "), ")"))
    msg <- if (length(what) == 0L) {
      "[flowr] nothing selected; the cache is unchanged."
    } else {
      paste0("[flowr] removed ", paste(what, collapse = " and "), " from the cache.")
    }
    if (length(engines) > 0L && .flowr_bundled_available()) {
      msg <- paste0(msg, " The shipped bundle (engine = \"bundled\", needs Node) ",
                    "remains available.")
    }
    message(msg)
  }
  invisible(TRUE)
}

# Every flowR version with something in the cache, from the directory names the
# installers create.
.flowr_cached_versions <- function() {
  dirs <- basename(Sys.glob(file.path(.flowr_cache_dir(), c("binary-*", "node-flowr-*", "sigdb-*"))))
  v <- sub("^sigdb-", "", dirs)
  v <- sub("^node-flowr-", "", v)
  # binary-<version>-<platform key>: drop the prefix, then the trailing key
  v <- sub("^binary-(.*)-[^-]+-[^-]+$", "\\1", v)
  sort(unique(v[nzchar(v)]))
}

# Ensure an engine is present, asking for consent before any download.
.flowr_ensure_engine <- function(engine, version, quiet) {
  installed <- switch(engine,
    binary = .flowr_binary_installed(version),
    node = .flowr_node_installed(version),
    FALSE
  )
  if (installed) {
    return(invisible(TRUE))
  }
  if (!.flowr_consent(engine, version)) {
    .flowr_stop("flowR engine \"", engine, "\" (", version, ") is not installed.\n",
         "Run  flowr_install(engine = \"", engine, "\")  to download it.")
  }
  flowr_install(version = version, engine = engine, quiet = quiet)
}

# TRUE if we may download: explicit opt-in via option/env, else an interactive
# yes/no prompt. Never downloads unattended (e.g. on CRAN / in CI).
.flowr_consent <- function(engine, version) {
  if (isTRUE(getOption("flowr.consent_download")) ||
      isTRUE(.flowr_coerce(Sys.getenv("FLOWR_CONSENT_DOWNLOAD", "false"), TRUE))) {
    return(TRUE)
  }
  if (!interactive()) {
    return(FALSE)
  }
  ans <- readline(sprintf(
    "flowR %s (%s engine) is not installed. Download it now? [y/N] ", version, engine))
  tolower(trimws(ans)) %in% c("y", "yes")
}
