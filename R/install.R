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
#' @param version flowR version to install (default `"2.11.1"`).
#' @param engine `"binary"` (a self-contained executable, no Node/Docker needed),
#'   `"node"` (the flowR npm package run with a system or private Node.js) or
#'   `"docker"` (pull the flowR docker image).
#' @param quiet Suppress progress messages.
#' @param force Reinstall even if already present.
#' @return `TRUE`, invisibly.
#' @export
#' @examples
#' \dontrun{
#' flowr_install()                    # default binary engine, flowR 2.11.1
#' flowr_install(engine = "node")     # Node engine
#' flowr_install(engine = "docker")   # pull the flowR docker image
#' }
flowr_install <- function(version = flowr_option("flowr_version"),
                          engine = c("binary", "node", "docker"),
                          quiet = FALSE, force = FALSE) {
  engine <- match.arg(engine)
  if (engine == "binary") {
    if (force) {
      unlink(.flowr_binary_dir(version, .flowr_platform()$key), recursive = TRUE)
    }
    if (!.flowr_binary_installed(version)) {
      .flowr_install_binary(version, quiet = quiet)
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
  invisible(TRUE)
}

#' Is a flowR engine installed?
#'
#' Reports engine readiness from the engine registry, so it stays correct as
#' engines are added. Use `"all"` to see every engine at once.
#'
#' @param engine One engine (`"binary"`, `"bundled"`, `"node"`, `"docker"`),
#'   `"any"` (is at least one ready?), or `"all"` (a named logical for every
#'   engine).
#' @param version flowR version to check.
#' @return For a single engine or `"any"`, a single `TRUE`/`FALSE`. For `"all"`,
#'   a named logical vector, one entry per registered engine.
#' @export
#' @examples
#' flowr_installed("binary")
#' flowr_installed("all")
flowr_installed <- function(engine = c("any", "all", "binary", "bundled", "node", "docker"),
                            version = flowr_option("flowr_version")) {
  engine <- match.arg(engine)
  specs <- .flowr_engine_specs()
  ready <- function(nm) isTRUE(tryCatch(specs[[nm]]$ready(version), error = function(e) FALSE))
  if (engine == "all") {
    return(vapply(stats::setNames(names(specs), names(specs)), ready, logical(1)))
  }
  if (engine == "any") {
    return(any(vapply(names(specs), ready, logical(1))))
  }
  ready(engine)
}

# The latest published flowR version, from the npm registry; NULL if it cannot
# be determined (e.g. offline). Used by flowr_update() to check for updates.
.flowr_latest_version <- function() {
  url <- "https://registry.npmjs.org/@eagleoutice/flowr/latest"
  tryCatch({
    tmp <- tempfile(fileext = ".json")
    on.exit(unlink(tmp), add = TRUE)
    utils::download.file(url, tmp, quiet = TRUE, mode = "wb")
    v <- jsonlite::fromJSON(tmp)$version
    if (is.character(v) && length(v) == 1L && nzchar(v)) v else NULL
  }, error = function(e) NULL)
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
    stop("`version` must be a single flowR version string, e.g. \"2.12.0\"",
         call. = FALSE)
  }
  flowr_disconnect()                       # drop any session on the old version
  flowr_install(version = version, engine = engine, quiet = quiet)
  # target this version + engine for subsequent sessions (bundled is pinned, so
  # auto is not used here)
  options(flowr.flowr_version = version, flowr.engine = engine)
  message("[flowr] now targeting flowR ", version, " via the ", engine, " engine.")
  invisible(version)
}

#' Remove cached flowR engines
#'
#' Removes downloaded binary/node engines from the cache. The docker image is
#' left in place unless `docker = TRUE` (removing it needs the docker daemon).
#' The `bundled` engine cannot be removed: flowR's JS+wasm bundle ships inside
#' the package, so `flowr_installed()` can stay `TRUE` after uninstalling.
#'
#' @param version A version to remove, or `NULL` to remove the whole cache.
#' @param docker Also remove the flowR docker image for `version` (best effort).
#' @param quiet Suppress the summary message.
#' @return `TRUE`, invisibly.
#' @export
flowr_uninstall <- function(version = NULL, docker = FALSE, quiet = FALSE) {
  if (is.null(version)) {
    unlink(.flowr_cache_dir(), recursive = TRUE)
  } else {
    unlink(.flowr_binary_dir(version, .flowr_platform()$key), recursive = TRUE)
    unlink(.flowr_node_dir(version), recursive = TRUE)
    if (isTRUE(docker) && .flowr_docker_installed(version)) {
      tryCatch(sys::exec_wait("docker", c("rmi", .flowr_docker_image(version)),
                              std_out = FALSE, std_err = FALSE),
               error = function(e) NULL)
    }
  }
  if (!quiet && !isTRUE(flowr_option("quiet"))) {
    msg <- "[flowr] removed downloaded engines from the cache."
    if (.flowr_bundled_available()) {
      msg <- paste0(msg, " The shipped bundle (engine = \"bundled\", needs Node) ",
                    "remains available.")
    }
    message(msg)
  }
  invisible(TRUE)
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
    stop("flowR engine \"", engine, "\" (", version, ") is not installed.\n",
         "Run  flowr_install(engine = \"", engine, "\")  to download it.",
         call. = FALSE)
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
