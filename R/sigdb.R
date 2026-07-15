# Signature database ----------------------------------------------------------
#
# flowR ships only a pointer to its signature database, so with nothing mounted
# it reports "no sigdb" and cannot resolve library()/:: exports.
#
# The database is data, not code: it serves every platform and engine alike, so
# it is published beside the binaries and obtained/updated independently of them.
# It comes as three non-redundant sets that mount together, so `flowr.sigdb`
# selects what to download (see ?flowr_install). They share one directory
# per version -- filenames are set-prefixed, so they never collide -- and which
# are installed is read back from the manifests present, so the directory
# describes itself rather than trusting a separate record.
#
# Mounting needs no flowR config: it searches $FLOWR_SIGDB_DIR (.flowr_with_sigdb).

# The sets, smallest first; also the order they are installed in.
.flowr_sigdb_all_scopes <- c("base", "current", "history")

# Normalise a `flowr.sigdb` value to a set of scopes. Accepts a character vector,
# a comma/space-separated string (so FLOWR_SIGDB="current,history" works), FALSE
# or "none" for nothing, and "all" for everything.
.flowr_sigdb_scopes <- function(x = flowr_option("sigdb")) {
  if (is.null(x) || isFALSE(x) || length(x) == 0L) {
    return(character(0))
  }
  if (isTRUE(x)) {
    return("current")
  }
  x <- trimws(unlist(strsplit(as.character(x), "[,[:space:]]+")))
  x <- x[nzchar(x)]
  if (length(x) == 0L || any(x %in% c("none", "off"))) {
    return(character(0))
  }
  if (any(x == "all")) {
    return(.flowr_sigdb_all_scopes)
  }
  bad <- setdiff(x, .flowr_sigdb_all_scopes)
  if (length(bad) > 0L) {
    .flowr_stop("unknown signature-database set(s): ", paste(bad, collapse = ", "),
         "\n  pick from: ", paste(.flowr_sigdb_all_scopes, collapse = ", "),
         ", or \"all\" / \"none\"")
  }
  # keep the canonical order, drop duplicates
  .flowr_sigdb_all_scopes[.flowr_sigdb_all_scopes %in% x]
}

# One directory per flowR version; the sets live side by side inside it.
.flowr_sigdb_dir <- function(version = flowr_option("flowr_version")) {
  file.path(.flowr_cache_dir(), paste0("sigdb-", version))
}

# Which sets are installed, read back from the manifests actually present.
.flowr_sigdb_scopes_installed <- function(version = flowr_option("flowr_version")) {
  d <- .flowr_sigdb_dir(version)
  if (!dir.exists(d)) {
    return(character(0))
  }
  present <- vapply(.flowr_sigdb_all_scopes, function(s) {
    length(Sys.glob(file.path(d, paste0(s, ".manifest.json*")))) > 0L
  }, logical(1))
  .flowr_sigdb_all_scopes[present]
}

.flowr_sigdb_installed <- function(version = flowr_option("flowr_version")) {
  length(.flowr_sigdb_scopes_installed(version)) > 0L
}

.flowr_sigdb_manifest_entry <- function(version, scope, manifest = .flowr_manifest()) {
  for (s in manifest$sigdb %||% list()) {
    if (identical(s$version, version) && identical(s$scope, scope)) {
      return(s)
    }
  }
  NULL
}

# Where to fetch a set: the shipped manifest (pinned) first, else the release
# naming convention with a checksum sidecar, so a new flowR version works without
# updating this package. Mirrors .flowr_binary_source(), minus the platform: one
# archive serves all.
.flowr_sigdb_source <- function(version, scope) {
  entry <- .flowr_sigdb_manifest_entry(version, scope)
  if (!is.null(entry)) {
    return(list(url = entry$url, sha256 = entry$sha256,
                sig = entry$sig %||% paste0(entry$url, ".sig")))
  }
  repo <- flowr_option("binary_repo")
  url <- sprintf("https://github.com/%s/releases/download/flowr-v%s/flowr-sigdb-%s-%s.tar.gz",
                 repo, version, scope, version)
  # probe the checksum sidecar; a 404 just means this set is not published for
  # this version yet, so swallow the download warning
  sha <- tryCatch(suppressWarnings({
    tmp <- tempfile()
    on.exit(unlink(tmp), add = TRUE)
    utils::download.file(paste0(url, ".sha256"), tmp, quiet = TRUE, mode = "wb")
    trimws(sub("\\s.*$", "", readLines(tmp, warn = FALSE)[1]))
  }), error = function(e) NULL)
  list(url = url, sha256 = sha, sig = paste0(url, ".sig"))
}

# Download + verify + unpack one set into the shared directory. Verification is
# the binaries' path exactly: a mandatory checksum, plus a signature against the
# pinned key that secure mode makes non-optional. A tampered database would
# mis-resolve every library() call, so it is held to the same bar as an executable.
.flowr_install_sigdb_scope <- function(version, scope, quiet) {
  src <- .flowr_sigdb_source(version, scope)
  if (is.null(src$sha256) && isTRUE(flowr_option("secure"))) {
    .flowr_stop("no verifiable \"", scope, "\" signature database is available for flowR ",
         version, " (it may not be published yet).\n",
         "flowR analyses without a database, it just cannot resolve library() ",
         "exports. Set options(flowr.sigdb = \"none\") to stop trying, or ",
         "options(flowr.secure = FALSE) to allow an unverified download.")
  }
  dir <- .flowr_sigdb_dir(version)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  archive <- file.path(dir, basename(src$url))
  on.exit(unlink(archive), add = TRUE)
  if (!quiet) {
    message("[flowr] downloading the \"", scope, "\" signature database for flowR ", version)
    message("[flowr]   from ", src$url)
  }
  .flowr_download_verify(
    src$url, src$sha256, archive, quiet = quiet,
    on_missing = paste0(
      "could not download the \"", scope, "\" signature database from\n  ", src$url,
      "\n(it may not be published for flowR ", version, " yet). flowR still ",
      "analyses without it; it just cannot resolve library() exports.")
  )
  if (isTRUE(flowr_option("verify_signature")) || isTRUE(flowr_option("secure"))) {
    sig <- .flowr_verify_signature(archive, src$sig, what = "flowR signature database")
  } else {
    sig <- NA
  }
  # drop this set's old files before unpacking, so a re-install replaces it
  # without leaving shards of a previous build alongside the new ones
  unlink(Sys.glob(file.path(dir, paste0(scope, ".*"))))
  utils::untar(archive, exdir = dir)
  if (!scope %in% .flowr_sigdb_scopes_installed(version)) {
    unlink(Sys.glob(file.path(dir, paste0(scope, ".*"))))
    .flowr_stop("the downloaded \"", scope, "\" signature database contains no ",
         "manifest; it is not usable and has been discarded.")
  }
  level <- if (isTRUE(sig)) "signature" else if (!is.null(src$sha256)) "checksum" else "none"
  writeLines(level, file.path(dir, paste0(scope, ".VERIFICATION")))
  invisible(level)
}

# How strongly the installed sets were verified: the weakest of them, since that
# is the guarantee the mounted database as a whole carries.
.flowr_sigdb_verification <- function(version = flowr_option("flowr_version")) {
  scopes <- .flowr_sigdb_scopes_installed(version)
  if (length(scopes) == 0L) {
    return(NA_character_)
  }
  lv <- vapply(scopes, function(s) {
    f <- file.path(.flowr_sigdb_dir(version), paste0(s, ".VERIFICATION"))
    if (file.exists(f)) trimws(readLines(f, warn = FALSE)[1]) else "none"
  }, character(1))
  rank <- c(none = 1L, checksum = 2L, signature = 3L)
  names(which.min(rank[lv]))[1] %||% "none"
}

# The directory to mount, or NULL when nothing is installed. Never downloads.
.flowr_sigdb_mount <- function(version = flowr_option("flowr_version")) {
  if (.flowr_sigdb_installed(version)) .flowr_sigdb_dir(version) else NULL
}

# Run `expr` with $FLOWR_SIGDB_DIR pointing at the cached database, so a spawned
# flowR mounts it. sys::exec_background() takes no env, so the child inherits
# ours; the variable is restored afterwards to keep this session's environment
# unchanged. An FLOWR_SIGDB_DIR the user set themselves is left strictly alone:
# it is an explicit choice, and flowR searches whatever roots it names.
.flowr_with_sigdb <- function(expr, version = flowr_option("flowr_version")) {
  dir <- .flowr_sigdb_mount(version)
  if (!is.null(dir) && !nzchar(Sys.getenv("FLOWR_SIGDB_DIR"))) {
    Sys.setenv(FLOWR_SIGDB_DIR = dir)
    on.exit(Sys.unsetenv("FLOWR_SIGDB_DIR"), add = TRUE)
    .flowr_log("mounting signature database: ", dir)
  }
  force(expr)
}

# base::interactive() and base::readline() behind package bindings. A sealed
# namespace (any installed package, so `R CMD check`) cannot have base functions
# mocked out from under it, so the prompt could otherwise only be exercised by
# hand -- which is how a prompt that blocks a script gets shipped.
.flowr_interactive <- function() interactive()
.flowr_readline <- function(prompt) readline(prompt)

# Ask which sets to fetch. The choice spans ~35 MB, so when nobody has said what
# they want, asking beats silently guessing in either direction. Interactive
# only: an unattended session (CRAN, CI, a script) must never block on a prompt,
# so it takes the default.
.flowr_ask_sigdb <- function(default) {
  if (!.flowr_interactive()) {
    return(default)
  }
  message("flowR resolves library()/:: exports from a signature database:\n",
          "  1: current  ~24 MB  base R + every current CRAN package (default)\n",
          "  2: all      ~59 MB  ... and every past package version\n",
          "  3: none             skip it; flowR still analyses code, it just\n",
          "                      leaves package exports unresolved")
  ans <- tolower(trimws(.flowr_readline("Download which? [1/2/3, default 1] ")))
  if (!nzchar(ans)) {
    ans <- "1"                                  # a bare Enter takes the default
  }
  pick <- switch(ans,
    "1" = , "current" = "current",
    "2" = , "all" = "all",
    "3" = , "none" = "none",
    NULL)
  if (is.null(pick)) {
    message("[flowr] \"", ans, "\" not understood; taking the default.")
    return(default)
  }
  .flowr_sigdb_scopes(pick)
}

# Which sets to obtain: an explicit argument, or a configured `flowr.sigdb`,
# settles it outright. Only when neither said anything -- and there is actually
# something to download -- do we ask.
.flowr_resolve_sigdb <- function(sigdb, ask, version, force = FALSE) {
  scopes <- .flowr_sigdb_scopes(sigdb)          # validate before anything else
  if (!ask) {
    return(scopes)
  }
  if (!force && all(scopes %in% .flowr_sigdb_scopes_installed(version))) {
    return(scopes)                              # nothing to fetch, nothing to ask
  }
  .flowr_ask_sigdb(scopes)
}

# Obtain the sets `flowr.sigdb` asks for, skipping those already present (unless
# `force` re-fetches them). Used by flowr_install() so an engine arrives ready to
# resolve exports; failures are reported but never fatal, since an engine without
# a database still analyses.
.flowr_ensure_sigdb <- function(version, quiet, scopes = .flowr_sigdb_scopes(),
                                force = FALSE) {
  have <- .flowr_sigdb_scopes_installed(version)
  want <- if (force) scopes else setdiff(scopes, have)
  # Say where the database stands even when there is nothing to do. The engine
  # reports itself ("already installed"), so a silent database reads as one that
  # was never considered -- the user cannot tell "present" from "forgotten".
  if (length(want) == 0L) {
    if (!quiet) {
      if (length(scopes) == 0L) {
        message("[flowr] no signature database requested (flowr.sigdb = \"none\"); ",
                "library() exports stay unresolved.")
      } else {
        message("[flowr] signature database (", paste(have, collapse = ", "),
                ") already installed in ", .flowr_sigdb_dir(version),
                " (use force = TRUE to re-download)")
      }
    }
    return(invisible(character(0)))
  }
  ok <- character(0)
  for (s in want) {
    done <- tryCatch({
      .flowr_install_sigdb_scope(version, s, quiet = quiet)
      TRUE
    }, error = function(e) {
      .flowr_warn("could not install the \"", s, "\" signature database: ",
                  conditionMessage(e),
                  "\n  flowR will run without it (library() exports stay unresolved).")
      FALSE
    })
    if (isTRUE(done)) ok <- c(ok, s)
  }
  if (!quiet && length(ok) > 0L) {
    message("[flowr] signature database ready (",
            paste(.flowr_sigdb_scopes_installed(version), collapse = ", "), ", ",
            .flowr_sigdb_verification(version), "-verified).")
  }
  invisible(ok)
}
