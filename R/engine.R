# Engines ---------------------------------------------------------------------
#
# An "engine" is a way to obtain a *running* flowR server that the client then
# talks to over the socket protocol.  All engines expose the same handle shape
# and are interchangeable; the resolver picks one and the session opens a socket
# to it.  This is the seam that makes the underlying flowR runtime selectable.
#
#   bundled the JS+wasm bundle shipped in the package, run on the user's Node
#   binary  self-contained downloaded executable (no Node/Docker for the user)
#   node    a (system or private) Node.js running the flowR npm package
#   docker  `docker run eagleoutice/flowr --server`

# Platform --------------------------------------------------------------------

.flowr_platform <- function() {
  sysname <- Sys.info()[["sysname"]]
  os <- switch(sysname,
    Windows = "win", Linux = "linux", Darwin = "darwin",
    stop("unsupported operating system: ", sysname, call. = FALSE)
  )
  machine <- Sys.info()[["machine"]]
  arch <- switch(machine,
    "x86-64" = , "x86_64" = , "amd64" = "x64",
    "arm64"  = , "aarch64" = "arm64",
    stop("unsupported architecture: ", machine, call. = FALSE)
  )
  list(os = os, arch = arch, key = paste0(os, "-", arch),
       exe = if (os == "win") ".exe" else "")
}

# Persistent cache directory (CRAN-sanctioned; never the package install dir).
.flowr_cache_dir <- function() {
  d <- tools::R_user_dir("flowr", "cache")
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}

# Binary manifest -------------------------------------------------------------

# The manifest maps (version, platform) -> download url + sha256 so that the
# downloaded executable can be integrity-checked before it is ever run.  A copy
# ships with the package (`inst/flowr-manifest.json`); it can be refreshed from
# the release so that new flowR versions work without updating this package.
.flowr_manifest <- function() {
  path <- system.file("flowr-manifest.json", package = "flowr")
  if (!nzchar(path) || !file.exists(path)) {
    return(list(binaries = list()))
  }
  jsonlite::fromJSON(path, simplifyVector = FALSE)
}

.flowr_manifest_entry <- function(version, key, manifest = .flowr_manifest()) {
  for (b in manifest$binaries) {
    if (identical(b$version, version) && identical(b$platform, key)) {
      return(b)
    }
  }
  NULL
}

# Download + integrity-check + extract --------------------------------------

.flowr_download_verify <- function(url, sha256, dest) {
  ok <- tryCatch({
    suppressWarnings(utils::download.file(url, dest, mode = "wb",
                                          quiet = flowr_option("quiet")))
    file.exists(dest) && file.info(dest)$size > 0
  }, error = function(e) FALSE)
  if (!isTRUE(ok)) {
    unlink(dest)
    stop("could not download the flowR binary from\n  ", url,
         "\n(the prebuilt binary for this platform/version may not be published ",
         "yet). Use flowr_connect(engine = \"bundled\") or ",
         "flowr_install(engine = \"node\") instead.", call. = FALSE)
  }
  if (!is.null(sha256)) {
    got <- .flowr_sha256(dest)
    if (!identical(tolower(got), tolower(sha256))) {
      unlink(dest)
      stop("checksum mismatch for ", url, "\n  expected ", sha256,
           "\n  got      ", got, call. = FALSE)
    }
  }
  invisible(dest)
}

# SHA-256 of a file. Uses the (Suggested) digest package; required only when
# downloading a binary, so the core package stays base-R + jsonlite + sys.
.flowr_sha256 <- function(path) {
  if (!requireNamespace("digest", quietly = TRUE)) {
    stop("verifying the flowR binary download requires the 'digest' package.\n",
         "Install it with install.packages(\"digest\"), or use the Node engine.",
         call. = FALSE)
  }
  tolower(digest::digest(file = path, algo = "sha256"))
}

# Binary engine ---------------------------------------------------------------

.flowr_binary_dir <- function(version, key) {
  file.path(.flowr_cache_dir(), paste0("binary-", version, "-", key))
}

.flowr_binary_installed <- function(version = flowr_option("flowr_version"),
                                    key = .flowr_platform()$key) {
  dir <- .flowr_binary_dir(version, key)
  exe <- file.path(dir, paste0("flowr", .flowr_platform()$exe))
  file.exists(exe)
}

# When the installed binary was written (its mtime), or NA when not installed.
.flowr_binary_mtime <- function(version = flowr_option("flowr_version"),
                                key = .flowr_platform()$key) {
  exe <- file.path(.flowr_binary_dir(version, key), paste0("flowr", .flowr_platform()$exe))
  if (!file.exists(exe)) {
    return(NA)
  }
  file.info(exe)$mtime
}

# How strongly the installed binary was verified: "signature" (matched the
# pinned key), "checksum" (SHA-256 only, no signature), "none" (neither), or
# NA when it is not installed / predates this record.
.flowr_binary_verification <- function(version = flowr_option("flowr_version"),
                                       key = .flowr_platform()$key) {
  if (!.flowr_binary_installed(version, key)) {
    return(NA_character_)
  }
  f <- file.path(.flowr_binary_dir(version, key), "VERIFICATION")
  if (!file.exists(f)) {
    return("none")
  }
  trimws(readLines(f, warn = FALSE)[1])
}

# The recorded SHA-256 of the installed binary's archive, or NA when unknown.
.flowr_binary_hash <- function(version = flowr_option("flowr_version"),
                               key = .flowr_platform()$key) {
  f <- file.path(.flowr_binary_dir(version, key), "SHA256")
  if (!.flowr_binary_installed(version, key) || !file.exists(f)) {
    return(NA_character_)
  }
  trimws(readLines(f, warn = FALSE)[1])
}

# Resolve where to fetch a binary from: the shipped manifest (strongest, pinned
# and verified), else the release naming convention with a checksum sidecar so
# new flowR versions work without updating this package.
.flowr_binary_source <- function(version, key) {
  entry <- .flowr_manifest_entry(version, key)
  if (!is.null(entry)) {
    return(list(url = entry$url, sha256 = entry$sha256,
                sig = entry$sig %||% paste0(entry$url, ".sig")))
  }
  repo <- flowr_option("binary_repo")
  url <- sprintf("https://github.com/%s/releases/download/flowr-v%s/flowr-%s-%s.tar.gz",
                 repo, version, version, key)
  # probe the checksum sidecar; a 404 just means no prebuilt binary is published
  # for this version/platform yet, so swallow the download warning
  sha <- tryCatch(suppressWarnings({
    tmp <- tempfile()
    utils::download.file(paste0(url, ".sha256"), tmp, quiet = TRUE, mode = "wb")
    trimws(sub("\\s.*$", "", readLines(tmp, warn = FALSE)[1]))
  }), error = function(e) NULL)
  list(url = url, sha256 = sha, sig = paste0(url, ".sig"))
}

# Verify a downloaded archive's signature against the public key pinned in the
# package (`inst/flowr-pubkey.pem`). Uses the openssl package, so no gpg
# installation is required and the check is deterministic. When no key is shipped
# or openssl is unavailable, the SHA-256 checksum remains the guarantee (in
# secure mode a missing verifier is an error, not a silent skip).
.flowr_verify_signature <- function(archive, sig_url) {
  pub <- system.file("flowr-pubkey.pem", package = "flowr")
  if (!nzchar(pub) || !file.exists(pub)) {
    return(invisible(NA))                      # no pinned key -> rely on SHA-256
  }
  if (is.null(sig_url) || !nzchar(sig_url)) {
    if (isTRUE(flowr_option("secure"))) {
      stop("secure mode: a pinned key is shipped but this download has no ",
           "signature URL.", call. = FALSE)
    }
    return(invisible(NA))
  }
  if (!requireNamespace("openssl", quietly = TRUE)) {
    if (isTRUE(flowr_option("secure"))) {
      stop("secure mode: verifying the binary signature needs the 'openssl' ",
           "package. install.packages(\"openssl\"), or set ",
           "options(flowr.verify_signature = FALSE).", call. = FALSE)
    }
    message("[flowr] 'openssl' not installed; skipping signature check (SHA-256 verified).")
    return(invisible(NA))
  }
  sig <- tempfile(fileext = ".sig")
  ok <- tryCatch({
    utils::download.file(sig_url, sig, mode = "wb", quiet = TRUE)
    file.exists(sig) && file.info(sig)$size > 0
  }, error = function(e) FALSE)
  if (!isTRUE(ok)) {
    if (isTRUE(flowr_option("secure"))) {
      stop("secure mode: no signature found at ", sig_url, call. = FALSE)
    }
    message("[flowr] no signature available; skipping (SHA-256 verified).")
    return(invisible(NA))
  }
  key <- openssl::read_pubkey(pub)
  sig_raw <- readBin(sig, "raw", n = file.info(sig)$size)
  data <- readBin(archive, "raw", n = file.info(archive)$size)
  valid <- tryCatch(
    openssl::signature_verify(data, sig_raw, hash = openssl::sha256, pubkey = key),
    error = function(e) FALSE
  )
  if (!isTRUE(valid)) {
    unlink(archive)
    stop("signature verification failed for the downloaded flowR binary ",
         "(it did not match the pinned key).", call. = FALSE)
  }
  invisible(TRUE)
}

.flowr_install_binary <- function(version, quiet = flowr_option("quiet")) {
  plat <- .flowr_platform()
  src <- .flowr_binary_source(version, plat$key)
  if (is.null(src$sha256) && isTRUE(flowr_option("secure"))) {
    stop("no verifiable flowR binary is available for ", plat$key, " / ", version,
         " (the prebuilt binary may not be published yet).\n",
         "You do not need it: use the shipped bundle instead - ",
         "flowr_connect(engine = \"bundled\") (needs Node, no download). ",
         "Or the Node engine (flowr_install(engine = \"node\")), ",
         "or set options(flowr.secure = FALSE) to allow an unverified binary.",
         call. = FALSE)
  }
  dir <- .flowr_binary_dir(version, plat$key)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  archive <- file.path(dir, basename(src$url))
  if (!quiet) message("Downloading flowR ", version, " (", plat$key, ") ...")
  .flowr_download_verify(src$url, src$sha256, archive)
  # the hash we verified; digest may be absent (Suggested), so record NA if so
  got <- tryCatch(.flowr_sha256(archive), error = function(e) NA_character_)
  # Signature verification is mandatory in secure mode whenever a public key is
  # pinned, so it cannot be silently turned off with verify_signature = FALSE.
  pinned <- nzchar(system.file("flowr-pubkey.pem", package = "flowr"))
  sig <- NA
  if (isTRUE(flowr_option("verify_signature")) || (isTRUE(flowr_option("secure")) && pinned)) {
    sig <- .flowr_verify_signature(archive, src$sig)   # TRUE = matched, NA = skipped
  }
  # record how strongly this binary was verified, and the hash, for flowr_status()
  level <- if (isTRUE(sig)) "signature" else if (!is.null(src$sha256)) "checksum" else "none"
  writeLines(level, file.path(dir, "VERIFICATION"))
  if (!is.na(got)) {
    writeLines(got, file.path(dir, "SHA256"))
  }
  if (grepl("\\.zip$", archive)) {
    utils::unzip(archive, exdir = dir)
  } else {
    utils::untar(archive, exdir = dir)
  }
  unlink(archive)
  exe <- file.path(dir, paste0("flowr", plat$exe))
  Sys.chmod(exe, "0755")
  if (plat$os == "darwin") {
    # best-effort: clear the quarantine flag so macOS Gatekeeper does not block
    # the freshly downloaded executable
    tryCatch(
      sys::exec_wait("xattr", c("-dr", "com.apple.quarantine", dir),
                     std_out = FALSE, std_err = FALSE),
      error = function(e) NULL
    )
  }
  dir
}

# Locate the two tree-sitter wasm files under a directory (order matters):
# returns list(r = <tree-sitter-r.wasm>, runtime = <tree-sitter.wasm>) or NULL.
.flowr_find_wasm <- function(root) {
  r <- list.files(root, pattern = "^tree-sitter-r\\.wasm$", recursive = TRUE, full.names = TRUE)
  rt <- list.files(root, pattern = "^tree-sitter\\.wasm$", recursive = TRUE, full.names = TRUE)
  if (length(r) == 0 || length(rt) == 0) {
    return(NULL)
  }
  list(r = r[[1]], runtime = rt[[1]])
}

# Node engine -----------------------------------------------------------------

.flowr_node_dir <- function(version) {
  file.path(.flowr_cache_dir(), paste0("node-flowr-", version))
}

# Find a usable `node` executable: a suitable one on PATH, else a private one.
.flowr_node_exe <- function() {
  sys_node <- Sys.which("node")
  if (nzchar(sys_node) && .flowr_node_ok(sys_node)) {
    return(unname(sys_node))
  }
  priv <- .flowr_private_node_exe()
  if (!is.null(priv) && file.exists(priv)) {
    return(priv)
  }
  NA_character_
}

# The "no standalone binary" notice, phrased by whether the bundled engine can
# actually run: the bundle needs Node.js >= 18, so if a usable Node is present we
# nudge toward a binary; if it is missing we tell the user that installing a
# binary is how to get a working flowR backend at all. Returns the coloured line.
.flowr_no_binary_msg <- function() {
  have_node <- !is.na(tryCatch(.flowr_node_exe(), error = function(e) NA_character_))
  msg <- if (have_node) {
    "flowr: using bundled engine with Node.js; run flowr_install() for a binary."
  } else {
    "flowr: no Node.js found; run flowr_install() to get the flowR backend."
  }
  .flowr_ansi(msg, "90", .flowr_use_color())
}

.flowr_node_ok <- function(node) {
  v <- tryCatch(sys::exec_internal(node, "--version"), error = function(e) NULL)
  if (is.null(v) || v$status != 0) {
    return(FALSE)
  }
  major <- suppressWarnings(as.integer(sub("^v(\\d+).*", "\\1", rawToChar(v$stdout))))
  isTRUE(major >= 18)
}

.flowr_private_node_exe <- function() {
  base <- file.path(.flowr_cache_dir(), "node")
  dirs <- list.dirs(base, recursive = FALSE)
  if (length(dirs) != 1) {
    return(NULL)
  }
  plat <- .flowr_platform()
  if (plat$os == "win") file.path(dirs[[1]], "node.exe") else file.path(dirs[[1]], "bin", "node")
}

.flowr_install_private_node <- function(node_ver = "22.13.1", quiet = flowr_option("quiet")) {
  plat <- .flowr_platform()
  base <- file.path(.flowr_cache_dir(), "node")
  unlink(base, recursive = TRUE)
  dir.create(base, recursive = TRUE, showWarnings = FALSE)
  ext <- if (plat$os == "win") "zip" else "tar.gz"
  name <- sprintf("node-v%s-%s-%s", node_ver, plat$os, plat$arch)
  url <- sprintf("https://nodejs.org/dist/v%s/%s.%s", node_ver, name, ext)
  archive <- file.path(base, paste0("node.", ext))
  if (!quiet) message("Downloading Node.js ", node_ver, " ...")
  utils::download.file(url, archive, mode = "wb", quiet = quiet)
  if (ext == "zip") utils::unzip(archive, exdir = base) else utils::untar(archive, exdir = base)
  unlink(archive)
  .flowr_private_node_exe()
}

.flowr_npm_exe <- function(node) {
  # npm sits next to node; on Windows it is npm.cmd
  d <- dirname(node)
  cand <- if (.flowr_platform()$os == "win") file.path(d, "npm.cmd") else file.path(d, "bin", "npm")
  if (!file.exists(cand)) {
    cand <- if (.flowr_platform()$os == "win") file.path(d, "npm.cmd") else file.path(d, "npm")
  }
  if (file.exists(cand)) cand else unname(Sys.which("npm"))
}

.flowr_flowr_js <- function(version) {
  file.path(.flowr_node_dir(version), "node_modules", "@eagleoutice", "flowr", "cli", "flowr.js")
}

.flowr_node_installed <- function(version = flowr_option("flowr_version")) {
  file.exists(.flowr_flowr_js(version))
}

.flowr_install_node_flowr <- function(version, quiet = flowr_option("quiet")) {
  node <- .flowr_node_exe()
  if (is.na(node)) {
    if (!quiet) message("No suitable Node.js found; installing a private copy.")
    node <- .flowr_install_private_node(quiet = quiet)
  }
  npm <- .flowr_npm_exe(node)
  if (!nzchar(npm) || !file.exists(npm)) {
    stop("could not locate npm next to node at ", node, call. = FALSE)
  }
  dir <- .flowr_node_dir(version)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  if (!quiet) message("Installing @eagleoutice/flowr@", version, " via npm ...")
  log <- file.path(dir, "npm-install.log")
  status <- sys::exec_wait(
    npm,
    c("install", "--no-fund", "--no-audit", "--prefix", dir,
      paste0("@eagleoutice/flowr@", version)),
    std_out = log, std_err = log
  )
  if (status != 0 || !.flowr_node_installed(version)) {
    stop("npm install of flowR failed (see ", log, ")", call. = FALSE)
  }
  dir
}

# Docker engine ---------------------------------------------------------------

.flowr_docker_image <- function(version = flowr_option("flowr_version")) {
  paste0(flowr_option("docker_image"), ":", version)
}

# TRUE if docker is present and the flowR image for `version` is already pulled.
.flowr_docker_installed <- function(version = flowr_option("flowr_version")) {
  if (!nzchar(Sys.which("docker"))) {
    return(FALSE)
  }
  st <- tryCatch(sys::exec_internal("docker", c("image", "inspect", .flowr_docker_image(version)),
                                    error = FALSE),
                 error = function(e) NULL)
  !is.null(st) && st$status == 0
}

# Pull the flowR docker image for `version`.
.flowr_install_docker <- function(version, quiet = flowr_option("quiet")) {
  if (!nzchar(Sys.which("docker"))) {
    stop("the docker engine needs the 'docker' command on your PATH.", call. = FALSE)
  }
  image <- .flowr_docker_image(version)
  if (!quiet) {
    message("Pulling ", image, " ...")
  }
  status <- sys::exec_wait("docker", c("pull", image),
                           std_out = !quiet, std_err = !quiet)
  if (status != 0) {
    stop("docker pull failed for ", image, call. = FALSE)
  }
  invisible(TRUE)
}

# Engine argument construction ------------------------------------------------

# flowR parser-engine + wasm flags shared by binary and node engines.
.flowr_engine_flags <- function(flowr_engine, wasm) {
  flags <- character(0)
  if (identical(flowr_engine, "tree-sitter")) {
    # tree-sitter needs no R; disable r-shell so no R process is ever spawned.
    flags <- c("--default-engine", "tree-sitter", "--engine.r-shell.disabled")
  } else if (identical(flowr_engine, "r-shell")) {
    # r-shell reuses the R already on PATH (we are running inside R).
    flags <- c("--default-engine", "r-shell")
  } else {
    stop("unknown flowr_engine: ", flowr_engine, call. = FALSE)
  }
  # Always point flowR at the shipped wasm when we have it: the binary still
  # initialises the tree-sitter engine even under r-shell, and without the paths
  # it looks for the wasm relative to the cwd and aborts.
  if (!is.null(wasm)) {
    flags <- c(flags,
      "--engine.tree-sitter.wasm-path", wasm$r,
      "--engine.tree-sitter.tree-sitter-wasm-path", wasm$runtime)
  }
  # detailed flowR logging (to the server log / console) when requested
  if (isTRUE(flowr_option("verbose"))) {
    flags <- c(flags, "--verbose")
  }
  flags
}

.flowr_server_flags <- function(port, ws) {
  c("--server", if (isTRUE(ws)) "--ws", "--port", as.character(port))
}

# Spawn + lifecycle -----------------------------------------------------------

.flowr_new_handle <- function(provider, host, port, pid = NULL, owns = FALSE, log = NULL) {
  h <- new.env(parent = emptyenv())
  h$provider <- provider
  h$host <- host
  h$port <- port
  h$pid <- pid
  h$owns <- owns
  h$log <- log
  reg <- .flowr_state$engines %||% new.env(parent = emptyenv())
  .flowr_state$engines <- reg
  if (!is.null(pid)) {
    reg[[as.character(pid)]] <- h
  }
  h
}

# Pick a free TCP port by actually binding a listener to it: if we can bind, the
# flowR server can too. This is stricter than a connect probe (it also catches
# ports the OS/firewall will not let us listen on) and avoids the race where a
# port looks free to connect but cannot be bound.
.flowr_can_bind <- function(p) {
  s <- tryCatch(suppressWarnings(serverSocket(as.integer(p))), error = function(e) NULL)
  if (is.null(s)) {
    return(FALSE)
  }
  close(s)
  TRUE
}

.flowr_free_port <- function(preferred = NULL) {
  if (!is.null(preferred) && .flowr_can_bind(preferred)) {
    return(as.integer(preferred))
  }
  # restore the caller's RNG state so we do not perturb their set.seed() workflow
  if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
    old_seed <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
    on.exit(assign(".Random.seed", old_seed, envir = globalenv()), add = TRUE)
  }
  for (i in seq_len(100)) {
    p <- as.integer(sample(20000:65000, 1))
    if (.flowr_can_bind(p)) {
      return(p)
    }
  }
  stop("could not find a bindable free port for the flowR server ",
       "(are all local ports blocked?)", call. = FALSE)
}

# Wait until a server accepts connections on host:port, or time out.
.flowr_wait_ready <- function(host, port, pid, timeout, log) {
  deadline <- Sys.time() + timeout
  repeat {
    con <- tryCatch(
      suppressWarnings(socketConnection(host, port, server = FALSE,
                                        blocking = FALSE, open = "r+b", timeout = 2)),
      error = function(e) NULL
    )
    if (!is.null(con)) {
      close(con)
      return(invisible(TRUE))
    }
    if (!is.null(pid) && !.flowr_pid_alive(pid)) {
      stop("flowR server process exited during startup",
           if (!is.null(log) && file.exists(log)) paste0(":\n", .flowr_tail(log)) else "",
           call. = FALSE)
    }
    if (Sys.time() > deadline) {
      stop("timed out waiting for the flowR server to start",
           if (!is.null(log) && file.exists(log)) paste0(":\n", .flowr_tail(log)) else "",
           call. = FALSE)
    }
    Sys.sleep(0.2)
  }
}

.flowr_pid_alive <- function(pid) {
  isTRUE(tryCatch(is.na(sys::exec_status(pid, wait = FALSE)), error = function(e) FALSE))
}

.flowr_tail <- function(path, n = 20) {
  ln <- tryCatch(readLines(path, warn = FALSE), error = function(e) character(0))
  paste(utils::tail(ln, n), collapse = "\n")
}

# Engine registry -------------------------------------------------------------
#
# Every engine lives behind one spec so the starter and resolver stay generic:
# adding an engine is adding an entry here, nothing else changes. Each spec has
#   order     integer preference for `auto` (lower is tried first)
#   auto      whether `auto` may pick it (docker never is; it is opt-in)
#   ready     function(version) -> is it runnable now without a new download?
#   ensure    function(version, quiet) -> make it runnable (may download/consent)
#   spawn     function(version, flowr_engine, port, ws) -> list(cmd, args, host)
# The two host-and-wasm engines (binary/node) and the shipped bundle and docker
# all satisfy the same contract.
.flowr_engine_specs <- function() {
  list(
    binary = list(
      order = 1L, auto = TRUE,
      ready = function(v) .flowr_binary_installed(v),
      ensure = function(v, quiet) .flowr_ensure_engine("binary", v, quiet),
      spawn = function(v, fe, port, ws) {
        dir <- .flowr_binary_dir(v, .flowr_platform()$key)
        exe <- file.path(dir, paste0("flowr", .flowr_platform()$exe))
        list(cmd = exe, host = "127.0.0.1",
             args = c(.flowr_server_flags(port, ws),
                      .flowr_engine_flags(fe, .flowr_find_wasm(dir))))
      },
      console = function(v, fe) {
        dir <- .flowr_binary_dir(v, .flowr_platform()$key)
        list(cmd = file.path(dir, paste0("flowr", .flowr_platform()$exe)),
             args = .flowr_engine_flags(fe, .flowr_find_wasm(dir)))
      }
    ),
    bundled = list(
      order = 2L, auto = TRUE,
      ready = function(v) .flowr_bundled_available() && !is.na(.flowr_node_exe()),
      ensure = function(v, quiet) {
        if (!.flowr_bundled_available()) {
          stop("the bundled flowR is missing from this installation", call. = FALSE)
        }
        if (is.na(.flowr_node_exe())) {
          stop("the bundled engine needs Node.js on your PATH; none was found.\n",
               "Install Node.js, or use the self-contained binary: ",
               "flowr_install(engine = \"binary\").", call. = FALSE)
        }
      },
      spawn = function(v, fe, port, ws) {
        dir <- .flowr_bundled_dir()
        list(cmd = .flowr_node_exe(), host = "127.0.0.1",
             args = c(file.path(dir, "flowr.min.js"), .flowr_server_flags(port, ws),
                      .flowr_engine_flags(fe, .flowr_find_wasm(dir))))
      },
      console = function(v, fe) {
        dir <- .flowr_bundled_dir()
        list(cmd = .flowr_node_exe(),
             args = c(file.path(dir, "flowr.min.js"),
                      .flowr_engine_flags(fe, .flowr_find_wasm(dir))))
      }
    ),
    node = list(
      order = 3L, auto = TRUE,
      ready = function(v) .flowr_node_installed(v),
      ensure = function(v, quiet) .flowr_ensure_engine("node", v, quiet),
      spawn = function(v, fe, port, ws) {
        node <- .flowr_node_exe()
        if (is.na(node)) {
          node <- .flowr_install_private_node(quiet = flowr_option("quiet"))
        }
        list(cmd = node, host = "127.0.0.1",
             args = c(.flowr_flowr_js(v), .flowr_server_flags(port, ws),
                      .flowr_engine_flags(fe, .flowr_find_wasm(.flowr_node_dir(v)))))
      },
      console = function(v, fe) {
        node <- .flowr_node_exe()
        if (is.na(node)) {
          node <- .flowr_install_private_node(quiet = flowr_option("quiet"))
        }
        list(cmd = node,
             args = c(.flowr_flowr_js(v),
                      .flowr_engine_flags(fe, .flowr_find_wasm(.flowr_node_dir(v)))))
      }
    ),
    docker = list(
      order = 4L, auto = FALSE,
      ready = function(v) .flowr_docker_installed(v),
      ensure = function(v, quiet) {
        if (!nzchar(Sys.which("docker"))) {
          stop("the docker engine needs the 'docker' command on your PATH.",
               call. = FALSE)
        }
        # `docker run` pulls the image on demand, so presence of docker is enough
      },
      spawn = function(v, fe, port, ws) {
        image <- paste0(flowr_option("docker_image"), ":", v)
        list(cmd = "docker", host = "127.0.0.1",
             args = c("run", "--rm", "-p", sprintf("127.0.0.1:%d:%d", port, port),
                      image, .flowr_server_flags(port, ws),
                      .flowr_engine_flags(fe, NULL)))
      }
    )
  )
}

# The known engine names (single source of truth for match.arg elsewhere).
.flowr_engine_names <- function() {
  names(.flowr_engine_specs())
}

# Start an engine and return a ready handle (does not open the client socket).
.flowr_start_engine <- function(engine = flowr_option("engine"),
                                flowr_version = flowr_option("flowr_version"),
                                flowr_engine = flowr_option("flowr_engine"),
                                host = flowr_option("host"),
                                port = flowr_option("port"),
                                ws = flowr_option("ws"),
                                quiet = flowr_option("quiet")) {
  requested <- engine
  engine <- .flowr_resolve_engine(engine, flowr_version)
  timeout <- flowr_option("connect_timeout")
  .flowr_log("engine resolved: '", requested, "' -> '", engine, "' (flowR ", flowr_version, ")")

  # `auto` silently fell back to the bundled engine because no binary is present;
  # tell the user how to get a standalone binary and why (bundled needs Node).
  if (identical(requested, "auto") && identical(engine, "bundled") &&
      !isTRUE(tryCatch(.flowr_binary_installed(flowr_version),
                       error = function(e) FALSE))) {
    .flowr_notify("no-binary", .flowr_no_binary_msg())
  }

  # Secure-mode invariants (default on). We never build a shell string anywhere
  # (sys::exec_* takes an argv vector), so there is no shell-escape surface.
  if (isTRUE(flowr_option("secure")) && identical(flowr_engine, "r-shell")) {
    stop("secure mode forbids the r-shell engine (it would hand code to an R ",
         "interpreter). Use flowr_engine = \"tree-sitter\", or set ",
         "options(flowr.secure = FALSE).", call. = FALSE)
  }

  spec <- .flowr_engine_specs()[[engine]]
  if (is.null(spec)) {
    stop("unknown flowR engine: ", engine, call. = FALSE)
  }
  spec$ensure(flowr_version, quiet)
  port <- .flowr_free_port(port)
  s <- spec$spawn(flowr_version, flowr_engine, port, ws)
  .flowr_spawn_wait(engine, s$cmd, s$args, s$host, port, timeout)
}

# Spawn a server process and wait until it is ready; on any failure the child is
# killed so a failed start never leaks a process.
.flowr_spawn_wait <- function(provider, cmd, args, host, port, timeout) {
  log <- tempfile(paste0("flowr-", provider, "-"), fileext = ".log")
  .flowr_log("starting ", provider, " engine: ", cmd, " ", paste(args, collapse = " "))
  .flowr_log("server log: ", log)
  pid <- sys::exec_background(cmd, args, std_out = log, std_err = log)
  tryCatch(
    .flowr_wait_ready(host, port, pid, timeout, log),
    error = function(e) {
      tryCatch(tools::pskill(pid), error = function(.) NULL)
      stop(e)
    }
  )
  .flowr_new_handle(provider, host, port, pid = pid, owns = TRUE, log = log)
}

# The flowR JS+wasm bundle shipped inside the package (universal, needs Node).
.flowr_bundled_dir <- function() {
  system.file("flowr-js", package = "flowr")
}

.flowr_bundled_available <- function() {
  d <- .flowr_bundled_dir()
  nzchar(d) && file.exists(file.path(d, "flowr.min.js"))
}

# Decide which concrete engine `auto` uses: the first auto-eligible engine (in
# registry order) that is ready now (already downloaded binary; then the shipped
# JS bundle when Node is present; then an npm-installed flowR), otherwise the
# binary, downloaded on demand.
.flowr_resolve_engine <- function(engine, flowr_version) {
  specs <- .flowr_engine_specs()
  if (engine != "auto") {
    return(match.arg(engine, names(specs)))
  }
  ordered <- names(specs)[order(vapply(specs, function(s) s$order, integer(1)))]
  for (nm in ordered) {
    spec <- specs[[nm]]
    if (isTRUE(spec$auto) &&
        isTRUE(tryCatch(spec$ready(flowr_version), error = function(e) FALSE))) {
      return(nm)
    }
  }
  "binary"
}


.flowr_stop_engine <- function(handle) {
  if (is.null(handle) || !isTRUE(handle$owns) || is.null(handle$pid)) {
    return(invisible(FALSE))
  }
  tryCatch(tools::pskill(handle$pid), error = function(e) NULL)
  reg <- .flowr_state$engines
  if (!is.null(reg)) {
    key <- as.character(handle$pid)
    if (!is.null(reg[[key]])) rm(list = key, envir = reg)
  }
  invisible(TRUE)
}

# Kill anything we spawned when the package is unloaded.
.onUnload <- function(libpath) {
  reg <- .flowr_state$engines
  if (!is.null(reg)) {
    for (key in ls(reg)) {
      tryCatch(tools::pskill(reg[[key]]$pid), error = function(e) NULL)
    }
  }
}
