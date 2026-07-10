# Package the cached flowR binary, sign it with flowr-signing-key.pem, and write
# a local manifest so the binary engine verifies it. Local testing only; run from
# the package root via `make reinstall_signed`.
suppressPackageStartupMessages(library(flowr))
stopifnot(file.exists("flowr-signing-key.pem"), nchar(Sys.which("openssl")) > 0)

version <- flowr:::flowr_option("flowr_version")
plat <- flowr:::.flowr_platform()
bindir <- flowr:::.flowr_binary_dir(version, plat$key)
exe <- file.path(bindir, paste0("flowr", plat$exe))
if (!file.exists(exe)) stop("no cached binary; run flowr_install() first", call. = FALSE)

dir.create("signed-build", showWarnings = FALSE)
archive <- normalizePath(file.path("signed-build",
  sprintf("flowr-%s-%s.tar.gz", version, plat$key)), mustWork = FALSE)
files <- c(basename(exe), basename(unlist(flowr:::.flowr_find_wasm(bindir))))
stopifnot(system2("tar", c("-czf", shQuote(archive), "-C", shQuote(bindir), files)) == 0)
sig <- paste0(archive, ".sig")
stopifnot(system2("openssl", c("dgst", "-sha256", "-sign", "flowr-signing-key.pem",
                               "-out", shQuote(sig), shQuote(archive))) == 0)

writeLines(jsonlite::toJSON(auto_unbox = TRUE, pretty = TRUE, list(binaries = list(list(
  version = version, platform = plat$key, sha256 = flowr:::.flowr_sha256(archive),
  url = paste0("file://", archive), sig = paste0("file://", sig))))),
  "inst/flowr-manifest.json")
cat("signed", basename(archive), "\n")
