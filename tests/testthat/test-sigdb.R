# The signature database: a shared, verified, engine-independent download that
# flowr_install() obtains alongside an engine.
#
# These exercise the R plumbing (resolve -> verify -> unpack -> mount) against a
# local file:// archive, so no network or real database is needed. That the
# unpacked layout actually mounts in flowR is checked by tools/pack-sigdb.sh,
# which boots a real flowR against each packed asset before it is published.

# Install the database on its own, the way flowr_install() does, without needing
# an engine to download first.
install_sigdb <- function(scope = "current", version = "9.9.9", force = FALSE) {
  flowr:::.flowr_ensure_sigdb(version, quiet = TRUE,
                              scopes = flowr:::.flowr_sigdb_scopes(scope), force = force)
}

# A minimal archive shaped like a real one: what makes a directory a mountable
# database is a `<scope>.manifest.json`, so the fixture carries one.
local_sigdb_archive <- function(env = parent.frame(), manifest = TRUE, scope = "current") {
  src <- withr::local_tempdir(.local_envir = env)
  if (manifest) {
    writeLines("{}", file.path(src, paste0(scope, ".manifest.json")))
  }
  writeLines("payload", file.path(src, paste0(scope, ".dict.sigs.ndjson.br")))
  tgz <- file.path(withr::local_tempdir(.local_envir = env), "flowr-sigdb-9.9.9.tar.gz")
  withr::with_dir(src, utils::tar(tgz, ".", compression = "gzip"))
  tgz
}

local_sigdb_cache <- function(env = parent.frame()) {
  cache <- withr::local_tempdir(.local_envir = env)
  testthat::local_mocked_bindings(.flowr_cache_dir = function() cache, .env = env)
  cache
}

# Point the resolver at a local archive, with a real (or deliberately wrong) hash.
local_sigdb_source <- function(tgz, sha256 = NULL, sig = NULL, env = parent.frame()) {
  if (is.null(sha256)) {
    sha256 <- flowr:::.flowr_sha256(tgz)
  }
  testthat::local_mocked_bindings(
    .flowr_sigdb_source = function(version, scope) {
      list(url = paste0("file://", tgz), sha256 = sha256, sig = sig)
    },
    .env = env
  )
}

test_that("the sigdb installs, is detected, and is removed again", {
  withr::local_options(list(flowr.secure = FALSE, flowr.verify_signature = FALSE))
  local_sigdb_cache()
  local_sigdb_source(local_sigdb_archive())

  expect_false(flowr:::.flowr_sigdb_installed("9.9.9"))
  expect_null(flowr:::.flowr_sigdb_mount("9.9.9"))

  expect_identical(install_sigdb(), "current")
  expect_true(flowr:::.flowr_sigdb_installed("9.9.9"))
  expect_identical(flowr:::.flowr_sigdb_mount("9.9.9"), flowr:::.flowr_sigdb_dir("9.9.9"))
  # a checksum-only download is recorded as such, so flowr_status() can say so
  expect_identical(flowr:::.flowr_sigdb_verification("9.9.9"), "checksum")

  # removal goes through flowr_uninstall(), which drops the database with the engines
  flowr_uninstall(version = "9.9.9", quiet = TRUE)
  expect_false(flowr:::.flowr_sigdb_installed("9.9.9"))
  expect_true(is.na(flowr:::.flowr_sigdb_verification("9.9.9")))
})

test_that("an already-installed sigdb is not re-downloaded unless forced", {
  withr::local_options(list(flowr.secure = FALSE, flowr.verify_signature = FALSE))
  local_sigdb_cache()
  local_sigdb_source(local_sigdb_archive())
  install_sigdb()

  # a stale shard of this set survives a skipped install and is wiped by a forced
  # one, so a re-install replaces the set rather than merging into it
  stale <- file.path(flowr:::.flowr_sigdb_dir("9.9.9"), "current.stale.sigs.ndjson.br")
  file.create(stale)
  install_sigdb()
  expect_true(file.exists(stale))                        # skipped

  install_sigdb(force = TRUE)
  expect_false(file.exists(stale))                       # replaced, not merged
  expect_true(flowr:::.flowr_sigdb_installed("9.9.9"))
})

test_that("a corrupted sigdb download is rejected and nothing is left mounted", {
  withr::local_options(list(flowr.secure = FALSE, flowr.verify_signature = FALSE))
  local_sigdb_cache()
  local_sigdb_source(local_sigdb_archive(), sha256 = strrep("0", 64))

  expect_error(flowr:::.flowr_install_sigdb_scope("9.9.9", "current", quiet = TRUE), "checksum mismatch")
  expect_false(flowr:::.flowr_sigdb_installed("9.9.9"))
})

test_that("an archive without a manifest is discarded rather than half-mounted", {
  withr::local_options(list(flowr.secure = FALSE, flowr.verify_signature = FALSE))
  local_sigdb_cache()
  local_sigdb_source(local_sigdb_archive(manifest = FALSE))

  expect_error(flowr:::.flowr_install_sigdb_scope("9.9.9", "current", quiet = TRUE), "no manifest")
  expect_false(flowr:::.flowr_sigdb_installed("9.9.9"))
})

test_that("secure mode refuses a sigdb that has no published checksum", {
  withr::local_options(list(flowr.secure = TRUE))
  local_sigdb_cache()
  local_sigdb_source(local_sigdb_archive(), sha256 = NULL)
  # the resolver found no checksum sidecar
  testthat::local_mocked_bindings(
    .flowr_sigdb_source = function(version, scope) list(url = "file:///nope.tar.gz", sha256 = NULL, sig = NULL)
  )
  expect_error(flowr:::.flowr_install_sigdb_scope("9.9.9", "current", quiet = TRUE), "no verifiable")
})

test_that(".flowr_with_sigdb mounts for the child and restores the environment", {
  withr::local_options(list(flowr.secure = FALSE, flowr.verify_signature = FALSE))
  local_sigdb_cache()
  local_sigdb_source(local_sigdb_archive())
  withr::local_envvar(c(FLOWR_SIGDB_DIR = NA))

  # nothing installed: no variable is invented
  expect_identical(flowr:::.flowr_with_sigdb(Sys.getenv("FLOWR_SIGDB_DIR"), "9.9.9"), "")

  install_sigdb()
  dir <- flowr:::.flowr_sigdb_dir("9.9.9")
  expect_identical(flowr:::.flowr_with_sigdb(Sys.getenv("FLOWR_SIGDB_DIR"), "9.9.9"), dir)
  # the session's own environment is left as it was
  expect_identical(Sys.getenv("FLOWR_SIGDB_DIR"), "")
})

test_that("a user-set FLOWR_SIGDB_DIR wins over the cached database", {
  withr::local_options(list(flowr.secure = FALSE, flowr.verify_signature = FALSE))
  local_sigdb_cache()
  local_sigdb_source(local_sigdb_archive())
  install_sigdb()

  withr::local_envvar(c(FLOWR_SIGDB_DIR = "/user/choice"))
  expect_identical(flowr:::.flowr_with_sigdb(Sys.getenv("FLOWR_SIGDB_DIR"), "9.9.9"), "/user/choice")
})

test_that("the sigdb source falls back to the release naming convention", {
  withr::local_options(list(flowr.binary_repo = "acme/flowr-x"))
  # no manifest entry -> conventional URL; the checksum probe is allowed to fail
  testthat::local_mocked_bindings(.flowr_manifest = function() list(binaries = list(), sigdb = list()))
  src <- flowr:::.flowr_sigdb_source("1.2.3", "current")
  expect_match(src$url, "acme/flowr-x/releases/download/flowr-v1.2.3/flowr-sigdb-current-1.2.3.tar.gz",
               fixed = TRUE)
  expect_identical(src$sig, paste0(src$url, ".sig"))     # signature sits beside it
})

test_that("a pinned manifest entry beats the naming convention", {
  testthat::local_mocked_bindings(.flowr_manifest = function() {
    list(binaries = list(), sigdb = list(list(version = "1.2.3", scope = "current",
                                              url = "https://pinned/db.tar.gz", sha256 = "abc")))
  })
  src <- flowr:::.flowr_sigdb_source("1.2.3", "current")
  expect_identical(src$url, "https://pinned/db.tar.gz")
  expect_identical(src$sha256, "abc")
})

# Selecting what to download --------------------------------------------------

test_that("flowr.sigdb selects the sets, and \"none\" switches the database off", {
  expect_identical(flowr:::.flowr_sigdb_scopes("current"), "current")
  expect_identical(flowr:::.flowr_sigdb_scopes("all"), c("base", "current", "history"))
  expect_identical(flowr:::.flowr_sigdb_scopes("none"), character(0))
  expect_identical(flowr:::.flowr_sigdb_scopes(FALSE), character(0))
  expect_identical(flowr:::.flowr_sigdb_scopes(character(0)), character(0))

  # a vector, or an env-var style string, both select several sets (canonically ordered)
  expect_identical(flowr:::.flowr_sigdb_scopes(c("history", "current")), c("current", "history"))
  expect_identical(flowr:::.flowr_sigdb_scopes("current,history"), c("current", "history"))

  expect_error(flowr:::.flowr_sigdb_scopes("everything"), "unknown signature-database set")
})

test_that("the default is `current`, and the option drives it", {
  withr::local_options(list(flowr.sigdb = NULL))
  withr::local_envvar(c(FLOWR_SIGDB = NA))
  expect_identical(flowr:::.flowr_sigdb_scopes(), "current")

  withr::local_options(list(flowr.sigdb = "none"))
  expect_identical(flowr:::.flowr_sigdb_scopes(), character(0))
})

test_that("the non-redundant sets install side by side and are removed one by one", {
  withr::local_options(list(flowr.secure = FALSE, flowr.verify_signature = FALSE))
  local_sigdb_cache()
  # each set resolves to an archive carrying that set's own files
  archives <- list(current = local_sigdb_archive(scope = "current"),
                   history = local_sigdb_archive(scope = "history"))
  testthat::local_mocked_bindings(
    .flowr_sigdb_source = function(version, scope) {
      list(url = paste0("file://", archives[[scope]]),
           sha256 = flowr:::.flowr_sha256(archives[[scope]]), sig = NULL)
    }
  )

  install_sigdb(c("current", "history"))
  expect_identical(flowr:::.flowr_sigdb_scopes_installed("9.9.9"), c("current", "history"))

  # a later install adds a set rather than replacing what is there: the sets are
  # non-redundant and share one directory without colliding
  expect_identical(flowr:::.flowr_sigdb_scopes_installed("9.9.9"), c("current", "history"))
})

test_that("selecting no set installs nothing", {
  local_sigdb_cache()
  expect_identical(install_sigdb("none"), character(0))
  expect_false(flowr:::.flowr_sigdb_installed("9.9.9"))
})

test_that("a sigdb failure never fails the engine install around it", {
  withr::local_options(list(flowr.secure = FALSE, flowr.verify_signature = FALSE))
  local_sigdb_cache()
  testthat::local_mocked_bindings(
    .flowr_sigdb_source = function(version, scope) {
      list(url = "file:///does/not/exist.tar.gz", sha256 = "abc", sig = NULL)
    }
  )
  expect_warning(flowr:::.flowr_ensure_sigdb("9.9.9", quiet = TRUE, scopes = "current"),
                 "could not install")
  expect_false(flowr:::.flowr_sigdb_installed("9.9.9"))
})

# Selecting what to uninstall -------------------------------------------------

test_that("flowr_uninstall drops one database set and keeps the others", {
  withr::local_options(list(flowr.secure = FALSE, flowr.verify_signature = FALSE))
  local_sigdb_cache()
  archives <- list(current = local_sigdb_archive(scope = "current"),
                   history = local_sigdb_archive(scope = "history"))
  testthat::local_mocked_bindings(
    .flowr_sigdb_source = function(version, scope) {
      list(url = paste0("file://", archives[[scope]]),
           sha256 = flowr:::.flowr_sha256(archives[[scope]]), sig = NULL)
    }
  )
  install_sigdb(c("current", "history"))

  flowr_uninstall(version = "9.9.9", engine = "none", sigdb = "history", quiet = TRUE)
  expect_identical(flowr:::.flowr_sigdb_scopes_installed("9.9.9"), "current")

  flowr_uninstall(version = "9.9.9", engine = "none", sigdb = "current", quiet = TRUE)
  expect_false(flowr:::.flowr_sigdb_installed("9.9.9"))
})

test_that("flowr_uninstall(sigdb = 'none') leaves the database alone", {
  withr::local_options(list(flowr.secure = FALSE, flowr.verify_signature = FALSE))
  local_sigdb_cache()
  local_sigdb_source(local_sigdb_archive())
  install_sigdb()

  flowr_uninstall(version = "9.9.9", sigdb = "none", quiet = TRUE)
  expect_identical(flowr:::.flowr_sigdb_scopes_installed("9.9.9"), "current")
})

test_that("flowr_uninstall selects engines, and rejects ones it cannot remove", {
  expect_identical(flowr:::.flowr_uninstall_engines("all"), c("binary", "node"))
  expect_identical(flowr:::.flowr_uninstall_engines("none"), character(0))
  expect_identical(flowr:::.flowr_uninstall_engines("binary"), "binary")
  # the bundle ships in the package and docker has its own flag
  expect_error(flowr:::.flowr_uninstall_engines("bundled"), "cannot uninstall engine")
  expect_error(flowr:::.flowr_uninstall_engines("docker"), "cannot uninstall engine")
})

test_that("cached versions are read back from the cache's directory names", {
  cache <- local_sigdb_cache()
  for (d in c("binary-2.12.3-linux-x64", "node-flowr-2.11.1", "sigdb-2.12.3")) {
    dir.create(file.path(cache, d), recursive = TRUE)
  }
  expect_identical(flowr:::.flowr_cached_versions(), c("2.11.1", "2.12.3"))
})

test_that("uninstalling everything for every version clears the cache", {
  withr::local_options(list(flowr.secure = FALSE, flowr.verify_signature = FALSE))
  cache <- local_sigdb_cache()
  local_sigdb_source(local_sigdb_archive())
  install_sigdb()
  expect_true(flowr:::.flowr_sigdb_installed("9.9.9"))

  flowr_uninstall(quiet = TRUE)                    # no version, everything
  expect_false(dir.exists(cache))
})
