# Engine resolution and the download checksum guard (no network).

test_that("auto prefers the shipped bundle when Node is present and no binary is cached", {
  withr::local_envvar(c(R_USER_CACHE_DIR = withr::local_tempdir()))  # empty cache
  skip_if(is.na(flowr:::.flowr_node_exe()), "no Node.js available")
  skip_if_not(flowr:::.flowr_bundled_available())
  expect_identical(flowr:::.flowr_resolve_engine("auto", "2.11.1"), "bundled")
})

test_that("binary verification level is read from the install marker", {
  withr::local_envvar(c(R_USER_CACHE_DIR = withr::local_tempdir()))
  plat <- flowr:::.flowr_platform()
  dir <- flowr:::.flowr_binary_dir("9.9.9", plat$key)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  # not installed yet -> NA
  expect_true(is.na(flowr:::.flowr_binary_verification("9.9.9")))
  writeLines("", file.path(dir, paste0("flowr", plat$exe)))   # now "installed"
  expect_identical(flowr:::.flowr_binary_verification("9.9.9"), "none")  # no marker
  writeLines("signature", file.path(dir, "VERIFICATION"))
  expect_identical(flowr:::.flowr_binary_verification("9.9.9"), "signature")
})

test_that("the engine registry exposes the expected engines", {
  specs <- flowr:::.flowr_engine_specs()
  expect_setequal(names(specs), c("binary", "bundled", "node", "docker"))
  for (s in specs) {
    expect_true(is.function(s$ready) && is.function(s$ensure) && is.function(s$spawn))
  }
  # docker is opt-in: auto never selects it
  expect_false(specs$docker$auto)
})

test_that("flowr_is_installed('all') reports every engine as a named logical", {
  got <- flowr_is_installed("all")
  expect_type(got, "logical")
  expect_setequal(names(got), names(flowr:::.flowr_engine_specs()))
  expect_identical(flowr_is_installed("any"), any(got))
})

test_that("the shipped bundle is complete (js + both wasm)", {
  d <- flowr:::.flowr_bundled_dir()
  skip_if_not(nzchar(d))
  expect_true(file.exists(file.path(d, "flowr.min.js")))
  expect_false(is.null(flowr:::.flowr_find_wasm(d)))
})

test_that("a binary download is rejected on a checksum mismatch", {
  skip_if_not_installed("digest")
  src <- tempfile(fileext = ".bin")
  writeBin(as.raw(rep(0:255, 8)), src)
  good <- flowr:::.flowr_sha256(src)
  url <- paste0("file://", normalizePath(src))

  ok <- tempfile()
  suppressWarnings(flowr:::.flowr_download_verify(url, good, ok))
  expect_true(file.exists(ok))                       # correct checksum passes

  bad <- tempfile()
  expect_error(
    suppressWarnings(flowr:::.flowr_download_verify(url, "deadbeef", bad)),
    "checksum mismatch"
  )
})

# Signature verification (the pinned-key scheme, secure-mode fail-closed
# behaviour, tamper/wrong-key rejection) is covered in test-signature.R against
# the actual .flowr_verify_sig / .flowr_verify_signature code.
