# Engine resolution, platform detection and secure-mode enforcement.

test_that("platform detection returns a supported os/arch key", {
  p <- flowr:::.flowr_platform()
  expect_true(p$os %in% c("win", "linux", "darwin"))
  expect_true(p$arch %in% c("x64", "arm64"))
  expect_identical(p$key, paste0(p$os, "-", p$arch))
})

test_that("auto resolves to a real engine when nothing is cached", {
  # Point the cache somewhere empty so no downloaded engine looks installed.
  withr::local_envvar(c(R_USER_CACHE_DIR = withr::local_tempdir()))
  expected <- if (flowr:::.flowr_bundled_available() && !is.na(flowr:::.flowr_node_exe())) {
    "bundled"
  } else {
    "binary"
  }
  expect_identical(flowr:::.flowr_resolve_engine("auto", "2.11.1"), expected)
})

test_that("explicit engine names are validated", {
  expect_identical(flowr:::.flowr_resolve_engine("node", "2.11.1"), "node")
  expect_error(flowr:::.flowr_resolve_engine("banana", "2.11.1"))
})

test_that("secure mode forbids the r-shell engine", {
  withr::local_options(list(flowr.secure = TRUE))
  expect_error(
    flowr_connect(engine = "binary", flowr_engine = "r-shell"),
    "r-shell"
  )
})
