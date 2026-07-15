# Configuration resolution and secure defaults.

test_that("option resolution follows arg > option > env > default", {
  withr::local_options(list(flowr.flowr_version = NULL))
  withr::local_envvar(c(FLOWR_FLOWR_VERSION = NA))
  expect_identical(flowr:::flowr_option("flowr_version"), "2.12.3") # default

  withr::local_envvar(c(FLOWR_FLOWR_VERSION = "9.9.9"))
  expect_identical(flowr:::flowr_option("flowr_version"), "9.9.9")  # env beats default

  withr::local_options(list(flowr.flowr_version = "8.8.8"))
  expect_identical(flowr:::flowr_option("flowr_version"), "8.8.8")  # option beats env

  expect_identical(flowr:::flowr_option("flowr_version", "7.7.7"), "7.7.7") # arg wins
})

test_that("env vars are coerced to the default's type", {
  withr::local_envvar(c(FLOWR_PORT = "1234"))
  expect_identical(flowr:::flowr_option("port"), 1234L)
  withr::local_envvar(c(FLOWR_SECURE = "false"))
  expect_false(flowr:::flowr_option("secure"))
})

test_that("secure mode is on by default", {
  withr::local_options(list(flowr.secure = NULL))
  withr::local_envvar(c(FLOWR_SECURE = NA))
  expect_true(flowr:::flowr_option("secure"))
})

test_that("unknown options are rejected", {
  expect_error(flowr:::flowr_option("nonsense"), "unknown flowr option")
})

test_that("flowr_config returns every documented option", {
  cfg <- flowr_config()
  expect_true(all(c("engine", "flowr_version", "flowr_engine", "secure") %in% names(cfg)))
  expect_s3_class(cfg, "flowr_config")
})

test_that("flowr_set_config sets options and rejects unknown names", {
  withr::local_options(list(flowr.request_timeout = NULL))
  flowr_set_config(request_timeout = 321)
  expect_identical(getOption("flowr.request_timeout"), 321)
  expect_error(flowr_set_config(nope = 1), "unknown flowr setting")
  expect_error(flowr_set_config(), "named settings")
})

test_that("assigning into a flowr_config object sets the option", {
  withr::local_options(list(flowr.engine = NULL))
  cfg <- flowr_config()
  cfg$engine <- "binary"
  expect_identical(getOption("flowr.engine"), "binary")
  cfg[["engine"]] <- "node"
  expect_identical(getOption("flowr.engine"), "node")
})

test_that("string values coerce to the option's type", {
  withr::local_options(list(flowr.port = NULL))
  flowr_set_config(port = "4242")
  expect_identical(getOption("flowr.port"), 4242L)
})

test_that("flowr.timing controls the per-command timing line", {
  st <- flowr:::.flowr_state
  st$timing_active <- NULL
  withr::local_options(list(flowr.timing = TRUE))
  expect_message(flowr:::.flowr_timer("slice")(), "slice:.*ms")

  st$timing_active <- NULL
  withr::local_options(list(flowr.timing = FALSE))
  expect_silent(flowr:::.flowr_timer("slice")())
})

test_that("flowr.verbose adds --verbose to the engine flags", {
  withr::local_options(list(flowr.verbose = TRUE))
  expect_true("--verbose" %in% flowr:::.flowr_engine_flags("tree-sitter", NULL))
  withr::local_options(list(flowr.verbose = FALSE))
  expect_false("--verbose" %in% flowr:::.flowr_engine_flags("tree-sitter", NULL))
})

test_that("flowr_log errors without an active session", {
  flowr_disconnect()
  expect_error(flowr_log(), "no active flowR session")
})

test_that("nested timers report only once (outermost wins)", {
  st <- flowr:::.flowr_state
  st$timing_active <- NULL
  withr::local_options(list(flowr.timing = TRUE))
  outer <- flowr:::.flowr_timer("outer")
  inner <- flowr:::.flowr_timer("inner")   # reentrant: a no-op
  expect_silent(inner())
  expect_message(outer(), "outer")
})

# flowR 2.12 slicing knobs ----------------------------------------------------

test_that(".flowr_flag accepts only a single non-NA logical", {
  expect_true(flowr:::.flowr_flag(TRUE, "x"))
  expect_false(flowr:::.flowr_flag(FALSE, "x"))
  expect_error(flowr:::.flowr_flag("yes", "include_callees"), "include_callees")
  expect_error(flowr:::.flowr_flag(NA, "x"), "TRUE or FALSE")
  expect_error(flowr:::.flowr_flag(c(TRUE, TRUE), "x"), "TRUE or FALSE")
})

test_that(".flowr_inline_warnings always yields the same columns", {
  cols <- c("kind", "id", "path")
  empty <- flowr:::.flowr_inline_warnings(NULL)
  expect_s3_class(empty, "data.frame")
  expect_identical(names(empty), cols)
  expect_identical(nrow(empty), 0L)
  expect_identical(names(flowr:::.flowr_inline_warnings(list())), cols)

  # `path` is optional in flowR's payload: an unresolved source() may omit it
  got <- flowr:::.flowr_inline_warnings(list(
    list(kind = "cycle", callId = "3", path = "a.R"),
    list(kind = "unresolved", callId = "9")
  ))
  expect_identical(nrow(got), 2L)
  expect_identical(names(got), cols)
  expect_identical(got$kind, c("cycle", "unresolved"))
  expect_identical(got$id, c("3", "9"))
  expect_identical(got$path, c("a.R", NA_character_))
})
