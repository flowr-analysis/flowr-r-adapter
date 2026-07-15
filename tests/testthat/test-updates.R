# Version checks. Knowing about a newer release is a nicety: these pin down that
# it stays silent when current, and degrades to silence -- never an error, a
# warning, or a stall -- when the network is unavailable or hostile.

# the per-session answer is cached; each test starts from "not yet asked"
reset_updates <- function() {
  st <- flowr:::.flowr_state
  st$updates <- NULL
  invisible(NULL)
}

test_that("nothing is said when everything is up to date", {
  reset_updates()
  withr::local_options(list(flowr.check_updates = TRUE))
  testthat::local_mocked_bindings(
    .flowr_latest_version = function() flowr_option("flowr_version"),
    .flowr_latest_adapter_version = function() as.character(utils::packageVersion("flowr"))
  )
  expect_identical(flowr:::.flowr_updates(), list())
})

test_that("a newer release is reported, with what you have and what is out", {
  reset_updates()
  withr::local_options(list(flowr.check_updates = TRUE, flowr.flowr_version = "2.12.3"))
  testthat::local_mocked_bindings(
    .flowr_latest_version = function() "2.13.0",
    .flowr_latest_adapter_version = function() "99.0.0"
  )
  up <- flowr:::.flowr_updates()
  expect_identical(up$flowR, c("2.12.3", "2.13.0"))
  expect_identical(up$flowr[2], "99.0.0")
})

test_that("an older published version is not mistaken for an update", {
  reset_updates()
  withr::local_options(list(flowr.check_updates = TRUE, flowr.flowr_version = "2.12.3"))
  testthat::local_mocked_bindings(
    .flowr_latest_version = function() "2.11.1",          # older than ours
    .flowr_latest_adapter_version = function() NULL
  )
  expect_identical(flowr:::.flowr_updates(), list())
})

test_that("being offline is silent, not an error", {
  reset_updates()
  withr::local_options(list(flowr.check_updates = TRUE))
  # every lookup fails to resolve, as it would with no connection
  testthat::local_mocked_bindings(
    .flowr_latest_version = function() NULL,
    .flowr_latest_adapter_version = function() NULL
  )
  expect_silent(res <- flowr:::.flowr_updates())
  expect_identical(res, list())
})

test_that("a check that throws can never break the caller", {
  reset_updates()
  withr::local_options(list(flowr.check_updates = TRUE))
  testthat::local_mocked_bindings(
    .flowr_latest_version = function() stop("network on fire"),
    .flowr_latest_adapter_version = function() stop("network on fire")
  )
  expect_silent(res <- flowr:::.flowr_updates())
  expect_identical(res, list())
})

test_that("a garbage reply is not mistaken for a version", {
  expect_null(flowr:::.flowr_one_version(NULL))
  expect_null(flowr:::.flowr_one_version(list(1, 2)))
  expect_null(flowr:::.flowr_one_version(character(0)))
  expect_null(flowr:::.flowr_one_version(""))
  expect_null(flowr:::.flowr_one_version(c("1.0", "2.0")))
  expect_identical(flowr:::.flowr_one_version("v1.2.3"), "1.2.3")   # tags carry a v
})

test_that("an unreachable host returns NULL without signalling or hanging", {
  # .invalid is reserved by RFC 2606: it can never resolve
  expect_silent(res <- flowr:::.flowr_fetch_json("https://flowr.invalid/x.json", timeout = 1))
  expect_null(res)
})

test_that("the fetch restores the global timeout it lowered", {
  before <- getOption("timeout")
  flowr:::.flowr_fetch_json("https://flowr.invalid/x.json", timeout = 1)
  expect_identical(getOption("timeout"), before)
})

test_that("check_updates = FALSE never looks", {
  reset_updates()
  withr::local_options(list(flowr.check_updates = FALSE))
  testthat::local_mocked_bindings(
    .flowr_latest_version = function() stop("must not be called"),
    .flowr_latest_adapter_version = function() stop("must not be called")
  )
  expect_identical(flowr:::.flowr_updates(), list())
})

test_that("the answer is cached, so a session asks at most once", {
  reset_updates()
  withr::local_options(list(flowr.check_updates = TRUE, flowr.flowr_version = "2.12.3"))
  n <- new.env(parent = emptyenv()); n$calls <- 0L
  testthat::local_mocked_bindings(
    .flowr_latest_version = function() { n$calls <- n$calls + 1L; "2.13.0" },
    .flowr_latest_adapter_version = function() NULL
  )
  flowr:::.flowr_updates()
  flowr:::.flowr_updates()
  expect_identical(n$calls, 1L)
  reset_updates()
})
