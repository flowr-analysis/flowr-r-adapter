# flowr_guess_versions(): building the tidy result and the guess-dep-versions
# query wiring.

# the per-session "already asked" flag is cached; each test starts fresh
reset_history_ask <- function() {
  st <- flowr:::.flowr_state
  st$history_sigdb_asked <- NULL
  invisible(NULL)
}

test_that("history sigdb already installed: no message, no prompt", {
  reset_history_ask()
  testthat::local_mocked_bindings(
    .flowr_sigdb_scopes_installed = function(version) c("current", "history"),
    .flowr_interactive = function() stop("should not check interactivity")
  )
  expect_no_message(flowr:::.flowr_guess_check_history("9.9.9"))
})

test_that("history sigdb missing, non-interactive: one message, no prompt", {
  reset_history_ask()
  withr::local_options(list(flowr.quiet = FALSE))
  testthat::local_mocked_bindings(
    .flowr_sigdb_scopes_installed = function(version) "current",
    .flowr_interactive = function() FALSE,
    .flowr_readline = function(prompt) stop("should not prompt")
  )
  expect_message(flowr:::.flowr_guess_check_history("9.9.9"), "history")
  expect_true(flowr:::.flowr_state$history_sigdb_asked)
})

test_that("history sigdb missing, interactive: declining installs nothing", {
  reset_history_ask()
  installed <- FALSE
  testthat::local_mocked_bindings(
    .flowr_sigdb_scopes_installed = function(version) character(0),
    .flowr_interactive = function() TRUE,
    .flowr_readline = function(prompt) "n",
    .flowr_ensure_sigdb = function(...) installed <<- TRUE
  )
  expect_message(flowr:::.flowr_guess_check_history("9.9.9"), "none installed")
  expect_false(installed)
})

test_that("history sigdb missing, interactive: accepting installs it", {
  reset_history_ask()
  seen <- NULL
  testthat::local_mocked_bindings(
    .flowr_sigdb_scopes_installed = function(version) "current",
    .flowr_interactive = function() TRUE,
    .flowr_readline = function(prompt) "y",
    .flowr_ensure_sigdb = function(version, quiet, scopes) seen <<- scopes
  )
  suppressMessages(flowr:::.flowr_guess_check_history("9.9.9"))
  expect_identical(seen, "history")
})

test_that("asked at most once per session", {
  reset_history_ask()
  n <- 0L
  testthat::local_mocked_bindings(
    .flowr_sigdb_scopes_installed = function(version) character(0),
    .flowr_interactive = function() { n <<- n + 1L; TRUE },
    .flowr_readline = function(prompt) "n"
  )
  suppressMessages(flowr:::.flowr_guess_check_history("9.9.9"))
  suppressMessages(flowr:::.flowr_guess_check_history("9.9.9"))
  expect_identical(n, 1L)
})

test_that("disabling `signature` evidence skips the history check entirely", {
  reset_history_ask()
  srv <- mock_server_start()
  on.exit(mock_server_stop(srv), add = TRUE)
  s <- flowr:::.flowr_connect_to("127.0.0.1", srv$port)
  on.exit(flowr_disconnect(s), add = TRUE)
  testthat::local_mocked_bindings(
    .flowr_sigdb_scopes_installed = function(version) character(0),
    .flowr_interactive = function() FALSE
  )
  msgs <- testthat::capture_messages(
    flowr_guess_versions(code = "1", disable_evidence = "signature", session = s))
  expect_false(any(grepl("history", msgs)))
  expect_null(flowr:::.flowr_state$history_sigdb_asked)
})

.guess_raw <- function() {
  list(
    dependencies = list(
      list(package = "base", base = TRUE, used = TRUE, range = ">=4.0.0 <=4.5.3",
           minVersion = "4.0.0", maxVersion = "4.5.3", candidateCount = 30L, totalVersions = 30L,
           candidates = list("4.0.0", "4.5.3"), truncated = TRUE),
      list(package = "dplyr", base = FALSE, used = FALSE, range = "1.1.4",
           minVersion = "1.1.4", maxVersion = "1.1.4", candidateCount = 1L, totalVersions = 40L,
           declaredConstraints = list(">=1.0.0"), linkedWith = list("tibble"),
           evidence = list(list(source = "declared", origin = "DESCRIPTION", bound = ">=1.0.0"),
                          list(source = "signature", origin = "lead", bound = ">=1.1.4",
                               `function` = "lead", parameter = "n"))),
      list(package = "impossible", base = FALSE, used = TRUE, range = "d>=1.0.0 d<=0.9.0",
           candidateCount = 0L, totalVersions = 5L, unsatisfiable = TRUE, evidence = list())
    ),
    dateCutoff = "2024-01-01", rVersion = "4.5.3", versionSelection = "newest",
    runnableCombinations = 3, possibleCombinations = 30,
    linkedGroups = list(list("dplyr", "tibble")),
    assignments = list(list(versions = list(dplyr = "1.1.4")))
  )
}

test_that("the tidy dependencies data frame carries scalar and list-columns correctly", {
  g <- structure(flowr:::.flowr_guess_build(.guess_raw()), class = "flowr_guess")
  df <- g$dependencies
  expect_s3_class(df, "data.frame")
  expect_identical(df$package, c("base", "dplyr", "impossible"))
  expect_identical(df$base, c(TRUE, FALSE, FALSE))
  expect_identical(df$used, c(TRUE, FALSE, TRUE))
  expect_identical(df$unsatisfiable, c(NA, NA, TRUE))
  expect_identical(df$candidates[[1]], c("4.0.0", "4.5.3"))
  expect_identical(df$linked_with[[2]], "tibble")
  expect_null(df$linked_with[[1]])
  expect_length(df$evidence[[2]], 2L)
  expect_identical(g$date_cutoff, "2024-01-01")
  expect_identical(g$r_version, "4.5.3")
  expect_identical(g$linked_groups[[1]], c("dplyr", "tibble"))
  expect_identical(g$assignments[[1]], c(dplyr = "1.1.4"))
})

test_that("an empty dependency list builds a zero-row frame, not an error", {
  g <- flowr:::.flowr_guess_build(list(dependencies = list()))
  expect_equal(nrow(g$dependencies), 0L)
  expect_true(is.na(g$r_version))
})

test_that("a missing signature database surfaces as $message with no dependencies", {
  g <- structure(flowr:::.flowr_guess_build(list(
    dependencies = list(),
    message = "No signature database is loaded; version guessing needs the signature database."
  )), class = "flowr_guess")
  expect_match(g$message, "No signature database")
  expect_output(print(g), "No signature database")
})

test_that("print.flowr_guess renders the header, sample and evidence tally", {
  g <- structure(flowr:::.flowr_guess_build(.guess_raw()), class = "flowr_guess")
  out <- capture.output(print(g))
  expect_true(any(grepl("3 dependencies", out)))
  expect_true(any(grepl("runnable combinations: 3 / 30", out)))
  expect_true(any(grepl("linked: dplyr \\+ tibble", out)))
  expect_true(any(grepl("^  dplyr .*\\[linked: tibble\\]", out)))
  expect_true(any(grepl("unsatisfiable", out)))
  expect_true(any(grepl("assignments: 1", out)))
})

test_that("print.flowr_guess flags zero candidates even when flowR left `unsatisfiable` unset", {
  # flowR doesn't always set `unsatisfiable` (e.g. a `date` cutoff), but 0
  # candidates used to print as a plain, healthy-looking range regardless
  raw <- list(dependencies = list(
    list(package = "dplyr", base = FALSE, used = TRUE, range = "*",
         candidateCount = 0L, totalVersions = 1L, evidence = list())
  ))
  g <- structure(flowr:::.flowr_guess_build(raw), class = "flowr_guess")
  expect_true(is.na(g$dependencies$unsatisfiable[1]))
  out <- capture.output(print(g))
  expect_true(any(grepl("no version satisfies", out)))
})

test_that("explode_prefer must be a named vector", {
  expect_error(flowr_guess_versions(code = "1", explode = TRUE,
                                    explode_prefer = c("1.1.4")), "named")
})

test_that("disable_evidence rejects an unknown evidence source", {
  expect_error(flowr_guess_versions(code = "1", disable_evidence = "bogus"),
              "unknown evidence source")
})

test_that("flowr_guess_versions() against a mock server builds the right result", {
  srv <- mock_server_start()
  on.exit(mock_server_stop(srv), add = TRUE)
  s <- flowr:::.flowr_connect_to("127.0.0.1", srv$port)
  on.exit(flowr_disconnect(s), add = TRUE)

  g <- flowr_guess_versions(code = "library(mockpkg)", session = s)
  expect_s3_class(g, "flowr_guess")
  expect_identical(g$dependencies$package, "mockpkg")
  expect_identical(g$dependencies$range, "1.0.0")
  expect_identical(g$r_version, "4.5.0")
})
