# bug report / feedback URL construction (non-interactive: no browser opens).

test_that("bug report builds a prefilled GitHub issue URL", {
  url <- suppressMessages(flowr_bug_report("boom happens on nested calls"))
  expect_match(url, "flowr-analysis/flowr-r-adapter/issues/new")
  expect_match(url, "labels=bug")
  expect_match(url, "boom")
})

test_that("feedback opens the shared flowR feedback form", {
  expect_match(suppressMessages(flowr_feedback()), "docs.google.com/forms")
})

test_that("report metadata includes versions and platform", {
  m <- flowr:::.flowr_report_meta()
  expect_match(m, "flowr \\(R adapter\\)")
  expect_match(m, "R:")
  expect_match(m, "platform:")
})
