# Slice printing: git-style diff, dimmed unused lines, plain code.
# Needs a real engine, so skipped on CRAN and when none is installed.

mk_slice <- function() {
  testthat::skip_on_cran()
  if (!flowr_installed("binary") && !flowr_installed("node")) {
    testthat::skip("no real flowR engine installed")
  }
  flowr_connect()
  withr::defer(flowr_disconnect(), envir = parent.frame())
  slice("x <- 1\ny <- 2\nz <- x + 5\nprint(z)", "3@z")
}

test_that("a slice records the original source and its covered lines", {
  sl <- mk_slice()
  expect_true(all(c(1, 3) %in% sl$lines))
  expect_false(4 %in% sl$lines)
})

test_that("diff style marks removed lines and keeps context", {
  out <- capture.output(print(mk_slice(), style = "diff", color = FALSE))
  expect_true(any(grepl("^- y <- 2", out)))
  expect_true(any(grepl("^  x <- 1", out)))
})

test_that("gray style keeps every line and code style shows the slice", {
  sl <- mk_slice()
  expect_length(capture.output(print(sl, style = "gray", color = FALSE)), 5)
  expect_true(any(grepl("z <- x \\+ 5",
                        capture.output(print(sl, style = "code", color = FALSE)))))
})

test_that("colour is emitted only when requested", {
  sl <- mk_slice()
  expect_false(any(grepl("\x1b\\[", capture.output(print(sl, color = FALSE)))))
  expect_true(any(grepl("\x1b\\[", capture.output(print(sl, color = TRUE)))))
})
