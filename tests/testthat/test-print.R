# Slice printing: git-style diff, dimmed unused lines, plain code.
# Needs a real engine, so skipped on CRAN and when none is installed.

mk_slice <- function() {
  testthat::skip_on_cran()
  # The bundled engine counts: it runs flowR whenever Node.js >= 18 is present.
  if (!flowr_is_installed("binary") && !flowr_is_installed("node") &&
      !flowr_is_installed("bundled")) {
    testthat::skip("no flowR engine available (need a binary/node install, or Node.js for the bundle)")
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

test_that("the criterion token is underlined in every style that shows source", {
  sl <- mk_slice()
  for (style in c("diff", "gray")) {
    out <- capture.output(print(sl, style = style, color = TRUE))
    expect_true(any(grepl("\x1b\\[4mz\x1b\\[0m", out)), info = style)
  }
})

# The regression the dimming exists for: a line survives if any node touches it,
# so `a <- 1; b <- 2` is kept whole even though only the first statement slices.
test_that("a partly contributing line dims the part that is not in the slice", {
  testthat::skip_on_cran()
  if (!flowr_is_installed("binary") && !flowr_is_installed("node") &&
      !flowr_is_installed("bundled")) {
    testthat::skip("no flowR engine available")
  }
  flowr_connect()
  withr::defer(flowr_disconnect())
  sl <- slice("a <- 1; b <- 2\nprint(a)", "2@a")
  out <- capture.output(print(sl, style = "gray", color = TRUE))
  expect_true(any(grepl("^a <- 1\x1b\\[90m; b <- 2\x1b\\[0m$", out)))
  # ... and without colour there is nothing to dim with, so the line stays whole
  expect_true(any(grepl("^a <- 1; b <- 2$",
                        capture.output(print(sl, style = "gray", color = FALSE)))))
})

test_that("covered ranges follow node locations, including multi-line nodes", {
  lines <- c("a <- 1; b <- 2", "f <- function() {", "  1", "}")
  loc <- list("0" = list(1, 1, 1, 1), "1" = list(1, 6, 1, 6),
              "9" = list(2, 6, 4, 1))
  r <- .flowr_covered_ranges(c("0", "1"), loc, lines)
  expect_equal(r[["1"]], list(c(1, 1), c(6, 6)))
  expect_null(r[["2"]])
  # a node spanning lines covers the tail of its first line, all of the middle
  # ones, and the head of its last
  r2 <- .flowr_covered_ranges("9", loc, lines)
  expect_equal(r2[["2"]], list(c(6, nchar(lines[2]))))
  expect_equal(r2[["3"]], list(c(1, nchar(lines[3]))))
  expect_equal(r2[["4"]], list(c(1, 1)))
})

test_that("an id with no or a broken location is skipped, not fatal", {
  lines <- "a <- 1"
  expect_length(.flowr_covered_ranges("built-in:<-", list(), lines), 0)
  expect_length(.flowr_covered_ranges("7", list("7" = list(1, 1)), lines), 0)
  expect_length(.flowr_covered_ranges("7", list("7" = list("x", 1, 1, 1)), lines), 0)
})

test_that("whitespace-only gaps between contributing tokens are not dimmed", {
  # `a <- 1` covers columns 1, 3-4 and 6; the spaces between must not chop the
  # run into separate escape sequences
  m <- .flowr_contrib_mask("a <- 1; b <- 2", list(c(1, 1), c(3, 4), c(6, 6)))
  expect_true(all(m[1:6]))
  expect_false(any(m[7:14]))
  # leading indentation has no node, but dimming it would be noise
  expect_true(all(.flowr_contrib_mask("  x", list(c(3, 3)))))
  expect_equal(.flowr_contrib_mask("", list()), logical(0))
})

test_that("connector punctuation between contributing tokens is not dimmed", {
  # "print" (1-5) and "x" (7) are covered but the call's parens are not
  # themselves a node; a genuinely uncovered gap with real identifiers must
  # still stay dimmed, so covering both cases here
  m <- .flowr_contrib_mask("print(x)", list(c(1, 5), c(7, 7)))
  expect_true(all(m))
  m2 <- .flowr_contrib_mask("f(a, uncovered)", list(c(1, 3)))
  expect_false(any(m2[5:15]))
})

test_that("styling merges dimming and underline without shifting columns", {
  expect_equal(.flowr_style_line("abc", c(TRUE, TRUE, TRUE), list(), TRUE), "abc")
  expect_equal(.flowr_style_line("abc", rep(TRUE, 3), list(c(2, 2)), TRUE),
               "a\x1b[4mb\x1b[0mc")
  expect_equal(.flowr_style_line("abc", c(TRUE, FALSE, FALSE), list(), TRUE),
               "a\x1b[90mbc\x1b[0m")
  # a dimmed and underlined column carries both codes
  expect_equal(.flowr_style_line("ab", c(FALSE, TRUE), list(c(1, 2)), TRUE),
               "\x1b[90;4ma\x1b[0m\x1b[4mb\x1b[0m")
  expect_equal(.flowr_style_line("abc", c(TRUE, FALSE, TRUE), list(), FALSE), "abc")
})
