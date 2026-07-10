# Real-engine functionality. Skipped on CRAN and when no engine is installed.

skip_no_engine <- function() {
  testthat::skip_on_cran()
  # The bundled engine is real too (runs flowR when Node.js >= 18 is present);
  # only skip when nothing can actually run.
  if (!flowr_installed("binary") && !flowr_installed("node") &&
      !flowr_installed("bundled")) {
    testthat::skip("no flowR engine available (need a binary/node install, or Node.js for the bundle)")
  }
}

test_that("real engine slices with source line locations", {
  skip_no_engine()
  s <- flowr_connect()
  on.exit(flowr_disconnect(s), add = TRUE)
  sl <- slice("x <- 1\ny <- 2\nz <- x + 5\nprint(z)", "3@z")
  expect_true(all(c(1, 3) %in% sl$lines))
  expect_false(4 %in% sl$lines)
  expect_match(sl$code, "z <- x")
})

test_that("real engine runs a dataflow query and the repl", {
  skip_no_engine()
  s <- flowr_connect()
  on.exit(flowr_disconnect(s), add = TRUE)
  expect_gt(length(query("f <- function(x) x + 1\ng <- f(2)", "dataflow")$dataflow), 0)
  expect_gt(nchar(flowr_repl(":help")$output), 0)
})

test_that("multi-file folder slice returns a slice object", {
  skip_no_engine()
  d <- withr::local_tempdir()
  writeLines("helper <- function(x) x + 1", file.path(d, "helper.R"))
  writeLines("source('helper.R')\na <- 1\nb <- helper(a)\nprint(b)",
             file.path(d, "main.R"))
  s <- flowr_connect()
  on.exit(flowr_disconnect(s), add = TRUE)
  # Folder mode numbers all files together: helper.R is line 1, so `b` is line 4.
  sl <- slice(folder = d, criterion = "4@b")
  expect_s3_class(sl, "flowr_slice")
  # slice must reach b's definition (line 4) and the helper it needs (line 1)
  expect_true(all(c(1, 4) %in% sl$lines))
})

test_that("dataflow graph converts to vertices/edges and igraph", {
  skip_no_engine()
  on.exit(flowr_disconnect(), add = TRUE)
  g <- flowr_graph("x <- 1\ny <- x + 1\nplot(y)")
  expect_s3_class(g, "flowr_graph")
  expect_true(nrow(g$vertices) > 0 && nrow(g$edges) > 0)
  expect_setequal(unique(c(g$edges$from, g$edges$to)), g$vertices$id[g$vertices$id %in% c(g$edges$from, g$edges$to)])
  skip_if_not_installed("igraph")
  expect_s3_class(flowr_as_igraph(g), "igraph")
})

test_that("overview items carry criteria that plug into slice", {
  skip_no_engine()
  on.exit(flowr_disconnect(), add = TRUE)
  code <- "plot(1:10)\nhist(rnorm(20))\nx <- 5"
  ov <- flowr_overview(code)
  expect_s3_class(ov, "flowr_overview")
  expect_length(ov$visualize, 2)
  crit <- vapply(ov$visualize, function(v) v$criterion, character(1))
  expect_true(all(grepl("^[$]", crit)))
  expect_s3_class(slice(code, crit), "flowr_slice")
})

test_that("an explicit session parameter works alongside the default", {
  skip_no_engine()
  s <- flowr_connect()
  on.exit(flowr_disconnect(s), add = TRUE)
  expect_true(is_flowr_session(s))
  expect_s3_class(slice("x <- 1\ny <- x", "2@y", session = s), "flowr_slice")
})
