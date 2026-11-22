# Real-engine functionality. Skipped on CRAN and when no engine is installed.

skip_no_engine <- function() {
  testthat::skip_on_cran()
  # The bundled engine is real too (runs flowR when Node.js >= 18 is present);
  # only skip when nothing can actually run.
  if (!flowr_is_installed("binary") && !flowr_is_installed("node") &&
      !flowr_is_installed("bundled")) {
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

test_that("the linter rules new in flowR 2.17 run and read well", {
  skip_no_engine()
  s <- flowr_connect()
  on.exit(flowr_disconnect(s), add = TRUE)
  code <- paste(
    "key <- Sys.getenv('API_KEY')",
    "message('calling with ', key)",
    "View(mtcars)",
    "combn(4, -1)",
    sep = "\n")
  rules <- c("secret-exfiltration", "interactive-functions", "call-contracts")
  l <- suppressMessages(flowr_lint(code, rules = rules))
  expect_setequal(unique(l$linter), rules)
  expect_match(l$message[l$linter == "secret-exfiltration"], "reaches the logs via `message`")
  expect_match(l$message[l$linter == "interactive-functions"], "View` only works")
  expect_match(l$message[l$linter == "call-contracts"], "call of `combn` fails its contract: ")
})

test_that("the queries new in flowR 2.17 answer through query()", {
  skip_no_engine()
  s <- flowr_connect()
  on.exit(flowr_disconnect(s), add = TRUE)
  clones <- query("f <- function(v) {\n  m <- mean(v)\n  (v - m) / sd(v)\n}\ng <- function(w) {\n  u <- mean(w)\n  (w - u) / sd(w)\n}",
                  list(type = "code-clones", algorithms = I("gst")))
  expect_gte(length(clones[["code-clones"]]$pairs), 1L)
  amb <- query("options(digits = 3)\nSys.setenv(A = '1')", "ambient-access")
  writes <- unlist(lapply(amb[["ambient-access"]]$results, `[[`, "writes"))
  expect_true(all(c("#option:digits", "#env:A") %in% writes))
  guards <- query("f <- function(x) { if (!is.numeric(x)) stop('no'); x }", "inspect-fn-guards")
  expect_gte(length(guards[["inspect-fn-guards"]]$guards), 1L)
  taint <- query("x <- c(1, 2, 3)\ny <- scale(x)\nz <- mean(y)",
                 list(type = "taint", defs = I("normalization-constant")))
  expect_false(is.null(taint$taint$results))
})
