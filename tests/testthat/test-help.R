# flowr_help(): topic parsing, printing, and the signature-query wiring.

test_that("topic parsing splits pkg::fn, pairs with an explicit package, or is a bare function", {
  expect_identical(flowr:::.flowr_help_parse("dplyr::filter", NULL),
                   list(package = "dplyr", fn = "filter"))
  expect_identical(flowr:::.flowr_help_parse("filter", "dplyr"),
                   list(package = "dplyr", fn = "filter"))
  expect_identical(flowr:::.flowr_help_parse("filter", NULL),
                   list(package = NULL, fn = "filter"))
  expect_identical(flowr:::.flowr_help_parse(NULL, "dplyr"),
                   list(package = "dplyr", fn = NULL))
  # trailing "::" with nothing after it -> fn = NA, distinct from NULL
  expect_identical(flowr:::.flowr_help_parse("dplyr::", NULL),
                   list(package = "dplyr", fn = NA_character_))
})

test_that("flowr_help() rejects a topic ending in '::' instead of sending a malformed query", {
  expect_error(flowr_help("dplyr::"), "missing a function name")
})

test_that("glob detection matches flowR's own wildcard characters", {
  expect_true(flowr:::.flowr_has_glob("pivot_*"))
  expect_true(flowr:::.flowr_has_glob("fn?"))
  expect_false(flowr:::.flowr_has_glob("filter"))
  expect_false(flowr:::.flowr_has_glob(NULL))
})

test_that("the callable-signature preview renders defaults and bare names", {
  params <- list(list(name = "x", required = TRUE),
                 list(name = "y", default = "1"),
                 list(name = "..."))
  expect_identical(flowr:::.flowr_help_signature_str("f", params), "f(x, y = 1, ...)")
  expect_identical(flowr:::.flowr_help_signature_str("f", list()), "f()")
})

test_that("print.flowr_help renders every result kind without erroring", {
  fn_obj <- structure(list(kind = "function", query = list(package = "p", `function` = "f"),
                          result = list(`function` = list(
                            name = "f", package = "p", version = "1.0.0", exported = TRUE,
                            properties = list("deprecated"), s3generic = TRUE, s3methods = list("f.foo"),
                            parameters = list(list(name = "x", required = TRUE),
                                              list(name = "y", default = "1", forced = TRUE)),
                            file = "R/f.R", line = 3L, docUrl = "https://rdrr.io/r"))),
                     class = "flowr_help")
  expect_output(print(fn_obj), "p::f")
  expect_output(print(fn_obj), "deprecated")
  expect_output(print(fn_obj), "arguments")
  # `$s3method` must not partial-match the plural `$s3methods`
  expect_false(grepl("S3 method of", paste(capture.output(print(fn_obj)), collapse = "\n")))

  pkg_obj <- structure(list(kind = "package", query = list(package = "p"),
                           result = list(package = list(
                             name = "p", version = "1.0.0", base = FALSE, cran = TRUE,
                             exportsTotal = 5L, functionCount = 4L, deprecated = list(),
                             dependencies = list(list(name = "R")), cranPage = "https://cran.r-project.org/package=p"))),
                      class = "flowr_help")
  expect_output(print(pkg_obj), "p 1.0.0")

  matches_obj <- structure(list(kind = "matches", query = list(package = "*", `function` = "f"),
                               result = list(matches = list(
                                 list(package = "p1", name = "f", exported = TRUE),
                                 list(package = "p2", name = "f", exported = FALSE)))),
                          class = "flowr_help")
  out <- capture.output(print(matches_obj))
  expect_true(any(grepl("2 matches", out)))
  expect_true(any(grepl("p2::f", out) & grepl("not exported", out)))

  zero_obj <- structure(list(kind = "matches", query = list(`function` = "nope"),
                            result = list(matches = list())),
                       class = "flowr_help")
  expect_output(print(zero_obj), "no function named 'nope'")

  # zero matches from a parameter filter should say so, not imply unknown
  filtered_obj <- structure(list(kind = "matches",
                                query = list(`function` = "filter", parameters = I("bogus_arg")),
                                result = list(matches = list())),
                           class = "flowr_help")
  out <- capture.output(print(filtered_obj))
  expect_true(any(grepl("matches parameter bogus_arg", out)))
  expect_false(any(grepl("found in the signature database", out)))

  nf_obj <- structure(list(kind = "not_found", query = list(package = "nope"),
                          result = list(message = "The signature database does not know the package 'nope'.",
                                       suggestions = list("nopeee"))),
                     class = "flowr_help")
  expect_output(print(nf_obj), "does not know the package")
  expect_output(print(nf_obj), "nopeee")

  summary_obj <- structure(list(kind = "summary", query = list(),
                               result = list(packageCount = 10L, sourceCount = 1L,
                                            databases = list(list(scope = "current", version = 1, date = "2026-01-01")))),
                          class = "flowr_help")
  expect_output(print(summary_obj), "10 packages")
})

test_that("flowr_help() errors when no topic or package is given", {
  expect_error(flowr_help(), "topic")
})

test_that("flowr_help() against a mock server resolves a lone bare-topic match and errors with no sigdb", {
  srv <- mock_server_start()
  on.exit(mock_server_stop(srv), add = TRUE)
  s <- flowr:::.flowr_connect_to("127.0.0.1", srv$port)
  on.exit(flowr_disconnect(s), add = TRUE)

  h <- flowr_help("mockfn", session = s)
  expect_s3_class(h, "flowr_help")
  expect_identical(h$kind, "function")
  expect_identical(h$result[["function"]]$package, "mockpkg")

  expect_error(flowr_help("no_sigdb_here_xyz", package = "unknownpkg", session = s), "signature database")

  # `res$package` used to partial-match `packageCount` and crash
  h2 <- flowr_help(package = "totally_unknown_pkg", session = s)
  expect_identical(h2$kind, "not_found")
  expect_output(print(h2), 'does not know the package "totally_unknown_pkg"')
})
