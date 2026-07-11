# Linter tests. The flatten + render logic is pure (it operates on a parsed
# flowR `linter` result), so most of it is tested here with a synthetic result
# and needs no running engine. One end-to-end test is engine-gated.

# A stand-in for what flowR's `linter` query returns (results keyed by rule,
# each finding carrying `loc` and rule-specific fields), matching the real
# shapes: a 5-element loc [sl, sc, el, ec, path] for located findings and a
# project-level finding with a negative loc and its own message.
mock_linter_results <- function() {
  list(
    `absolute-file-paths` = list(results = list(
      list(certainty = "certain", filePath = "/x", loc = list(3, 1, 3, 25, "a.R"))),
      .meta = list()),
    `unused-definitions` = list(results = list(
      list(certainty = "uncertain", variableName = "y", loc = list(4, 1, 4, 1, "a.R"))),
      .meta = list()),
    `naming-convention` = list(results = list(
      list(certainty = "certain", name = "foo_bar", detectedCasing = "snake_case",
           expectedCasing = "camelCase", loc = list(5, 1, 5, 7, "a.R"))),
      .meta = list()),
    `software-has-license` = list(results = list(
      list(certainty = "certain", loc = list(-1, -1, -1, -1),
           message = "No license found in the project")),
      .meta = list())
  )
}

test_that("linter findings flatten to lintr-compatible columns", {
  rows <- flowr:::.flowr_lint_rows(mock_linter_results(), code = NULL, file = "a.R")
  # the column names lintr's as.data.frame(lints) uses, plus flowR's certainty
  expect_identical(names(rows),
    c("filename", "line_number", "column_number", "type", "message",
      "line", "linter", "certainty"))
  expect_equal(nrow(rows), 4)
  expect_type(rows$line_number, "integer")
  expect_true(all(rows$filename == "a.R"))
})

test_that("severity maps to lintr types and positions are carried through", {
  rows <- flowr:::.flowr_lint_rows(mock_linter_results(), code = NULL, file = "a.R")
  by <- function(rule) rows[rows$linter == rule, , drop = FALSE]
  expect_identical(by("absolute-file-paths")$type, "warning")
  expect_identical(by("naming-convention")$type, "style")   # stylistic rule
  expect_identical(by("absolute-file-paths")$line_number, 3L)
  # a project-level finding (loc all -1) has no position
  expect_true(is.na(by("software-has-license")$line_number))
})

test_that("messages are human-readable (synthesised or passed through)", {
  rows <- flowr:::.flowr_lint_rows(mock_linter_results(), code = NULL, file = "a.R")
  msg <- function(rule) rows[rows$linter == rule, "message"]
  expect_match(msg("absolute-file-paths"), "absolute file path")
  expect_match(msg("unused-definitions"), "never used")
  expect_match(msg("naming-convention"), "camelCase")
  expect_identical(msg("software-has-license"), "No license found in the project")
})

test_that("empty results give a zero-row frame with the right columns", {
  rows <- flowr:::.flowr_lint_rows(list(), code = NULL, file = NULL)
  expect_equal(nrow(rows), 0)
  expect_true(all(c("filename", "line_number", "type", "message", "linter") %in% names(rows)))
})

test_that("github format emits one annotation command per finding", {
  rows <- flowr:::.flowr_lint_rows(mock_linter_results(), code = NULL, file = "a.R")
  gh <- flowr:::.flowr_lint_render(rows, "github")
  expect_length(gh, 4)
  expect_true(any(grepl("^::warning ", gh)))   # a defect
  expect_true(any(grepl("^::notice ", gh)))    # a stylistic finding
  expect_true(all(grepl("file=a.R", gh)))
})

test_that("sarif format is valid SARIF 2.1.0 with one result per finding", {
  rows <- flowr:::.flowr_lint_rows(mock_linter_results(), code = NULL, file = "a.R")
  sarif <- jsonlite::fromJSON(flowr:::.flowr_lint_render(rows, "sarif"),
                              simplifyVector = FALSE)
  expect_identical(sarif$version, "2.1.0")
  expect_identical(sarif$runs[[1]]$tool$driver$name, "flowr")
  expect_equal(length(sarif$runs[[1]]$results), 4)
  expect_true(all(vapply(sarif$runs[[1]]$results,
                         function(r) !is.null(r$ruleId), logical(1))))
})

test_that("jarl format matches jarl's native JSON shape", {
  rows <- flowr:::.flowr_lint_rows(mock_linter_results(), code = NULL, file = "a.R")
  jarl <- jsonlite::fromJSON(flowr:::.flowr_lint_render(rows, "jarl"),
                             simplifyVector = FALSE)
  expect_true(all(c("diagnostics", "errors") %in% names(jarl)))
  expect_equal(length(jarl$diagnostics), 4)
  d <- jarl$diagnostics[[1]]
  expect_true(all(c("message", "filename", "location") %in% names(d)))
  expect_true(all(c("name", "body", "suggestion") %in% names(d$message)))
})

test_that("lint() end-to-end returns findings when an engine is available", {
  skip_if_not(flowr_is_installed("binary") || flowr_is_installed("node") ||
              flowr_is_installed("bundled"),
              "no flowR engine available")
  withr::local_options(list(flowr.quiet = TRUE))
  res <- lint("setwd('/abs/path')\nunused_var <- 1")
  expect_s3_class(res, "flowr_lints")
  expect_true("absolute-file-paths" %in% res$linter)
  # the same findings, in jarl's machine-readable form
  gh <- lint("setwd('/abs/path')", format = "github")
  expect_true(any(grepl("absolute-file-paths", gh)))
})
