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
      "line", "linter", "certainty", "fixable"))
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

test_that("quick fixes are spliced correctly (replace, remove, no-op, overlap)", {
  apply <- flowr:::.flowr_apply_fixes_to_text
  r <- apply("foo_bar <- 1", list(list(sl = 1, sc = 1, el = 1, ec = 7, replacement = "fooBar")))
  expect_identical(r$text, "fooBar <- 1")
  expect_equal(r$applied, 1)
  r <- apply("a\nunused <- 2\nb", list(list(sl = 2, sc = 1, el = 2, ec = 11, replacement = "")))
  expect_identical(r$text, "a\n\nb")
  r <- apply("x <- 1", list(list(sl = 1, sc = 1, el = 1, ec = 1, replacement = "x")))
  expect_equal(r$applied, 0)                                     # no-op not counted
  r <- apply("name <- 1", list(
    list(sl = 1, sc = 1, el = 1, ec = 9, replacement = ""),      # remove all
    list(sl = 1, sc = 1, el = 1, ec = 4, replacement = "nm")))   # overlaps -> skipped
  expect_equal(r$applied, 1)
})

test_that(".flowr_collect_fixes pulls fixes out of a raw linter result", {
  results <- list(
    `unused-definitions` = list(results = list(
      list(loc = list(2, 1, 2, 11, "a.R"),
           quickFix = list(list(type = "remove", loc = list(2, 1, 2, 11, "a.R"),
                                description = "remove"))))),
    `naming-convention` = list(results = list(
      list(loc = list(3, 1, 3, 7, "a.R"),
           quickFix = list(list(type = "replace", replacement = "fooBar",
                                loc = list(3, 1, 3, 7, "a.R"), description = "rename"))))))
  fx <- flowr:::.flowr_collect_fixes(results)
  expect_length(fx, 2)
  expect_setequal(vapply(fx, function(f) f$file, ""), "a.R")
  expect_true(any(vapply(fx, function(f) f$replacement == "fooBar", logical(1))))
})

test_that("flowr_lint() end-to-end returns findings when an engine is available", {
  skip_if_not(flowr_is_installed("binary") || flowr_is_installed("node") ||
              flowr_is_installed("bundled"),
              "no flowR engine available")
  withr::local_options(list(flowr.quiet = TRUE))
  res <- flowr_lint("setwd('/abs/path')\nunused_var <- 1")
  expect_s3_class(res, "flowr_lints")
  expect_true("absolute-file-paths" %in% res$linter)
  expect_true("fixable" %in% names(res))
  # the same findings, in jarl's machine-readable form
  gh <- flowr_lint("setwd('/abs/path')", format = "github")
  expect_true(any(grepl("absolute-file-paths", gh)))
})

test_that("flowr_lint() rules can be selected per call and via config", {
  skip_if_not(flowr_is_installed("binary") || flowr_is_installed("node") ||
              flowr_is_installed("bundled"),
              "no flowR engine available")
  withr::local_options(list(flowr.quiet = TRUE))
  code <- "setwd('/x')\nunused_var <- 1"

  # per-call selection: only the requested rule appears
  expect_identical(unique(flowr_lint(code, rules = "absolute-file-paths")$linter),
                   "absolute-file-paths")

  # a configured default applies when no rules are passed, and a per-call value wins
  withr::local_options(list(flowr.lint_rules = "unused-definitions"))
  expect_identical(unique(flowr_lint(code)$linter), "unused-definitions")
  expect_identical(unique(flowr_lint(code, rules = "absolute-file-paths")$linter),
                   "absolute-file-paths")
})

test_that("flowr_lint_fix() applies flowR's real quick fixes end-to-end", {
  skip_if_not(flowr_is_installed("binary") || flowr_is_installed("node") ||
              flowr_is_installed("bundled"),
              "no flowR engine available")
  withr::local_options(list(flowr.quiet = TRUE))

  # an unused definition is one flowR attaches a (removal) quick fix to
  code <- "x <- 1\ndead_var <- 99\ncat(x)"
  expect_true(any(flowr_lint(code)$fixable))

  # inline: the fixed code no longer defines the unused variable
  fixed <- flowr_lint_fix(code)
  expect_type(fixed, "character")
  expect_false(grepl("dead_var", fixed))

  # file: preview returns the fixed text and does NOT touch the file
  tf <- withr::local_tempfile(fileext = ".R")
  writeLines(c("x <- 1", "dead_var <- 99", "cat(x)"), tf)
  prev <- flowr_lint_fix(file = tf, preview = TRUE)
  expect_false(grepl("dead_var", paste(prev, collapse = "\n")))
  expect_true(any(grepl("dead_var", readLines(tf))))     # unchanged on disk

  # file: a real run rewrites it in place
  applied <- flowr_lint_fix(file = tf)
  expect_gt(sum(applied), 0)
  expect_false(any(grepl("dead_var", readLines(tf))))
})
