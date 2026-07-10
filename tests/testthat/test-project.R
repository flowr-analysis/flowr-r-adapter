# Project-root auto-detection for the folder-default commands (no engine).

test_that("a DESCRIPTION marks a project root, found from a nested folder", {
  root <- withr::local_tempdir()
  writeLines(c("Package: demo", "Version: 0.1"), file.path(root, "DESCRIPTION"))
  nested <- file.path(root, "R", "sub")
  dir.create(nested, recursive = TRUE)
  writeLines("x <- 1", file.path(nested, "a.R"))
  expect_identical(normalizePath(flowr:::.flowr_project_root(nested)),
                   normalizePath(root))
  expect_match(flowr:::.flowr_root_label(root), "package 'demo'")
})

test_that("the outermost R-project marker wins when projects are nested", {
  outer <- withr::local_tempdir()
  writeLines(c("Package: outer", "Version: 0.1"), file.path(outer, "DESCRIPTION"))
  inner <- file.path(outer, "inner")
  dir.create(inner)
  writeLines(c("Package: inner", "Version: 0.1"), file.path(inner, "DESCRIPTION"))
  expect_identical(normalizePath(flowr:::.flowr_project_root(inner)),
                   normalizePath(outer))
})

test_that("a plain folder of R files is a root; an empty one is not", {
  d <- withr::local_tempdir()
  writeLines("y <- 2", file.path(d, "s.R"))
  expect_identical(normalizePath(flowr:::.flowr_project_root(d)), normalizePath(d))

  empty <- withr::local_tempdir()
  expect_null(flowr:::.flowr_project_root(empty))
})

test_that("no source and no marker errors instead of guessing", {
  empty <- withr::local_tempdir()
  old <- setwd(empty); on.exit(setwd(old), add = TRUE)
  expect_error(flowr:::.flowr_default_source(NULL, NULL, NULL), "inside an R project")
})
