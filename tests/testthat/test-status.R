# Status, debug toggle and JSON config file.

test_that("a JSON config file is resolved and applied", {
  dir <- withr::local_tempdir()
  cfg <- file.path(dir, "flowr.json")
  writeLines('{"engine":"node","flowr_version":"9.9.9","secure":false}', cfg)
  withr::local_options(list(flowr.config_file = cfg, flowr.engine = NULL))
  withr::local_envvar(c(FLOWR_ENGINE = NA, FLOWR_FLOWR_VERSION = NA, FLOWR_SECURE = NA))

  expect_identical(flowr_config_file(), cfg)
  expect_identical(flowr:::flowr_option("engine"), "node")        # from JSON
  expect_identical(flowr:::flowr_option("flowr_version"), "9.9.9")
  expect_false(flowr:::flowr_option("secure"))
})

test_that("options and env vars take precedence over the JSON config", {
  dir <- withr::local_tempdir()
  cfg <- file.path(dir, "flowr.json")
  writeLines('{"engine":"node"}', cfg)

  withr::local_options(list(flowr.config_file = cfg, flowr.engine = "node"))
  expect_identical(flowr:::flowr_option("engine"), "node")     # option beats JSON

  withr::local_options(list(flowr.config_file = cfg, flowr.engine = NULL))
  withr::local_envvar(c(FLOWR_ENGINE = "docker"))
  expect_identical(flowr:::flowr_option("engine"), "docker")      # env beats JSON
})

test_that("no config file resolves to NA", {
  withr::local_options(list(flowr.config_file = NULL))
  withr::local_envvar(c(FLOWR_CONFIG = NA))
  withr::local_dir(withr::local_tempdir())
  expect_true(is.na(flowr_config_file()))
})

test_that("flowr_status returns a printable structured report", {
  st <- flowr_status()
  expect_s3_class(st, "flowr_status")
  expect_true(all(c("version", "config", "engines", "cache_dir", "session") %in% names(st)))
  expect_output(print(st), "flowr")
  expect_output(print(st), "engine")
})

test_that("an available update names the command that actually gets it", {
  # flowr is not on CRAN, so "update the flowr package" was not actionable:
  # update.packages() does nothing for it and the install path is not guessable
  hint <- flowr:::.flowr_update_hint("flowr", "0.3.0")
  expect_match(paste(hint, collapse = " "), "remotes::install_github", fixed = TRUE)
  expect_match(paste(hint, collapse = " "), "flowr-analysis/flowr-r-adapter",
               fixed = TRUE)
  # each printed line stays inside a standard 80-column terminal (16 of prefix)
  expect_true(all(nchar(hint) + 16L <= 80L))

  # a newer flowR engine is a different command
  expect_identical(flowr:::.flowr_update_hint("flowR", "2.13.3"),
                   "flowr_update(\"2.13.3\")")
})

test_that("print.flowr_status shows the update command, not just the advice", {
  withr::local_options(list(flowr.check_updates = FALSE, flowr.color = FALSE))
  st <- flowr_status()
  st$updates <- list(flowr = c("0.2.9", "0.3.0"))
  out <- paste(capture.output(print(st)), collapse = "\n")
  expect_match(out, "flowr 0.2.9 -> 0.3.0 available", fixed = TRUE)
  expect_match(out, "remotes::install_github", fixed = TRUE)
  expect_false(grepl("update the flowr package to get it", out, fixed = TRUE))
})

test_that("the reported flowR version links to its release notes", {
  expect_identical(flowr:::.flowr_flowr_release_url("2.13.3"),
                   "https://github.com/flowr-analysis/flowr/releases/tag/v2.13.3")
  # only for something that is actually a version
  expect_null(flowr:::.flowr_flowr_release_url("?"))
  expect_null(flowr:::.flowr_flowr_release_url(NA_character_))

  withr::local_options(list(flowr.hyperlinks = TRUE))
  linked <- flowr:::.flowr_release_link("2.13.3", TRUE)
  expect_match(linked, "releases/tag/v2.13.3")
  expect_match(linked, "\033]8;;")                    # an OSC 8 hyperlink

  # never advertise a link the terminal cannot follow, or when colour is off
  expect_identical(flowr:::.flowr_release_link("2.13.3", FALSE), "2.13.3")
  withr::local_options(list(flowr.hyperlinks = FALSE))
  expect_identical(flowr:::.flowr_release_link("2.13.3", TRUE), "2.13.3")
})
