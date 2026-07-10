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
