test_that("find package directory", {
  expect_no_error(get_default_node_base_dir())
})
test_that("install node", {
  options(timeout = 300)
  expect_no_error(install_node("22.9.0", TRUE))
})
test_that("install flowr", {
  expect_equal(install_flowr("2.0.11", TRUE), 0)
})
