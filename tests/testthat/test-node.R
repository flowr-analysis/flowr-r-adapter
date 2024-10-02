test_that("find package directory", {
  expect_no_error(get_default_node_base_dir())
})
test_that("install node", {
  options(timeout = 300)
  expect_no_error(install_node("22.5.1"))
})
test_that("install flowr", {
  flowr_version <- "2.0.11"
  expect_equal(install_flowr(flowr_version), 0)

  # test if the installation was actually successful
  pid <- exec_flowr("--server", background = TRUE)
  on.exit(tools::pskill(pid), add = TRUE, after = FALSE)
  conn_hello <- connect()
  on.exit(flowr::disconnect(conn_hello[[1]]), add = TRUE, after = FALSE)
  response <- jsonlite::fromJSON(conn_hello[[2]])
  expect_equal(response$versions$flowr, flowr_version)
})
