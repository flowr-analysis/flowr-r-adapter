test_that("run flowr (docker)", {
  skip_on_os("windows")
  skip_on_os("mac")

  expect_output(exec_flowr_docker(c(), "2.0.11", c("--version"), TRUE), paste0("flowR: ", "2.0.11"))
})
