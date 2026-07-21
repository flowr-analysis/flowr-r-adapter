# End-to-end functionality of the real protocol client, driven against an
# in-R mock flowR server (no Node/Docker/network). Skipped on CRAN.

test_that("client connects and performs slice, query, dependencies and repl", {
  srv <- mock_server_start()
  on.exit(mock_server_stop(srv), add = TRUE)

  s <- flowr:::.flowr_connect_to("127.0.0.1", srv$port)
  on.exit(flowr_disconnect(s), add = TRUE)

  expect_true(is_flowr_session(s))
  expect_identical(s$versions$flowr, "mock")

  sl <- slice("a <- 1", "1@a")
  expect_s3_class(sl, "flowr_slice")
  expect_identical(sl$code, "x <- 1")             # mock's canned reconstruction

  q <- query("a <- 1", "dependencies")
  expect_identical(q$dependencies$library[[1]]$value, "dplyr")

  r <- flowr_repl(":x")
  expect_s3_class(r, "flowr_repl")
  expect_identical(r$output, "hello world")       # two streamed frames joined
})

test_that("server error responses surface as R errors", {
  srv <- mock_server_start()
  on.exit(mock_server_stop(srv), add = TRUE)
  s <- flowr:::.flowr_connect_to("127.0.0.1", srv$port)
  on.exit(flowr_disconnect(s), add = TRUE)
  expect_error(flowr:::flowr_analyze(code = "ERR"), "boom")
})

test_that("empty code is rejected before it reaches flowR", {
  srv <- mock_server_start()
  on.exit(mock_server_stop(srv), add = TRUE)
  s <- flowr:::.flowr_connect_to("127.0.0.1", srv$port)
  on.exit(flowr_disconnect(s), add = TRUE)
  expect_error(flowr:::flowr_analyze(code = ""), "must not be empty")
})

test_that("repeated ops reuse one cached analysis", {
  srv <- mock_server_start()
  on.exit(mock_server_stop(srv), add = TRUE)
  s <- flowr:::.flowr_connect_to("127.0.0.1", srv$port)
  on.exit(flowr_disconnect(s), add = TRUE)
  a1 <- flowr:::flowr_analyze(code = "a <- 1")
  a2 <- flowr:::flowr_analyze(code = "a <- 1")
  expect_identical(a1$filetoken, a2$filetoken)    # same content -> same token
})
