# Resource-leak tests: sockets, internal registries, the analysis cache and
# spawned processes must all be released.

test_that("closing a session frees its socket and read buffer", {
  srv <- mock_server_start()
  on.exit(mock_server_stop(srv), add = TRUE)

  flowr_disconnect()
  base_conn <- nrow(showConnections())
  base_readers <- length(ls(flowr:::.flowr_reader_registry()))

  s <- flowr:::.flowr_connect_to("127.0.0.1", srv$port)
  expect_gt(nrow(showConnections()), base_conn)          # socket opened

  flowr_disconnect(s)
  expect_identical(nrow(showConnections()), base_conn)   # socket closed
  expect_identical(length(ls(flowr:::.flowr_reader_registry())), base_readers)
  expect_true(s$closed)
})

test_that("many connect/disconnect cycles do not accumulate connections", {
  srv <- mock_server_start()
  on.exit(mock_server_stop(srv), add = TRUE)
  flowr_disconnect()
  base <- nrow(showConnections())
  for (i in seq_len(8)) {
    s <- flowr:::.flowr_connect_to("127.0.0.1", srv$port)
    flowr_disconnect(s)
  }
  expect_identical(nrow(showConnections()), base)
})

test_that("the per-session analysis cache is bounded", {
  withr::local_options(list(flowr.cache_size = 4L))
  session <- new.env(parent = emptyenv())
  session$cache <- new.env(parent = emptyenv())
  session$cache_order <- character(0)
  for (i in seq_len(20)) {
    flowr:::.flowr_cache_put(session, paste0("k", i), list(x = i))
  }
  expect_lte(length(session$cache_order), 4L)
  expect_lte(length(ls(session$cache)), 4L)
  expect_true("k20" %in% session$cache_order)   # newest kept
  expect_false("k1" %in% session$cache_order)    # oldest evicted
})

test_that("stopping an owned engine kills its process", {
  testthat::skip_on_cran()
  testthat::skip_on_os("windows")
  pid <- sys::exec_background("sleep", "60")
  handle <- flowr:::.flowr_new_handle("test", "127.0.0.1", 0L, pid = pid, owns = TRUE)
  expect_true(flowr:::.flowr_pid_alive(pid))
  flowr:::.flowr_stop_engine(handle)
  Sys.sleep(0.3)
  expect_false(flowr:::.flowr_pid_alive(pid))
})

test_that("a remote session never kills a server it did not start", {
  handle <- flowr:::.flowr_new_handle("remote", "127.0.0.1", 1042L, owns = FALSE)
  expect_false(flowr:::.flowr_stop_engine(handle))   # nothing to stop
})
