# Regression tests for behaviour that only shows up on large inputs: the cache
# key, the socket reader's framing, and the linear-time collection helpers.
# All pure (no socket, no engine), so they run everywhere including CRAN.

# A node with an id and a location, as the normalised AST delivers them.
leaf <- function(id, line) {
  line <- as.integer(line)
  list(type = "RSymbol", info = list(id = id), location = list(line, 1L, line, 3L))
}

test_that("cache keys survive inputs larger than R's 10000-byte symbol limit", {
  # keying the session cache on the source text itself raised "variable names are
  # limited to 10000 bytes" for any script past ~10 kB -- i.e. any real project
  big <- strrep("x <- 1\n", 5000)
  expect_gt(nchar(big, type = "bytes"), 10000L)

  key <- flowr:::.flowr_cache_key(NULL, big, cfg = FALSE)
  expect_lt(nchar(key, type = "bytes"), 100L)

  env <- new.env(parent = emptyenv())
  expect_silent(env[[key]] <- "entry")
  expect_identical(env[[key]], "entry")
})

test_that("cache keys separate different code, and cfg from non-cfg", {
  a <- flowr:::.flowr_cache_key(NULL, "x <- 1", cfg = FALSE)
  b <- flowr:::.flowr_cache_key(NULL, "x <- 2", cfg = FALSE)
  expect_false(identical(a, b))
  expect_false(identical(a, flowr:::.flowr_cache_key(NULL, "x <- 1", cfg = TRUE)))
  # stable for the same input
  expect_identical(a, flowr:::.flowr_cache_key(NULL, "x <- 1", cfg = FALSE))
})

test_that("a file cache key changes when the file on disk changes", {
  f <- withr::local_tempfile(fileext = ".R")
  writeLines("a <- 1", f)
  before <- flowr:::.flowr_cache_key(f, NULL, cfg = FALSE)
  expect_identical(before, flowr:::.flowr_cache_key(f, NULL, cfg = FALSE))

  # keying on the path alone served a stale analysis after an edit
  Sys.sleep(1.1)                      # mtime resolution on some filesystems
  writeLines("a <- 1\nb <- 2", f)
  expect_false(identical(before, flowr:::.flowr_cache_key(f, NULL, cfg = FALSE)))
})

# Push bytes into a reader exactly as .flowr_read_message() does off the socket.
feed <- function(reader, text) {
  chunk <- charToRaw(text)
  i <- length(reader$pending) + 1L
  reader$pending[[i]] <- chunk
  pos <- which(chunk == as.raw(0x0a))
  reader$pending_nl[[i]] <- pos
  if (length(pos) > 0) reader$has_nl <- TRUE
  invisible(reader)
}

test_that("the reader frames messages split across arbitrary chunk boundaries", {
  r <- flowr:::.flowr_reader_init(new.env(parent = emptyenv()))
  expect_null(flowr:::.flowr_reader_take(r))

  feed(r, "he")
  feed(r, "llo\nwor")
  expect_identical(flowr:::.flowr_reader_take(r), "hello")
  expect_null(flowr:::.flowr_reader_take(r))   # "wor" is not a message yet

  feed(r, "ld\r\na\nb\n")
  expect_identical(flowr:::.flowr_reader_take(r), "world")   # CRLF tolerated
  expect_identical(flowr:::.flowr_reader_take(r), "a")
  expect_identical(flowr:::.flowr_reader_take(r), "b")
  expect_null(flowr:::.flowr_reader_take(r))

  # a drained reader releases its buffer instead of holding a multi-MB reply
  expect_length(r$buf, 0L)

  feed(r, "\nx\n")
  expect_identical(flowr:::.flowr_reader_take(r), "")        # empty message
  expect_identical(flowr:::.flowr_reader_take(r), "x")
})

test_that("the reader returns a large multi-chunk message intact", {
  r <- flowr:::.flowr_reader_init(new.env(parent = emptyenv()))
  parts <- vapply(1:200, function(i) strrep(as.character(i %% 10), 1000), character(1))
  for (p in parts) feed(r, p)
  feed(r, "\n")
  msg <- flowr:::.flowr_reader_take(r)
  expect_identical(nchar(msg, type = "bytes"), 200000L)
  expect_identical(msg, paste(parts, collapse = ""))
})

test_that("the id-to-location map indexes by node id and scales to many nodes", {
  ast <- list(type = "RExpressionList",
              children = lapply(1:500, function(i) leaf(i, i)))
  map <- flowr:::make_id_to_location_map(ast)
  expect_identical(map[["1"]], list(1L, 1L, 1L, 3L))
  expect_identical(map[["500"]], list(500L, 1L, 500L, 3L))
  expect_null(map[["nope"]])
  expect_length(ls(map), 500L)
})

test_that("flowr_locations() still returns a named list, ordered by node id", {
  ast <- list(type = "RExpressionList",
              children = list(leaf(10, 10), leaf(2, 2), leaf(1, 1)))
  loc <- flowr_locations(ast)
  expect_type(loc, "list")
  expect_identical(names(loc), c("1", "2", "10"))
  expect_identical(loc[["10"]], list(10L, 1L, 10L, 3L))
})

test_that("covered lines and slice locations agree with the map", {
  ast <- list(type = "RExpressionList",
              children = list(leaf(1, 1), leaf(2, 4), leaf(3, 7)))
  map <- flowr:::make_id_to_location_map(ast)
  expect_identical(flowr:::.flowr_covered_lines(c(1, 3), map), c(1L, 7L))
  expect_identical(flowr:::.flowr_covered_lines(character(0), map), integer(0))
  expect_identical(flowr:::.flowr_covered_lines(c(99), map), integer(0))

  s <- structure(list(ids = c(1, 99, 2),
                      analysis = list(results = list(normalize = list(ast = ast)))),
                 class = "flowr_slice")
  expect_identical(flowr_slice_locations(s),
                   list(list(1L, 1L, 1L, 3L), list(4L, 1L, 4L, 3L)))
})

test_that("project file discovery prunes vendored trees and can be told not to", {
  d <- withr::local_tempdir()
  dir.create(file.path(d, "R"))
  dir.create(file.path(d, "renv", "library", "pkg"), recursive = TRUE)
  dir.create(file.path(d, "node_modules", "x"), recursive = TRUE)
  writeLines("a <- 1", file.path(d, "R", "mine.R"))
  writeLines("v <- 1", file.path(d, "renv", "library", "pkg", "vendored.R"))
  writeLines("j <- 1", file.path(d, "node_modules", "x", "junk.R"))

  withr::local_options(flowr.quiet = TRUE)
  expect_identical(basename(flowr:::.flowr_project_files(d)), "mine.R")

  withr::local_options(flowr.skip_dirs = character(0))
  expect_setequal(basename(flowr:::.flowr_project_files(d)),
                  c("mine.R", "vendored.R", "junk.R"))
})

test_that("an oversized file set is refused with an actionable message", {
  withr::local_options(flowr.max_files = 10L)
  expect_silent(flowr:::.flowr_check_file_count(10L))
  expect_error(flowr:::.flowr_check_file_count(11L), "flowr.max_files")

  withr::local_options(flowr.max_files = 0L)          # 0 disables the guard
  expect_silent(flowr:::.flowr_check_file_count(1e6))
})

test_that("the request timeout grows with the size of the analysed input", {
  withr::local_options(flowr.request_timeout = 100, flowr.timeout_per_mb = 10)
  expect_identical(flowr:::.flowr_scaled_timeout(0), 100)
  expect_identical(flowr:::.flowr_scaled_timeout(10 * 1048576), 200)
})
