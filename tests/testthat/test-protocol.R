# Regression tests for the wire-protocol framing and request construction.
# These are pure (no socket, no network) and run everywhere, including CRAN.

test_that("line extraction handles complete, partial, multiple and CRLF frames", {
  # no newline yet -> nothing to take
  expect_null(flowr:::.flowr_extract_line(charToRaw("partial")))

  # single complete line
  r <- flowr:::.flowr_extract_line(charToRaw("hello\n"))
  expect_identical(r$line, "hello")
  expect_length(r$rest, 0L)

  # two lines in one buffer: take first, keep remainder
  r <- flowr:::.flowr_extract_line(charToRaw("a\nb\n"))
  expect_identical(r$line, "a")
  expect_identical(rawToChar(r$rest), "b\n")

  # CRLF is tolerated
  r <- flowr:::.flowr_extract_line(charToRaw("win\r\n"))
  expect_identical(r$line, "win")

  # empty line
  r <- flowr:::.flowr_extract_line(charToRaw("\nx"))
  expect_identical(r$line, "")
  expect_identical(rawToChar(r$rest), "x")
})

test_that("query normalisation accepts names, objects and lists", {
  expect_identical(flowr:::.flowr_normalize_query("dependencies"),
                   list(list(type = "dependencies")))
  expect_identical(flowr:::.flowr_normalize_query(c("a", "b")),
                   list(list(type = "a"), list(type = "b")))
  q <- list(type = "static-slice", criteria = "3@x")
  expect_identical(flowr:::.flowr_normalize_query(q), list(q))
  expect_identical(flowr:::.flowr_normalize_query(list(q, q)), list(q, q))
  expect_error(flowr:::.flowr_normalize_query(42), "query type name")
})

test_that("a length-one criterion serialises as a JSON array, not a scalar", {
  # I()-wrapping is what keeps single criteria/queries as arrays for flowR
  json <- jsonlite::toJSON(list(criterion = I(as.character("3@x"))), auto_unbox = TRUE)
  expect_match(as.character(json), "\\[\"3@x\"\\]")
})
