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

test_that("a chunk-serialised reply's bare placeholders are read as strings", {
  # the chunked writer emits the built-in-environment placeholder bare, where
  # the replacer would have produced the string "<BuiltInEnvironment>"
  broken <- '{"env":{"id":7,"parent":<BuiltInEnvironment>,"memory":[]},"xs":[<BuiltInEnvironment>]}'
  res <- flowr:::.flowr_parse(broken)
  expect_identical(res$env$parent, "<BuiltInEnvironment>")
  expect_identical(res$xs[[1]], "<BuiltInEnvironment>")
  expect_identical(res$env$id, 7L)
})

test_that("a chunk-serialised reply's unescaped regex backslashes are read back", {
  # RegExp values are interpolated with `${re.toString()}`, so their own
  # backslashes arrive unescaped -- `\.` is not a JSON escape
  res <- flowr:::.flowr_parse('{"pattern":"/^(dev\\.new|x11)$/"}')
  expect_identical(res$pattern, "/^(dev\\.new|x11)$/")
})

test_that("escape repair respects backslash parity and is idempotent", {
  r <- flowr:::.flowr_repair_json
  # a legitimately escaped backslash followed by `.` must survive untouched
  expect_identical(r('"a\\\\.b"'), '"a\\\\.b"')
  # a lone backslash opening a non-escape gets escaped
  expect_identical(r('"a\\.b"'), '"a\\\\.b"')
  # real JSON escapes are left alone
  expect_identical(r('"a\\nb\\tc\\"d\\u0041"'), '"a\\nb\\tc\\"d\\u0041"')
  # running the repair twice changes nothing more
  expect_identical(r(r('"a\\.b"')), r('"a\\.b"'))
})

test_that("a well-formed reply is never rewritten by the repair", {
  # the rewrite only runs after a parse has already failed, so a payload that
  # legitimately contains the placeholder text is left exactly as it arrived
  ok <- '{"msg":"see <BuiltInEnvironment> for details","n":1}'
  expect_identical(flowr:::.flowr_parse(ok)$msg,
                   "see <BuiltInEnvironment> for details")
  expect_identical(flowr:::.flowr_repair_json(ok), ok)
})

test_that("a genuinely truncated reply still reports why it could not be read", {
  expect_error(flowr:::.flowr_parse('{"a":'), "could not parse flowR")
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

test_that("query() result printing caps long nested lists instead of dumping them whole", {
  # a long nested list (e.g. a package's full export list) must stay intact --
  # only the console view caps
  big <- as.list(sprintf("sym%d", 1:50))
  x <- flowr:::.flowr_query_tag(list(dependencies = list(
    library = list(list(value = "dplyr", namespaceInfo = list(exportedSymbols = big)))
  )))
  expect_length(x$dependencies$library[[1]]$namespaceInfo$exportedSymbols, 50L)

  out <- capture.output(print(x))
  expect_true(any(grepl("\\.\\.\\. 30 more", out)))
  expect_false(any(grepl("sym50", out)))
  expect_true(any(grepl("sym20", out)))
})

test_that("query() result printing also caps total size for narrow-but-deep structures", {
  # a per-level cap alone doesn't bound a chain of nested singletons (e.g. a
  # call-graph vertex's repeated environment/parent chain)
  chain <- list(id = 1)
  for (i in 1:500) chain <- list(id = i, parent = chain)
  out <- capture.output(print(flowr:::.flowr_query_truncate(chain, budget = 50L)))
  expect_true(any(grepl("output too large", out)))
  expect_lt(length(out), 500L)
})
