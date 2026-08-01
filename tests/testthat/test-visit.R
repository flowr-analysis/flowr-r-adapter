# AST visitor and the node-id -> location map it builds. Pure (no engine), so
# these run everywhere. The shapes below are the ones flowR actually sends.

test_that("an unnamed call is descended into and does not stop the walk", {
  # flowR omits `named` entirely for a call like `spec$ensure(x)` rather than
  # sending it as false, so anything but an explicit TRUE means unnamed
  node <- list(
    type = "RFunctionCall",
    lexeme = "spec$ensure",
    location = list(1L, 3L, 1L, 13L),
    calledFunction = list(type = "RSymbol", info = list(id = 5L),
                          location = list(1L, 3L, 1L, 13L)),
    arguments = list(list(type = "RSymbol", info = list(id = 6L),
                          location = list(1L, 15L, 1L, 15L)))
  )
  seen <- integer(0)
  flowr:::visit_node(node, function(n) {
    # base's %||% only exists from R 4.4, and the package's is not exported
    id <- n$info$id
    seen <<- c(seen, if (is.null(id)) -1L else as.integer(id))
    TRUE
  })
  expect_true(5L %in% seen)   # the called function was reached
  expect_true(6L %in% seen)   # ... and so were the arguments
})

test_that("a node with a location but no id is skipped, not an error", {
  # `info` is where the id lives, and flowR drops `info` when it serialises a
  # reply in chunks -- indexing a map with the resulting zero-length key errors
  node <- list(
    type = "RExpressionList",
    children = list(
      list(type = "RSymbol", location = list(1L, 1L, 1L, 1L)),               # no info
      list(type = "RSymbol", location = list(2L, 1L, 2L, 4L), info = list(id = 9L))
    )
  )
  map <- flowr:::make_id_to_location_map(node)   # errored before: zero-length key
  expect_identical(ls(map), "9")
  expect_identical(map[["9"]], list(2L, 1L, 2L, 4L))
})

test_that("an access node without an operator does not break the walk", {
  node <- list(type = "RAccess",
               accessed = list(type = "RSymbol", info = list(id = 1L),
                               location = list(1L, 1L, 1L, 1L)))
  expect_identical(ls(flowr:::make_id_to_location_map(node)), "1")
})

test_that("a slice reuses its stored location map and survives without one", {
  m <- flowr:::.flowr_new_map()
  m[["3"]] <- list(1L, 1L, 1L, 5L)
  s <- structure(list(locations = m), class = "flowr_slice")
  expect_identical(flowr:::.flowr_slice_location_map(s), m)

  # a slice without the field falls back to walking its AST
  old <- structure(list(analysis = list(results = list(normalize = list(ast =
    list(type = "RSymbol", info = list(id = 4L), location = list(2L, 1L, 2L, 2L))
  )))), class = "flowr_slice")
  expect_identical(ls(flowr:::.flowr_slice_location_map(old)), "4")
})

test_that("flowr_locations() returns an empty list rather than failing", {
  # an analysis whose AST carried no ids leaves the map empty; ordering it must
  # not error on the zero-length name vector
  s <- structure(list(locations = flowr:::.flowr_new_map()), class = "flowr_slice")
  expect_identical(flowr_locations(s), list())
})

test_that("a project AST tags each location with the file it is numbered in", {
  # line numbers restart per file, so which file a node sits in is part of its
  # address -- a project root knows that, a bare AST does not
  ast <- list(type = "RProject", files = list(
    list(filePath = "a.R", root = list(type = "RSymbol", info = list(id = 1L),
                                       location = list(3L, 1L, 3L, 2L))),
    list(filePath = "b.R", root = list(type = "RSymbol", info = list(id = 2L),
                                       location = list(3L, 1L, 3L, 2L)))
  ))
  map <- flowr:::make_id_to_location_map(ast)
  expect_identical(attr(map[["1"]], "file"), "a.R")
  expect_identical(attr(map[["2"]], "file"), "b.R")
  # the value itself stays the length-4 list every caller indexes
  expect_identical(map[["1"]][[1]], 3L)
  expect_length(map[["1"]], 4L)
})

test_that("covered lines are reported per file, not collapsed across them", {
  # line 3 of a.R and line 3 of b.R are different lines
  map <- flowr:::.flowr_new_map()
  map[["1"]] <- flowr:::.flowr_loc(list(3L, 1L, 3L, 2L), "a.R")
  map[["2"]] <- flowr:::.flowr_loc(list(3L, 1L, 3L, 2L), "b.R")
  cov <- flowr:::.flowr_covered_by_file(c(1, 2), map)
  expect_identical(nrow(cov), 2L)
  expect_identical(cov$file, c("a.R", "b.R"))
  expect_identical(cov$line, c(3L, 3L))
  # while the flat view still collapses them, which is why `covered` exists
  expect_identical(flowr:::.flowr_covered_lines(c(1, 2), map), 3L)
})

test_that("covered lines span multi-line nodes and drop duplicates", {
  map <- flowr:::.flowr_new_map()
  map[["1"]] <- flowr:::.flowr_loc(list(2L, 1L, 4L, 3L), "a.R")
  map[["2"]] <- flowr:::.flowr_loc(list(3L, 1L, 3L, 9L), "a.R")
  cov <- flowr:::.flowr_covered_by_file(c(1, 2), map)
  expect_identical(cov$line, 2:4)
  expect_true(all(cov$file == "a.R"))
})

test_that("a location without a file reports NA rather than dropping the line", {
  map <- flowr:::.flowr_new_map()
  map[["1"]] <- list(1L, 1L, 1L, 5L)          # no file attribute
  cov <- flowr:::.flowr_covered_by_file(1, map)
  expect_identical(nrow(cov), 1L)
  expect_true(is.na(cov$file))
  expect_identical(cov$line, 1L)
})

test_that("covered lines of an empty slice are an empty frame, not an error", {
  cov <- flowr:::.flowr_covered_by_file(integer(0), flowr:::.flowr_new_map())
  expect_identical(nrow(cov), 0L)
  expect_identical(names(cov), c("file", "line"))
})

test_that("flowr_locations() is ordered by node id, numerically", {
  m <- flowr:::.flowr_new_map()
  for (i in c(2L, 10L, 1L)) m[[as.character(i)]] <- list(i, 1L, i, 2L)
  s <- structure(list(locations = m), class = "flowr_slice")
  expect_identical(names(flowr_locations(s)), c("1", "2", "10"))
})
