# print.flowr_overview()/print.flowr_project(): dependency-item label rendering.

test_that("a library item's label is the package name, not its namespaceInfo", {
  # `$name` used to partial-match `namespaceInfo` on library/require items
  ov <- structure(list(library = list(list(
    nodeId = 3, functionName = "library", value = "dplyr", criterion = "$3",
    namespaceInfo = list(exportedSymbols = as.list(sprintf("sym%d", 1:50)))
  ))), class = "flowr_overview")
  out <- capture.output(print(ov))
  expect_true(any(grepl("^  dplyr\\b", out)))
  expect_false(any(grepl("sym1", out)))
})

test_that("print.flowr_project's dependency summary also uses the package name", {
  ov <- structure(list(library = list(list(
    nodeId = 3, functionName = "library", value = "dplyr", criterion = "$3",
    namespaceInfo = list(exportedSymbols = as.list(sprintf("sym%d", 1:50)))
  ))), class = "flowr_overview")
  proj <- structure(list(files = "a.R", roleCounts = list(), dependencies = ov,
                         flowr = "2.13.1"),
                    class = "flowr_project")
  out <- capture.output(print(proj))
  expect_true(any(grepl("dplyr", out)))
  expect_false(any(grepl("sym1", out)))
})
