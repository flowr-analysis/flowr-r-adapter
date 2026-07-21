# flowr_graph(): edge-type bitmask decoding.

test_that("edge type bitmasks decode to flowR's own edge-type names", {
  expect_identical(flowr:::.flowr_decode_edge_type(1L), "reads")
  expect_identical(flowr:::.flowr_decode_edge_type(65L), "reads,arg")
  expect_identical(flowr:::.flowr_decode_edge_type(72L), "returns,arg")
  expect_identical(flowr:::.flowr_decode_edge_type(NULL), NA_character_)
  # an unrecognised bit falls back to the raw number rather than dropping it
  expect_identical(flowr:::.flowr_decode_edge_type(1024L), "1024")
})
