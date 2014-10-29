##------------------------------------------------------------------------------
context("getChecksum/basics")
##------------------------------------------------------------------------------

test_that("getChecksum", {

  setReactiveS3(id = "x_1", value = 10)
  expect_equal(getChecksum(id = "x_1"), digest::digest(10))
  setReactiveS3(id = "x_2", value = function() "object-ref: {id: x_1}")
  expect_equal(getChecksum(id = "x_2"), digest::digest(x_1))
  
  ## Clean up //
  rmReactive("x_1")
  rmReactive("x_2")
  
})
