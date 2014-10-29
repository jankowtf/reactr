##------------------------------------------------------------------------------
context("getBinding/basics")
##------------------------------------------------------------------------------

test_that("getBinding", {

  setReactiveS3(id = "x_1", value = 10)
  expect_equal(getBinding(id = "x_1"), getFromRegistry("x_1")$.func)
  setReactiveS3(id = "x_2", value = function() "object-ref: {id: x_1}")
  expect_equal(getBinding(id = "x_2"), getFromRegistry("x_2")$.func)
  
  ## Clean up //
  rmReactive("x_1")
  rmReactive("x_2")
  
})
