##------------------------------------------------------------------------------
context("getBinding/basics")
##------------------------------------------------------------------------------

test_that("getBinding", {

  resetRegistry()
  obj <- ReactiveObject.S3(id = "x_1", value = 10)
  obj$.register()
  expect_equal(getBinding(id = "x_1"), getRegistry()[[obj$.uid]])
  resetRegistry()
  
})
