context("getFromRegistry/basics")
test_that("getFromRegistry", {

  resetRegistry()
  obj <- ReactiveObject.S3(id = "x_1", value = 10)
  obj$register()
  expect_equal(getFromRegistry(id = "x_1"), getRegistry()[[obj$uid]])
  resetRegistry()
  
})
