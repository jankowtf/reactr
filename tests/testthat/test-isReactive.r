context("isReactive/basics")
test_that("isReactive", {

  resetRegistry()
  setReactiveS3(id = "x_1", value = 10)
  expect_true(isReactive(id = "x_1"))
  unsetReactive("x_1")
  expect_false(isReactive(id = "x_1"))
  x_2 <- 10
  expect_false(isReactive(id = "x_2"))
  obj <- ReactiveObject.S3()
  expect_false(isReactive("obj"))
  resetRegistry()
  
})
