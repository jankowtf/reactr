context("getReactive/basics")
test_that("getReactive", {

  setReactiveS3(id = "x_1", value = 10)
  expect_equal(getReactive(id = "x_1"), 10)
  expect_is(res <- getReactive(id = "x_1", hidden = TRUE), 
            "ReactiveObject.S3")
  expect_equal(getFromRegistry(id = "x_1"), res)
  
})
