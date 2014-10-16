context("print.ReactiveObject.S3_A")
test_that("print.ReactiveObject.S3", {

  test <- reactr::ReactiveObject.S3(value = 10)
  expect_is(test, "ReactiveObject.S3")
  expect_is(print.ReactiveObject.S3(test), "ReactiveObject.S3")
  expect_is(print(test), "ReactiveObject.S3")
  expect_equal(capture.output(print(test, as_is = FALSE)), "[1] 10")
  expect_is(print(test, as_is = FALSE), "numeric")
  
})

