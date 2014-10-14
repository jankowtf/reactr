context("print.Reactive.S3_A")
test_that("print.Reactive.S3", {

  test <- reactr::Reactive.S3(value = 10)
  expect_is(test, "Reactive.S3")
  expect_is(print.Reactive.S3(test), "Reactive.S3")
  expect_is(print(test), "Reactive.S3")
  expect_equal(capture.output(print(test, as_is = FALSE)), "[1] 10")
  expect_is(print(test, as_is = FALSE), "numeric")
  
})

