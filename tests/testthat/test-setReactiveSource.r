##------------------------------------------------------------------------------
context("setReactiveSource/default")
##------------------------------------------------------------------------------

test_that("setReactiveSource/default", {

  expect_equal(res <- setReactiveSource(id = "x_1", value = 10), 10)
  expect_equal(x_1, 10)
  expect_equal(x_1 <- 20, 20)
  expect_equal(x_1, 20)
  rm("x_1")
  
})

test_that("setReactiveSource/overwrite", {

  expect_equal(setReactiveSource(id = "x_1", value = 10), 10)
  x_1 <- 20
  expect_equal(setReactiveSource(id = "x_1", value = 10, overwrite = FALSE), 20)
  rm("x_1")
  
})

test_that("setReactiveSource/typed", {

  expect_equal(res <- setReactiveSource(id = "x_1", value = 10, typed = TRUE), 10)
  expect_equal(x_1, 10)
  expect_error(x_1 <- "hello world!")
  rm("x_1")
  
})
