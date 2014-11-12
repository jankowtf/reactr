##------------------------------------------------------------------------------
context("reactiveSource/default")
##------------------------------------------------------------------------------

test_that("reactiveSource/default", {

  expect_equal(res <- reactiveSource(id = "x_1", value = 10), 10)
  expect_equal(x_1, 10)
  expect_equal(x_1 <- 20, 20)
  expect_equal(x_1, 20)
  rm("x_1")
  
})

test_that("reactiveSource/overwrite", {

  expect_equal(reactiveSource(id = "x_1", value = 10), 10)
  x_1 <- 20
  expect_equal(reactiveSource(id = "x_1", value = 10, overwrite = FALSE), 20)
  rm("x_1")
  
})

test_that("reactiveSource/typed", {

  ## Strict = 0 //
  expect_equal(res <- reactiveSource(id = "x_1", value = 10, typed = TRUE), 10)
  expect_equal(x_1, 10)
  expect_equal(x_1 <- "hello world!", "hello world!")
  expect_equal(x_1, 10)
  
  ## Strict = 1 //
  expect_equal(res <- reactiveSource(id = "x_1", value = 10, typed = TRUE,
    strict = 1), 10)
  expect_equal(x_1, 10)
  expect_warning(expect_equal(x_1 <- "hello world!", "hello world!"))
  expect_equal(x_1, 10)
  
  expect_equal(res <- reactiveSource(id = "x_1", value = 10, 
    typed = TRUE, strict = 2), 10)
  expect_error(x_1 <- "hello world!")
  rm("x_1")
  
})
