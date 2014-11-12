context("isReactive/basics")
test_that("isReactive", {

  resetRegistry()
  setReactive(id = "x_1", value = 10)
  expect_true(isReactive(id = "x_1"))
  unsetReactive("x_1")
  expect_false(isReactive(id = "x_1"))
  x_2 <- 10
  expect_false(isReactive(id = "x_2"))
  obj <- ReactiveObject.S3()
  expect_false(isReactive("obj"))
  resetRegistry()
  
})

##------------------------------------------------------------------------------
context("isReactive/shiny")
##------------------------------------------------------------------------------

test_that("isReactive", {

  resetRegistry()
  setShinyReactive(id = "x_1", value = 10)
  expect_true(isReactive(id = "x_1"))
  unsetReactive("x_1")
  expect_false(isReactive(id = "x_1"))
  x_2 <- 10
  expect_false(isReactive(id = "x_2"))
  resetRegistry()
  
})
