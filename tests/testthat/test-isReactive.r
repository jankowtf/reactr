##------------------------------------------------------------------------------
context("isReactive/basics")
##------------------------------------------------------------------------------

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

##------------------------------------------------------------------------------
context("isReactive/R6")
##------------------------------------------------------------------------------

test_that("isReactive/R6", {

  resetRegistry()
  require("R6")
  Test <- R6Class(
    classname = "Test",
    portable = TRUE,
    public = list(
      .x = "numeric"
    )
  )
#   Test <- setRefClass("Test",
#     fields = list(
#       .x = "numeric"
#     )
#   )
  where <- Test$new()
  expect_false(isReactive(id = ".x", where = where))
  
  setShinyReactive(id = ".x", value = 10, where = where)
  expect_true(isReactive(id = ".x", where = where))
  
  unsetReactive(".x", where = where)
  expect_false(isReactive(id = ".x", where = where))
  where$.x <- 20
  expect_false(isReactive(id = ".x", where = where))
  resetRegistry()
  
})

