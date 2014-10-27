##------------------------------------------------------------------------------
context("setShinyReactive/one-directional")
##------------------------------------------------------------------------------

test_that("setShinyReactive/one-directional (1)", {

  resetRegistry()
  expect_equal(setShinyReactive(id = "x_1", value = 10), 10)
  expect_equal(setShinyReactive(id = "x_2", value = function() {
    "object-ref: {id: x_1}"
    x_1 * 2
  }), 10 * 2)
  
  expect_equal(x_1, 10)
  expect_equal(x_2, x_1 * 2)
  expect_equal(x_1 <- 20, 20)
  expect_equal(x_2, x_1 * 2)
  
  ## Clean up //
  removeReactive("x_1")
  removeReactive("x_2")
  expect_true(!length(showRegistry()))

})

test_that("setShinyReactive/one-directional (2)", {

  skip("environment issues")
  resetRegistry()
  where_1 <- new.env()
  expect_equal(setShinyReactive(id = "x_1", value = 10, where = where_1), 10)
  expect_equal(setShinyReactive(id = "x_2", value = function() {
    "object-ref: {id: x_1, where: where_1}"
    x_1 * 2
  }, where = where_1, where_1 = where_1), 10 * 2)
  
  expect_equal(where_1$x_1, 10)
  expect_equal(where_1$x_2, where_1$x_1 * 2)
  expect_equal(where_1$x_1 <- 100, 100)
  expect_equal(where_1$x_2, where_1$x_1 * 2)

  ## Clean up //
  removeReactive("x_1")
  removeReactive("x_2")
  expect_true(!length(showRegistry()))
  
})

##------------------------------------------------------------------------------
context("setShinyReactive/bi-directional")
##------------------------------------------------------------------------------

test_that("setShinyReactive/bi-directional (1)", {

  expect_equal(setShinyReactive(id = "x_1", value = function() {
    "object-ref: {id: x_2}"
    x_2
  }), NULL)
  expect_equal(setShinyReactive(id = "x_2", value = function() {
    "object-ref: {id: x_1}"
    x_1
  }), NULL)
  
  expect_equal(x_1, NULL)
  expect_equal(x_2, NULL)

  expect_equal(x_1 <- 10, 10)
  expect_equal(x_2, 10)
  expect_equal(x_1 <- 20, 20)
  expect_equal(x_2, 20)

  ## Clean up //
  removeReactive("x_1")
  removeReactive("x_2")
  expect_true(!length(showRegistry()))
  
})
  
