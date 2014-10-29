##------------------------------------------------------------------------------
context("setShinyReactive/in parent environment")
##------------------------------------------------------------------------------

test_that("setShinyReactive/two", {

  require("shiny")
  
  ## In .GlobalEnv //
  ## Make sure 'x_1' and 'x_2' are removed:
#   rm(list = ls(environment()))
  suppressWarnings(rm(x_1))
  suppressWarnings(rm(x_2))
  
  value <- Sys.time()
  setShinyReactive(id = "x_1", value = value)
  expect_equal(x_1, value)
  value_2 <- Sys.time()
  expect_equal(x_1 <- value_2, value_2)
  expect_equal(x_1, value_2)
#   where=environment()
  expect_equal(
    setShinyReactive(id = "x_2", value = reactive(x_1 + 60*60*24)),
    value_2 + 60*60*24)
  expect_equal(x_2, value_2 + 60*60*24)
  ## --> 'x_1' + one day
  x_1 <- Sys.time()
  x_1
  expect_equal(x_2, x_1 + 60*60*24)
  ## --> reactive
  
  suppressWarnings(rm(x_1))
  suppressWarnings(rm(x_2))
  
})

test_that("setShinyReactive/three", {
  
  x_1 <- setShinyReactive("x_1", 10)
  x_2 <- setShinyReactive("x_2", 20)
  expect_equal(x_3 <- setShinyReactive("x_3", value = reactive(x_1 + x_2 * 2)),
               x_1 + x_2 * 2)
  expect_equal(x_3, x_1 + x_2 * 2)
  x_1 <- 100
  expect_equal(x_3, x_1 + x_2 * 2)
  x_2 <- 100
  expect_equal(x_3, x_1 + x_2 * 2)
  
  suppressWarnings(rm(x_1))
  suppressWarnings(rm(x_2))
  suppressWarnings(rm(x_3))
  
})

##------------------------------------------------------------------------------
context("setShinyReactive/in custom environment")
##------------------------------------------------------------------------------


test_that("setShinyReactive/two", {
  
  ## In custom environment //
  where <- new.env()
  value <- 10
  expect_equal(setShinyReactive("x_1", value = value, where = where), 
               value)
  expect_equal(where$x_1, value)
  expect_equal(where$x_1 <- 100, 100)
  expect_equal(where$x_1, 100)

  expect_equal(
    setShinyReactive(
      "x_2", value = reactive(where$x_1 * 2), where = where
    ),
    where$x_1 * 2
  )
  expect_equal(where$x_2, where$x_1 * 2)
  where$x_1 <- 500
  expect_equal(where$x_2, where$x_1 * 2)
  
})
