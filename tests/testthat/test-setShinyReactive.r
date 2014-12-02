##------------------------------------------------------------------------------
context("setShinyReactive/reactive-only")
##------------------------------------------------------------------------------

test_that("setShinyReactive/reactive-only", {

  shiny::makeReactiveBinding("x_1")
  x_1 <- 10
  expect_is(
    res <- setShinyReactive(id = "x_2", value = reactiveExpression(x_1 * 2)),
    "reactive"
  )
  expect_equal(res(), x_1 * 2)
  expect_equal(x_2, x_1 * 2)
  x_1 <- 20
  expect_equal(res(), x_1 * 2)
  expect_equal(x_2, x_1 * 2)
  x_2 <- 1
  expect_equal(x_2, x_1 * 2)
  rm("x_1")
  rm("x_2")
  
  shiny::makeReactiveBinding("x_1")
  x_1 <- 10
  expect_is(
    res <- setShinyReactive(id = "x_2", value = reactiveExpression(x_1 * 2), 
      lazy = TRUE),
    "reactive"
  )
  expect_equal(res(), x_1 * 2)
  expect_equal(x_2(), x_1 * 2)
  x_1 <- 20
  expect_equal(res(), x_1 * 2)
  expect_equal(x_2(), x_1 * 2)
  x_2 <- 1
  
})

test_that("setShinyReactive/strict_get=1", {
  
  shiny::makeReactiveBinding("x_1")
  x_1 <- 10
  expect_is(
    res <- setShinyReactive(id = "x_2", value = reactiveExpression(x_1 * 2), 
                            strict_set = 1),
    "reactive"
  )
  expect_warning(x_2 <- 1)
  rm("x_1")
  rm("x_2")

}) 

test_that("setShinyReactive/strict_get=2", {
  
  shiny::makeReactiveBinding("x_1")
  class(x_1)
  x_1 <- 10
  expect_is(
    res <- setShinyReactive(id = "x_2", value = reactiveExpression(x_1 * 2), 
                            strict_set = 2),
    "reactive"
  )
  expect_error(x_2 <- 1)
  rm("x_1")
  rm("x_2")

}) 

##------------------------------------------------------------------------------
context("setShinyReactive/source-and-reactive")
##------------------------------------------------------------------------------

test_that("setShinyReactive/source-and-reactive", {

  ## Eager //
  expect_equal(
    res <- setShinyReactive(id = "x_1", value = 10),
    10
  )
  expect_equal(res, 10)
  expect_equal(x_1, 10)
  x_1 <- 20
  expect_equal(res, 10)
  expect_equal(x_1, 20)
  expect_is(
    res <- setShinyReactive(id = "x_2", value = reactiveExpression(x_1 * 2)),
    "reactive"
  )
  expect_equal(res(), x_1 * 2)
  expect_equal(x_2, x_1 * 2)
  x_1 <- 30
  expect_equal(res(), x_1 * 2)
  expect_equal(x_2, x_1 * 2)
  x_2 <- 1
  expect_equal(x_2, x_1 * 2)
  rm("x_1")
  rm("x_2")
  
  ## Lazy //
  expect_equal(
    res <- setShinyReactive(id = "x_1", value = 10),
    10
  )
  expect_is(
    res <- setShinyReactive(id = "x_2", value = reactiveExpression(x_1 * 2), 
      lazy = TRUE),
    "reactive"
  )
  expect_equal(res(), x_1 * 2)
  expect_equal(x_2(), x_1 * 2)
  x_1 <- 20
  expect_equal(res(), x_1 * 2)
  expect_equal(x_2(), x_1 * 2)
  x_2 <- 1
  expect_equal(x_2(), x_1 * 2)
  
})

test_that("setShinyReactive/strict_get=1", {
  
  expect_equal(
    res <- setShinyReactive(id = "x_1", value = 10),
    10
  )
  expect_is(
    res <- setShinyReactive(id = "x_2", value = reactiveExpression(x_1 * 2), 
                            strict_set = 1),
    "reactive"
  )
  expect_warning(x_2 <- 1)
  rm("x_1")
  rm("x_2")

}) 

test_that("setShinyReactive/strict_get=2", {
  
  expect_equal(
    res <- setShinyReactive(id = "x_1", value = 10),
    10
  )
  x_1 <- 10
  expect_is(
    res <- setShinyReactive(id = "x_2", value = reactiveExpression(x_1 * 2), 
                            strict_set = 2),
    "reactive"
  )
  expect_error(x_2 <- 1)
  rm("x_1")
  rm("x_2")

}) 

##------------------------------------------------------------------------------
context("setShinyReactive/push")
##------------------------------------------------------------------------------

test_that("setShinyReactive/push", {

  skip("manual only as I don't know how to test for the messages yet")
  ## Eager //
  expect_equal(
    res <- setShinyReactive(id = "x_1", value = 10),
    10
  )
  expect_is(
    res <- setShinyReactive(
      id = "x_2", 
      value = reactiveExpression({
        message(paste0(Sys.time(), ": push from x_1"))
        x_1 * 2
      }),
      push = TRUE
    ),
    "reactive"
  )
  
  x_1 <- 20
  ## --> pushes to `x_2`
  x_1 <- 20
  ## --> no push as no actual update of `x_1` took place
  x_2
  ## --> cached value
  x_1 <- 30
  ## --> pushes to `x_2`
  x_2
  rm("x_1")
  rm("x_2")
  
  ## Lazy //
  expect_equal(
    res <- setShinyReactive(id = "x_1", value = 10),
    10
  )
  expect_is(
    res <- setShinyReactive(
      id = "x_2", 
      value = reactiveExpression({
        message(paste0(Sys.time(), ": push from x_1"))
        x_1 * 2
      }),
      lazy = TRUE,
      push = TRUE
    ),
    "reactive"
  )
  
  x_1 <- 20
  ## --> pushes to `x_2`
  x_1 <- 20
  ## --> no push as no actual update of `x_1` took place
  expect_equal(x_2(), x_1 * 2)
  ## --> cached value
  x_1 <- 30
  ## --> pushes to `x_2`
  expect_equal(x_2(), x_1 * 2)
  rm("x_1")
  rm("x_2")

})

##------------------------------------------------------------------------------
context("setShinyReactive/debugging observer")
##------------------------------------------------------------------------------

test_that("setShinyReactive/debugging observer", {
  
  skip("manual only")
  ## --> seems like if anything goes wrong in the call to `observe()` and/or it 
  ## `observer()` gets called more than once, then things somehow get messed up
  
  ## Eager //
  makeReactiveBinding("x_1")
  setShinyReactive("x_2", value = reactiveExpression({
    message(paste0(Sys.time(), ": push from x_1"))
    x_1 + 60*60*24
  }))
  shiny:::setAutoflush(FALSE)
  obs <- observe(print(paste("The time changed:", x_2)))
#   obs$destroy()
#   shiny:::flushReact()
  x_2
  x_1 <- Sys.time()
  x_2
  obs$destroy()
  rm("x_1")
  rm("x_2")
  
  ## Lazy //
  makeReactiveBinding("x_1")
  setShinyReactive("x_2", value = reactiveExpression({
    message(paste0(Sys.time(), ": push from x_1"))
    x_1 + 60*60*24
  }), lazy = TRUE)
  shiny:::setAutoflush(TRUE)
  obs <- observe(print(paste("The time changed:", x_2())))
#   obs$destroy()
#   shiny:::flushReact()
  
  x_2()
  ## --> still empty/uninitialized
  x_1 <- Sys.time()
  ## --> push
  x_2()
  obs$destroy()
  rm("x_1")
  rm("x_2")

  ## When error occurs //
  makeReactiveBinding("x_1")
  setShinyReactive("x_2", value = reactiveExpression({
    message(paste0(Sys.time(), ": push from x_1"))
    x_1 + 60*60*24
  }))
  shiny:::setAutoflush(FALSE)
  obs <- observe(print("The time changed:", x_2))
  ## --> intentional error
#   obs$destroy()
#   shiny:::flushReact()
  x_2
  x_1 <- Sys.time()
  ## --> no push which is evident
  x_2
  obs$destroy()
  ## --> destroying should remove the "observing" entry
  ## To make sure: remove `obs` and reactive objects as well:
  rm(obs)
  rm("x_1")
  rm("x_2")

  ## Correcting the error:
  makeReactiveBinding("x_1")
  setShinyReactive("x_2", value = reactiveExpression({
    message(paste0(Sys.time(), ": push from x_1"))
    x_1 + 60*60*24
  }))
  shiny:::setAutoflush(FALSE)
  obs <- observe(print(paste0("The time changed:", x_2)))
  
  x_1 <- Sys.time()
  ## --> no push which this time is not evident to me why not
  ## Q: how do I restore the integrity of a corrupt observer?
  
  rm("x_1")
  rm("x_2")

})


##------------------------------------------------------------------------------
context("setShinyReactive/bi-directional")
##------------------------------------------------------------------------------

test_that("setShinyReactive/bi-directional", {

  skip("not working as `o.value <<- v` is disabled")
  expect_equal(
    res <- setShinyReactive(id = "x_1", value = 10),
    10
  )
  expect_is(
    res <- setShinyReactive(id = "x_2", value = reactiveExpression(x_1 * 2)),
    "reactive"
  )
  expect_equal(x_2, 20)
  x_2 <- 100
  expect_equal(x_2, 100)
  x_1 <- 1
  expect_equal(x_2, 2)
  
})

# library(shiny)
# options(shiny.suppressMissingContextError=TRUE)
# makeReactiveBinding("x_1")
# x_1 <- Sys.time()
# x_2 <- reactive(x_1 + 60*60*24)
# x_1
# x_2()
# shiny:::setAutoflush(TRUE)
# obs <- observe(print(paste0("push from x_1: ", x_2())))
# # obs$destroy()
# # shiny:::flushReact()
# 
# 
# x_1 <- Sys.time()
# # obs$destroy()
# library(shiny)
# options(shiny.suppressMissingContextError=TRUE)
# 
# # Now let's try an observer
# makeReactiveBinding("x_1")
# shiny:::setAutoflush(TRUE)
# observe(print(paste("The time changed:", x_1)))
# x_1 <- Sys.time()
# rm(x_1)
# rm(x_2)
# 
# library(shiny)
# options(shiny.suppressMissingContextError=TRUE)
# makeReactiveBinding("a")
# makeReactiveBinding("b")
# observe(a <<- b)
# observe(b <<- a)
# 
# a <- 1
# b
# b <- 2
# a

##------------------------------------------------------------------------------
context("setShinyReactive/already regular binding")
##------------------------------------------------------------------------------

test_that("setShinyReactive/already regular binding", {
  
  x_1 <- 1
  expect_equal(setShinyReactive(id = "x_1", value = 10), 10)
  x_2 <- 1
  expect_is(
    setShinyReactive(id = "x_2", value = reactiveExpression(x_1 * 2)),
    "reactive"
  )
})

##------------------------------------------------------------------------------
context("setShinyReactive/R6")
##------------------------------------------------------------------------------

test_that("setShinyReactive/R6", {
  
  require("R6")
  Test <- R6Class(
    classname = "Test",
    portable = TRUE,
    public = list(
      .x = "numeric"
    )
  )
  where <- Test$new()
  expect_equal(setShinyReactive(id = ".x", value = 10, where = where), 10)
  expect_is(
    setShinyReactive(id = "x_1", value = reactiveExpression(where$.x * 2)),
    "reactive"
  )
  expect_identical(x_1, 20)
  where$.x <- 20
  expect_identical(x_1, 40)
  
})
