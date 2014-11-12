## Globals //
verbose <- FALSE

## Temporarily disable until everything is refactored
if (FALSE) {
  
##------------------------------------------------------------------------------
context("setShinyReactive/one-directional")
##------------------------------------------------------------------------------

test_that("setShinyReactive/one-directional (1)", {

#   resetRegistry()
  expect_equal(setShinyReactive(id = "x_1", value = 10, verbose = verbose), 10)
  expect_equal(setShinyReactive(id = "x_2", value = function() {
    "object-ref: {id: x_1}"
    x_1 * 2
  }, verbose = verbose), 10 * 2)
  
  expect_equal(x_1, 10)
  expect_equal(x_2, x_1 * 2)
  expect_equal(x_1 <- 20, 20)
  expect_equal(x_2, x_1 * 2)
  
  ## Clean up //
  rmReactive("x_1")
  rmReactive("x_2")

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
  rmReactive("x_1")
  rmReactive("x_2")
  
})

##------------------------------------------------------------------------------
context("setShinyReactive/bi-directional")
##------------------------------------------------------------------------------

test_that("setShinyReactive/bi-directional (1)", {

  expect_equal(setShinyReactive(id = "x_1", value = function() {
    "object-ref: {id: x_2}"
    x_2
  }, verbose = verbose), NULL)
  expect_equal(setShinyReactive(id = "x_2", value = function() {
    "object-ref: {id: x_1}"
    x_1
  }, verbose = verbose), NULL)
  
  expect_equal(x_1, NULL)
  expect_equal(x_2, NULL)

  expect_equal(x_1 <- 10, 10)
  expect_equal(x_2, 10)
  expect_equal(x_1 <- 20, 20)
  expect_equal(x_2, 20)
  
  ## Clean up //
  rmReactive("x_1")
  rmReactive("x_2")
  
})

test_that("setShinyReactive/bi-directional (2)", {  
  
  expect_equal(setShinyReactive(id = "x_1", value = function() {
    "object-ref: {id: x_2}"
    x_2
  }, verbose = verbose), NULL)
  expect_equal(setShinyReactive(id = "x_2", value = function() {
    "object-ref: {id: x_1}"
    x_1
  }, verbose = verbose), NULL)
  
  expect_equal(x_1 <- 10, 10)
  expect_equal(x_2, 10)
  expect_equal(x_1, 10)
  expect_equal(x_1, x_2)
  expect_equal(x_2, x_1)
  ## Update, `x_1`, `x_2` //
  expect_equal(x_2 <- 1, 1)
  expect_equal(x_1, 1) ## update (x_1:x_2:1)
  expect_equal(x_2, 1) ## update (x_2:x_1:1)
  expect_equal(x_1, 1) ## cache
  expect_equal(x_1, x_2) ## cache
  expect_equal(x_2, x_1) ## cache
  ## Update, `x_2`, `x_1` //
  expect_equal(x_2 <- 2, 2)
  expect_equal(x_2, 2) ## cache
  expect_equal(x_1, 2) ## update (x_1:x_2:2)
  expect_equal(x_2, 2) ## update (x_2:x_1:2)
  expect_equal(x_1, 2) ## cache
  expect_equal(x_2, 2) ## cache 
  ## Double update before explicit request, `x_1`, `x_1`, `x_2` //
  expect_equal(x_2 <- 3, 3)
  expect_equal(x_1 <- 4, 4)
  expect_equal(x_1 , 4) ## update (x_1:x_2:3:x_1:4)
  expect_equal(x_1 , 4) ## update (x_1:x_2:4)
  expect_equal(x_2 , 4) ## cache
  ## Double update before explicit request, `x_1`, `x_2`, `x_1` //
  expect_equal(x_2 <- 1, 1)
  expect_equal(x_1 <- 2, 2)
  expect_equal(x_1 , 2) ## update (x_1:x_2:1:x_1:2)
  expect_equal(x_2 , 2) ## update (x_1:x_2:2)
  expect_equal(x_1 , 2) ## cache
  ## Double update before explicit request, `x_2`, `x_2, `x_1` //
  expect_equal(x_2 <- 3, 3)
  expect_equal(x_1 <- 4, 4)
  expect_equal(x_2 , 3) ## update (x_2:x_1:4:x_2:3)
  expect_equal(x_2 , 3) ## update (x_2:x_1:3)
  expect_equal(x_1 , 3) ## cache
  ## Double update before explicit request, `x_2`, `x_1`, `x_2` //
  expect_equal(x_2 <- 1, 1)
  expect_equal(x_1 <- 2, 2)
  expect_equal(x_2 , 1) ## update (x_2:x_1:2:x_2:1)
  expect_equal(x_1 , 1) ## cache
  expect_equal(x_2 , 1) ## update (x_2:x_1:1)
  
  ## Clean up //
  rmReactive("x_1")
  rmReactive("x_2")
  
  ##----------
  
  if (FALSE) {
    setReactive(id = "x_3", value = function() {
      "object-ref: {id: x_4}"
      x_4
    }, verbose = verbose)
    setReactive(id = "x_4", value = function() {
      "object-ref: {id: x_3}"
      x_3
    }, verbose = verbose)
  
    expect_equal(x_3 <- 10, 10)
    expect_equal(x_4, 10)
    expect_equal(x_3 <- 20, 20)
    expect_equal(x_4, 20)
    expect_equal(x_3 <- 30, 30)
    expect_equal(x_4, 30)
    
    ## Clean up //
    rmReactive("x_3")
    rmReactive("x_4")
    
    setReactive(id = "x_3", value = function() {
      "object-ref: {id: x_4}"
      x_4
    }, verbose = verbose)
    setReactive(id = "x_4", value = function() {
      "object-ref: {id: x_3}"
      x_3
    }, verbose = verbose)
    
    expect_equal(x_3 <- 10, 10)
    expect_equal(x_4, 10)
    expect_equal(x_3, 10)
    expect_equal(x_3, x_4)
    expect_equal(x_4, x_3)
    ## Update, `x_3`, `x_4` //
    expect_equal(x_4 <- 1, 1)
    expect_equal(x_3, 1) ## update (x_3:x_4:1)
    expect_equal(x_4, 1) ## update (x_4:x_3:1)
    expect_equal(x_3, 1) ## cache
    expect_equal(x_3, x_4) ## cache
    expect_equal(x_4, x_3) ## cache
    ## Update, `x_4`, `x_3` //
    expect_equal(x_4 <- 2, 2)
    expect_equal(x_4, 2) ## cache
    expect_equal(x_3, 2) ## update (x_3:x_4:2)
    expect_equal(x_4, 2) ## update (x_4:x_3:2)
    expect_equal(x_3, 2) ## cache
    expect_equal(x_4, 2) ## cache 
    ## Double update before explicit request, `x_3`, `x_3`, `x_4` //
    expect_equal(x_4 <- 3, 3)
    expect_equal(x_3 <- 4, 4)
    expect_equal(x_3 , 4) ## update (x_3:x_4:3:x_3:4)
    expect_equal(x_3 , 4) ## update (x_3:x_4:4)
    expect_equal(x_4 , 4) ## cache
    ## Double update before explicit request, `x_3`, `x_4`, `x_3` //
    expect_equal(x_4 <- 1, 1)
    expect_equal(x_3 <- 2, 2)
    expect_equal(x_3 , 2) ## update (x_3:x_4:1:x_3:2)
    expect_equal(x_4 , 2) ## update (x_3:x_4:2)
    expect_equal(x_3 , 2) ## cache
    ## Double update before explicit request, `x_4`, `x_4, `x_3` //
    expect_equal(x_4 <- 3, 3)
    expect_equal(x_3 <- 4, 4)
    expect_equal(x_4 , 3) ## update (x_4:x_3:4:x_4:3)
    expect_equal(x_4 , 3) ## update (x_4:x_3:3)
    expect_equal(x_3 , 3) ## cache
    ## Double update before explicit request, `x_4`, `x_3`, `x_4` //
    expect_equal(x_4 <- 1, 1)
    expect_equal(x_3 <- 2, 2)
    expect_equal(x_4 , 1) ## update (x_4:x_3:2:x_4:1)
    expect_equal(x_3 , 1) ## cache
    expect_equal(x_4 , 1) ## update (x_4:x_3:1)
    
    ## Clean up //
    rmReactive("x_3")
    rmReactive("x_4")
  }
  
})

} ## End of `if(FALSE)` until refactored

################################################################################
## NEW
################################################################################

##------------------------------------------------------------------------------
context("setShinyReactive/reactive-only")
##------------------------------------------------------------------------------

test_that("setShinyReactive/reactive-only", {

  makeReactiveBinding("x_1")
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
  
  makeReactiveBinding("x_1")
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
  
  makeReactiveBinding("x_1")
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
  
  makeReactiveBinding("x_1")
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
