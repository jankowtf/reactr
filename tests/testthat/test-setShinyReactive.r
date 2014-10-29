## Globals //
verbose <- FALSE

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
  removeReactive("x_1")
  removeReactive("x_2")

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
  removeReactive("x_1")
  removeReactive("x_2")
  
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
  removeReactive("x_1")
  removeReactive("x_2")
  
  ##----------
  
  if (FALSE) {
    setReactiveS3(id = "x_3", value = function() {
      "object-ref: {id: x_4}"
      x_4
    }, verbose = verbose)
    setReactiveS3(id = "x_4", value = function() {
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
    removeReactive("x_3")
    removeReactive("x_4")
    
    setReactiveS3(id = "x_3", value = function() {
      "object-ref: {id: x_4}"
      x_4
    }, verbose = verbose)
    setReactiveS3(id = "x_4", value = function() {
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
    removeReactive("x_3")
    removeReactive("x_4")
  }
  
})
  
