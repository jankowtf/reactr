context("getValue-1")
test_that("getValue", {

  ## Create example content //
  envir <- new.env()
  setValue(id = "test", value = new.env(), envir = envir, 
           binding_type = 2)
  expect_is(getValue(id = "test", envir = envir), "environment")
  
  }
)
