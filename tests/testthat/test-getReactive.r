context("getThis-1")
test_that("getThis", {

  ## Create example content //
  where <- new.env()
  setReactive(id = "test", value = new.env(), where = where, 
           binding_type = 2)
  expect_is(getThis(id = "test", where = where), "environment")
  
  }
)
