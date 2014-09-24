context("getReactive-1")
test_that("getReactive", {

  ## Create example content //
  where <- new.env()
  setReactive(id = "test", value = new.env(), where = where, 
           binding_type = 2)
  expect_is(getReactive(id = "test", where = where), "environment")
  
  }
)
