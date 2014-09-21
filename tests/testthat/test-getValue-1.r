context("getValue-1")
test_that("getValue", {

  ## Create example content //
  where <- new.env()
  setValue(id = "test", value = new.env(), where = where, 
           binding_type = 2)
  expect_is(getValue(id = "test", where = where), "environment")
  
  }
)
