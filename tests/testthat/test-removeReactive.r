context("removeReactive-1")
test_that("removeReactive: non-strict", {
  
  where = new.env()  
  
  setReactiveS3(id = "x_1", value = 10, where = where)
  setReactiveS3(id = "x_2", value = function(refs = list(x_1 = where)) x_1, 
                where = where)

  expect_true(removeReactive(id = "x_1", where = where))
  expect_false(exists("x_1", envir = where, inherits = FALSE))
  expect_equal(where$x_2, 10)
  setReactiveS3(id = "x_1", value = 100, where = where)
#   expect_equal(where$x_2, NULL)  
#   expect_equal(where$x_2, 100)
})

test_that("removeReactive: non-strict", {
  
  where = new.env()  
  
  setReactiveS3(id = "x_1", value = 10, where = where)
  setReactiveS3(id = "x_2", value = function(refs = list(x_1 = where)) x_1, 
                where = where, strict = TRUE)
  expect_true(removeReactive(id = "x_1", where = where))
#   expect_error(where$x_2)

})
