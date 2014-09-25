context("removeReactive-1")
test_that("removeReactive", {
  
  where = new.env()  
  
  setReactive(id = "x_1", value = 10, where = where)
  setReactive(id = "x_2", watch = "x_1", where = where)

  expect_true(removeReactive(id = "x_1", where = where))
  expect_false(exists("x_1", envir = where, inherits = FALSE))
  expect_equal(where$x_2, 10)
  setReactive(id = "x_1", value = 100, where = where)
  expect_error(where$x_2)
#   expect_equal(where$x_2, 100)
  
  where = new.env()  
  
  setReactive(id = "x_1", value = 10, where = where)
  setReactive(id = "x_2", watch = "x_1", where = where, strict = TRUE)
  expect_true(removeReactive(id = "x_1", where = where))
  expect_equal(where$x_2, NULL)

})
