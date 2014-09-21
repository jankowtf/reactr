context("setValue_bare-2")
test_that("setValue_bare", {
  
  where = new.env()  
  id = "x_1"
  value = 10
  
  expect_equal(
    setValue_bare(
      id = id, 
      value = value, 
      where = where
    ),
    value
  )
  expect_equal(
    setValue_bare(
      id = id, 
      value = value, 
      where = where
    ),
    value
  )

  binding <- function(x, ...) {
    x + 100
  }
  
  watch = "x_1"
#   binding(x = where[[watch]])

  expected <- binding(value)
  expect_equal(
    setValue_bare(
      id = "x_2", 
      where = where, 
      watch = "x_1", 
      binding = binding, 
      binding_type = 1
    ),
    expected
  )
  
  expect_equal(where$x_1, value)
  expect_equal(where$x_2, binding(where$x_1))
  where$x_1 <- 100
  expect_equal(where$x_2, binding(where$x_1))

})
