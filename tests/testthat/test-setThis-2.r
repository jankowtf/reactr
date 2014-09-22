context("setThis-2")
test_that("setThis", {
  
  where = new.env()  
  id = "x_1"
  value = 10
  
  expect_equal(
    setThis(
      id = id, 
      value = value, 
      where = where
    ),
    value
  )
  expect_equal(
    setThis(
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
    setThis(
      id = "x_2", 
      where = where, 
      watch = "x_1", 
      binding = binding, 
      binding_type = 1
    ),
    expected
  )
  where$x_1  
  where$x_2
  where$x_1 <- 100
  where$x_2

})
