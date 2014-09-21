context("setValue-2")
test_that("setValue", {
  
  where = new.env()  
  id = "x_1"
  value = 10
  
  expect_equal(
    setValue(
      id = id, 
      value = value, 
      where = where, 
      binding = getBoilerplateCode()
    ),
    value
  )
  expect_equal(
    setValue(
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
    setValue(
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
