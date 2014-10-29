context("rmReactive/basics")
test_that("rmReactive/strict_get=0", {
  
  where <- environment()
  setReactiveS3(id = "x_1", value = 10)
  setReactiveS3(id = "x_2", value = function() "object-ref: {id: x_1}")

  expect_true(rmReactive(id = "x_1"))
  expect_false(exists("x_1", envir = where, inherits = FALSE))
  expect_equal(x_2, 10)
  setReactiveS3(id = "x_1", value = 100)
  expect_equal(x_2, 100)
  
})

test_that("rmReactive/strict_get=1", {
  
  setReactiveS3(id = "x_1", value = 10)
  setReactiveS3(id = "x_2", value = function() "object-ref: {id: x_1}",
                strict_get = 1)
  expect_true(rmReactive(id = "x_1"))
  expect_warning(x_2)

})

test_that("rmReactive/strict_get=2", {
  
  setReactiveS3(id = "x_1", value = 10)
  setReactiveS3(id = "x_2", value = function() "object-ref: {id: x_1}",
                strict_get = 2)
  expect_true(rmReactive(id = "x_1"))
  expect_error(x_2)

})
