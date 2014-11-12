context("rmReactiveByUid/basics")
test_that("rmReactiveByUid/strict_get=0", {
  
  where <- environment()
  setReactive(id = "x_1", value = 10)
  setReactive(id = "x_2", value = function() "object-ref: {id: x_1}")

  expect_true(rmReactiveByUid(uid = computeObjectUid("x_1")))
  expect_false(exists("x_1", envir = where, inherits = FALSE))
  expect_equal(x_2, 10)
  setReactive(id = "x_1", value = 100)
  expect_equal(x_2, 100)
  
})

test_that("rmReactiveByUid/strict_get=1", {
  
  setReactive(id = "x_1", value = 10)
  setReactive(id = "x_2", value = function() "object-ref: {id: x_1}",
                strict_get = 1)
  expect_true(rmReactiveByUid(uid = computeObjectUid("x_1")))
  expect_warning(x_2)

})

test_that("rmReactiveByUid/strict_get=2", {
  
  setReactive(id = "x_1", value = 10)
  setReactive(id = "x_2", value = function() "object-ref: {id: x_1}",
                strict_get = 2)
  expect_true(rmReactiveByUid(uid = computeObjectUid("x_1")))
  expect_error(x_2)

})
