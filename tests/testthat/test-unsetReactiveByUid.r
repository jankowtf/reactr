context("unsetReactiveByUid/basics")
test_that("unsetReactiveByUid", {
  
  setReactive(id = "x_1", value = 10)
  setReactive(id = "x_2", value = function() "object-ref: {id: x_1}")

  expect_true(unsetReactiveByUid(uid = computeObjectUid("x_1")))
  expect_equal(x_1, 10)
  expect_equal(x_2, x_1)
  x_1 <- 20
  expect_equal(x_1, 20)
  expect_equal(x_2, 10)
  ## --> no active reactive reletionship anymore
  expect_false(isReactive("x_1"))
  
  ## Reset referenced object //
  setReactive(id = "x_1", value = 50)
  expect_equal(x_2, x_1)
  
  ## Clean up //
  rm(x_1)
  rm(x_2)
  resetRegistry()
  
})

test_that("unsetReactiveByUid/strictness when invalidation (1)", {
  
  setReactive(id = "x_1", value = 10)
  setReactive(id = "x_2", value = function() "object-ref: {id: x_1}",
                strict_get = 1)

  expect_true(unsetReactiveByUid(uid = computeObjectUid("x_1")))
  expect_equal(x_1, 10)
  expect_warning(expect_equal(x_2, NULL))
  
  ## Reset referenced object //
  setReactive(id = "x_1", value = 50)
  expect_equal(x_2, x_1)
  
  ## Clean up //
  rm(x_1)
  rm(x_2)
  resetRegistry()
  
})

test_that("unsetReactiveByUid/strictness when invalidation (2)", {
  
  setReactive(id = "x_1", value = 10)
  setReactive(id = "x_2", value = function() "object-ref: {id: x_1}",
                strict_get = 2)

  expect_true(unsetReactiveByUid(uid = computeObjectUid("x_1")))
  expect_equal(x_1, 10)
  expect_error(x_2)
  
  ## Reset referenced object //
  setReactive(id = "x_1", value = 50)
  expect_equal(x_2, x_1)
  
  ## Clean up //
  rm(x_1)
  rm(x_2)
  resetRegistry()
  
})
