context("unsetReactive/basics")
test_that("unsetReactive", {
  
  setReactive(id = "x_1", value = 10)
  setReactive(id = "x_2", value = function() "object-ref: {id: x_1}")

  expect_true(unsetReactive(id = "x_1"))
  expect_equal(x_1, 10)
  expect_equal(x_2, x_1)
  x_1 <- 20
  expect_equal(x_1, 20)
  expect_equal(x_2, 10)
  expect_false(exists(computeObjectUid(id = "x_1"), 
                      getRegistry(), inherits = FALSE))
  
})
