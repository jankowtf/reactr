context("unsetReactive-A")
test_that("unsetReactive", {
  
  where <- new.env()
  setReactiveS3(id = "x_1", value = 10, where = where)
  setReactiveS3(id = "x_2", value = function(refs = list(x_1 = where)) x_1, 
                where = where)

  expect_true(unsetReactive(id = "x_1", where = where))
  expect_equal(where$x_1, 10)
  expect_equal(where$x_2, where$x_1)
  where$x_1 <- 20
  expect_equal(where$x_1, 20)
  expect_equal(where$x_2, 10)
  expect_false(exists(getReactiveUid(id = "x_1", where = where), 
                      getHashRegistry(), inherits = FALSE))
  
})
