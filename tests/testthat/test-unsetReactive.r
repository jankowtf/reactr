context("unsetReactive-1")
test_that("unsetReactive", {
  
  .hash_id <- "._HASH"
  where = new.env()  
  
  setReactive(id = "x_1", value = 10, where = where)
  setReactive(id = "x_2", watch = "x_1", where = where)

  expect_true(unsetReactive(id = "x_1", where = where))
  expect_equal(where$x_1, 10)
  expect_equal(where$x_2, where$x_1)
  where$x_1 <- 20
  expect_equal(where$x_1, 20)
  expect_equal(where$x_2, 10)
  
})
