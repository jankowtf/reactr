context("unsetReactiveByUid-A")
test_that("unsetReactiveByUid", {
  
  where = new.env()  
  
  setReactiveS3(id = "x_1", value = 10, where = where)
  setReactiveS3(id = "x_2", 
                value = function(deps = list(x_1 = where)) x_1,
                where = where)

  expect_true(unsetReactiveByUid(uid = getReactiveUid("x_1", where)))
  expect_equal(where$x_1, 10)
  expect_equal(where$x_2, where$x_1)
  where$x_1 <- 20
  expect_equal(where$x_1, 20)
  expect_equal(where$x_2, 10)
  
})
