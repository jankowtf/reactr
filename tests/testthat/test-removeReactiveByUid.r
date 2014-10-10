context("removeReactiveByUid-A")
test_that("removeReactiveByUid", {
  
  skip()
  where = new.env()  
  
  setReactiveS3(id = "x_1", value = 10, where = where, force = TRUE)
  setReactiveS3(id = "x_2", 
                value = function(deps = list(x_1 = where)) {x_1},
                where = where, force = TRUE)
  where$x_1 <- 100
  where$x_2
  
  expect_true(removeReactiveByUid(uid = getReactiveUid(id = "x_1", where = where)))
  expect_false(exists("x_1", envir = where, inherits = FALSE))
  expect_equal(where$x_2, 10)
  setReactiveS3(id = "x_1", value = 100, where = where)
#   expect_error(where$x_2)
#   expect_equal(where$x_2, 100)
  
#   where = new.env()  
  
#   setReactiveS3(id = "x_1", value = 10, where = where)
#   setReactiveS3(id = "x_2", watch = "x_1", where = where, strict = TRUE)
#   expect_true(removeReactiveByUid(id = "x_1", where = where))
#   expect_equal(where$x_2, NULL)

})
