context("removeFromHashRegistry-A")
test_that("removeFromHashRegistry", {

  where <- new.env()  
  id <- "x_1"
  id_2 <- "x_2"
  
  setReactiveS3(id = id, value = 10, where = where)
  setReactiveS3(id = id_2, value = function() 
    .ref_1 <- get("x_1", where),
  where = where
  )
  
  expect_true(removeFromHashRegistry(id = id, where = where))
  uid <- getReactiveUid(id = id, where = where)
  expect_false(exists(uid, envir = getHashRegistry(), inherits = FALSE))
  expect_equal(where$x_1, 10)
  expect_equal(where$x_2, where$x_1)
  
})
