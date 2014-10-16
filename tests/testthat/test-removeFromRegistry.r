context("removeFromRegistry/basics")
test_that("removeFromRegistry", {

  id <- "x_1"
  id_2 <- "x_2"
  
  setReactiveS3(id = id, value = 10)
  setReactiveS3(id = id_2, value = function() 
    .ref_1 <- get("x_1")
  )
  
  expect_true(removeFromRegistry(id = id))
  uid <- getObjectUid(id = id)
  expect_false(exists(uid, envir = getRegistry(), inherits = FALSE))
  expect_equal(x_1, 10)
  expect_equal(x_2, x_1)
  
})
