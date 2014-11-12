context("rmFromRegistry/basics")
test_that("rmFromRegistry", {

  id <- "x_1"
  id_2 <- "x_2"
  
  setReactive(id = id, value = 10)
  setReactive(id = id_2, value = function() 
    .ref_1 <- get("x_1")
  )
  
  expect_true(rmFromRegistry(id = id))
  uid <- computeObjectUid(id = id)
  expect_false(exists(uid, envir = getRegistry(), inherits = FALSE))
  expect_equal(x_1, 10)
  expect_equal(x_2, x_1)
  
})
