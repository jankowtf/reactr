context("removeFromRegistryByUid/basics")
test_that("removeFromRegistryByUid", {

  resetRegistry()
  id <- "x_1"
  id_2 <- "x_2"
  
  setReactiveS3(id = id, value = 10)
  setReactiveS3(id = id_2, value = function() {
    .ref_1 = get("x_1")
  })

  uid_1 <- computeObjectUid(id = id)
  uid_2 <- computeObjectUid(id = id_2)
  
  expect_true(removeFromRegistryByUid(uid = uid_1))
  expect_false(exists(uid_1, envir = getRegistry(), inherits = FALSE))
  expect_equal(x_1, 10)
  expect_equal(x_2, x_1)
  expect_true(exists(uid_2, envir = getRegistry(), inherits = FALSE))
  expect_true(removeFromRegistryByUid(uid = uid_2))
  expect_false(exists(uid_2, envir = getRegistry(), inherits = FALSE))
  expect_equal(x_2, x_1)
  
})
