context("removeFromHashRegistryByUid-A")
test_that("removeFromHashRegistryByUid", {

  resetHashRegistry()
  id <- "x_1"
  id_2 <- "x_2"
  where <- environment()
  
  setReactiveS3(id = id, value = 10, where = where)
  setReactiveS3(id = id_2, value = function() {
    .ref_1 = get("x_1", envir = where)
  }, where = where)

  uid_1 <- getReactiveUid(id = id, where = where)
  uid_2 <- getReactiveUid(id = id_2, where = where)
  
  expect_true(removeFromHashRegistryByUid(uid = uid_1))
  expect_false(exists(uid_1, envir = getHashRegistry(), inherits = FALSE))
  expect_equal(x_1, 10)
  expect_equal(x_2, x_1)
  expect_true(exists(uid_2, envir = getHashRegistry(), inherits = FALSE))
  expect_true(removeFromHashRegistryByUid(uid = uid_2))
  expect_false(exists(uid_2, envir = getHashRegistry(), inherits = FALSE))
  expect_equal(x_2, x_1)
  
})
