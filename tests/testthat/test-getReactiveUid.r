context("getReactiveUid-A")
test_that("getReactiveUid", {
  
  resetHashRegistry()
  
  id_1 <- "x_1"
  id_2 <- "x_2"
  
  setReactiveS3(id = id_1, value = 10)
  setReactiveS3(id = id_2, value = function() {
    .react_1 <- get(id_1, envir = .GlobalEnv)
  })

  uid_1 <- getReactiveUid(id = id_1, where = .GlobalEnv)
  uid_2 <- getReactiveUid(id = id_2, where = .GlobalEnv)
  
  hash <- getHashRegistry()
  expect_true(exists(uid_1, hash))
  expect_true(exists(uid_2, hash))
  
  on.exit(resetHashRegistry())
  
})
