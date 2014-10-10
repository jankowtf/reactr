context("getHashRegistry-A")
test_that("getHashRegistry", {
  
  options("reactr" = NULL)
  expect_is(res <- getHashRegistry(), "environment")
  expect_equal(res, getOption("reactr")$.hash)
  
})
