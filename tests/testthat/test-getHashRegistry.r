context("getHashRegistry-A")
test_that("getHashRegistry", {
  
  expect_equal(getHashRegistry(), getOption("reactr")$.hash)
  
})
