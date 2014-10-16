context("getRegistry-A")
test_that("getRegistry", {
  
  options("reactr" = NULL)
  expect_is(res <- getRegistry(), "environment")
  expect_equal(res, getOption("reactr")$.registry)
  
})
