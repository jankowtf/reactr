context("initializeHashRegistry-A")
test_that("initializeHashRegistry", {
  
  options("reactr" = NULL)
  expect_true(is.null(getOption("reactr")))
  expect_true(initializeHashRegistry())
  expect_is(getOption("reactr")$.hash, "environment")  
  
})
