context("removeHashRegistry-A")
test_that("removeHashRegistry", {
  
  initializeHashRegistry()
  expect_true(removeHashRegistry())
  expect_equal(getOption("reactr")$.hash, NULL)
  
})
