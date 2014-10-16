context("removeHashRegistry-A")
test_that("removeHashRegistry", {
  
  initializeRegistry()
  expect_true(removeHashRegistry())
  expect_equal(getOption("reactr")$.registry, NULL)
  
})
