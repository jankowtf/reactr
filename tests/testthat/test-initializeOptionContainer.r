##------------------------------------------------------------------------------
context("initializeOptionContainer/basics")
##------------------------------------------------------------------------------

test_that("initializeOptionContainer", {
  
  id <- "reactr"
  options("reactr" = NULL)
  expect_true(is.null(getOption(id)))
  expect_is(initializeOptionContainer(id), "environment")
  expect_is(getOption(id)$options, "environment")  
  expect_is(getOption(id)$.registry, "environment")  
  
})
