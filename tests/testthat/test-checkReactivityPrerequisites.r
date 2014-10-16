context("checkReactivityPrerequisites-A")
test_that("checkReactivityPrerequisites: default", {
  
  resetRegistry()
  input <- ReactiveObject.S3(
    id = "x_1",
    value = NULL,
    where = environment()
  )
  suppressWarnings(rm(x_1))
  expect_error(checkReactivityPrerequisites(input = input, strict = 2))
  
  x_1 <- 10
  expect_error(checkReactivityPrerequisites(input = input, strict = 2))
  
  expect_true(checkReactivityPrerequisites(input = input))
  
})
