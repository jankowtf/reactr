context("showRegistryContent/basics")
test_that("showRegistryContent", {
  
  expect_is(res <- showRegistryContent(), "character")
  
})
