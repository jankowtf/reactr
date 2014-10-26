context("showRegistry/basics")
test_that("showRegistry", {
  
  expect_is(res <- showRegistry(), "character")
  
})
