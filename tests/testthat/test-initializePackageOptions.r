context("initializePackageOptions/basics")
test_that("initializePackageOptions", {
  
  where <- new.env()
  expect_is(res <- initializePackageOptions(where = where), "environment")
  expect_true(exists("options", where, inherits = FALSE))
  expect_is(res <- initializePackageOptions(id = "test", where = where), 
            "environment")
  expect_true(exists("test", where, inherits = FALSE))
  
})
