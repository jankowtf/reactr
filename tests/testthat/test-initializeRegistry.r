context("initializeRegistry/basics")
test_that("initializeRegistry", {
  
  where <- new.env()
  expect_is(res <- initializeRegistry(where = where), "environment")
  expect_true(exists(".registry", where, inherits = FALSE))
  expect_is(res <- initializeRegistry(id = "test", where = where), 
            "environment")
  expect_true(exists("test", where, inherits = FALSE))
  
})
