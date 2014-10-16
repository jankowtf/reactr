context("removeFromRegistry-1")
test_that("removeFromRegistry", {
  
  ## CAUTION //
  ## Order really matters here!
  
  .hash_id <- "._HASH"
  where <- new.env()  
  id <- "x_1"
  watch <- "x_2"
  
  setReactive(id = id, value = 10, where = where)
  setReactive(id = watch, watch = id, where = where, .tracelevel = 0)

  expect_true(removeFromRegistry(id = id, where = where))
  expect_false(exists(id, envir = where[[.hash_id]], inherits = FALSE))
  expect_equal(where$x_1, 10)
  expect_equal(where$x_2, where$x_1)
  expect_false(exists(id, envir = where[[.hash_id]][[watch]], inherits = FALSE))
  expect_equal(where$x_2, where$x_1)
  
})
