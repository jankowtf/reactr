context("ensureHashRegistryState-1")
test_that("ensureHashRegistryState", {

  .hash_id <- "._HASH"
  where <- new.env()
  
  expect_true(res <- ensureHashRegistryState(id = "test", where = where))
  expect_true(ls(where[[.hash_id]]) == "test")
  expect_true(all(ls(where[[.hash_id]]$test) %in% c("test")))
  expect_equal(where[[.hash_id]]$test$test, digest::digest(NULL))
  
  where <- new.env()
  
  expect_true(res <- ensureHashRegistryState(id = "test", 
    watch = "watch", where = where))
  expect_true(ls(where[[.hash_id]]) == "watch")
  expect_true(all(ls(where[[.hash_id]]$watch) %in% c("test", "watch")))
  expect_equal(where[[.hash_id]]$watch$watch, digest::digest(NULL))
  expect_equal(where[[.hash_id]]$watch$test, digest::digest(NULL))
  
  where <- new.env()
  
  .hash_id <- ".HASH"
  expect_true(res <- ensureHashRegistryState(id = "test", where = where,
                                             .hash_id = .hash_id))
  expect_true(ls(where[[.hash_id]]) == "test")
  expect_true(all(ls(where[[.hash_id]]$test) %in% c("test")))
  expect_equal(where[[.hash_id]]$test$test, digest::digest(NULL))
  
  where <- new.env()
  
  expect_true(res <- ensureHashRegistryState(id = "test", 
    watch = "watch", where = where, .hash_id = .hash_id))
  expect_true(ls(where[[.hash_id]]) == "watch")
  expect_true(all(ls(where[[.hash_id]]$watch) %in% c("test", "watch")))
  expect_equal(where[[.hash_id]]$watch$watch, digest::digest(NULL))
  expect_equal(where[[.hash_id]]$watch$test, digest::digest(NULL))
  
  }
)
