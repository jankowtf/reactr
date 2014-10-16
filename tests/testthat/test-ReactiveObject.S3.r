context("ReactiveObject.S3_A")
test_that("ReactiveObject.S3", {

  where <- environment()
  expect_is(ReactiveObject.S3(), "ReactiveObject.S3")
  expect_is(ReactiveObject.S3(TRUE), "ReactiveObject.S3")
  
  expect_is(res <- ReactiveObject.S3(
      id = "x_1",
      value = 10
#       where = where
    ), 
    "ReactiveObject.S3"
  )
  expect_equal(res$value, 10)
  expect_equal(res$checksum, digest::digest(10))

  ## Register and unregister in registry //
  resetRegistry()
  registry <- getRegistry()

  expect_true(res$register())
  expect_true(exists(res$uid, registry, inherits = FALSE))
  expect_true(res$unregister())
  expect_false(exists(res$uid, registry, inherits = FALSE))

  ## Remove //
  assign(res$id, "hello world!", res$where)
  expect_equal(x_1, "hello world!")
  expect_true(exists(res$id, res$where, inherits = FALSE))
  expect_true(res$remove())
  expect_false(exists(res$id, res$where, inherits = FALSE))
  expect_error(get("x_1", envir = where, inherits = FALSE))

  ## Copy //
  resetRegistry()
  expect_true(res$register())
  ls(registry)
  res$copy(id = "x_copied")
  ls(registry)
  expect_equal(x_copied, 10)
  
})

test_that("ReactiveObject.S3/setReactiveS3", {

  resetRegistry()
  registry <- getRegistry()
  setReactiveS3(id = "x_1", value = 10)
  obj <- getFromRegistry("x_1")

  ## Copy //
  obj$copy(id = "x_1_copied")
  expect_true(exists("x_1_copied"))
  expect_equal(x_1_copied, 10)
  expect_equal(x_1_copied, x_1)
  x_1 <- 100
  expect_equal(x_1_copied, 10)
  ## --> independent
  expect_false(identical(getFromRegistry("x_1"), getFromRegistry("x_1_copied")))
  
  ## Unset //
  env <- environment()
  expect_true(bindingIsActive("x_1", env))
  obj$unset()
  expect_false(bindingIsActive("x_1", env))
  expect_false(exists(obj$uid, registry))

})
