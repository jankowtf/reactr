##------------------------------------------------------------------------------
context("Observable3")
##------------------------------------------------------------------------------

test_that("Observable3", {

  expect_is(
    res <- Observable3$new(
      id = "x_1", 
      value = 10,
      where = environment(),
      func = NULL
    ), 
    "Observable3"
  )
  
  expect_equal(res$.value, 10)
  expect_equal(res$.checksum, digest::digest(10))

  ## Register and unregister in registry //
  resetRegistry()
  registry <- getRegistry()

  expect_true(res$.register())
  expect_true(exists(res$.uid, registry, inherits = FALSE))
  expect_true(res$.unregister())
  expect_false(exists(res$.uid, registry, inherits = FALSE))

  ## Remove //
  assign(res$.id, "hello world!", res$.where)
  expect_equal(x_1, "hello world!")
  expect_true(exists(res$.id, res$.where, inherits = FALSE))
  expect_true(res$.remove())
  expect_false(exists(res$.id, res$.where, inherits = FALSE))
  expect_error(get("x_1", inherits = FALSE))

  ## Copy //
  resetRegistry()
  expect_true(res$.register())
#   showRegistry()
  expect_equal(res$.copy(id = "x_copied"), 10)
#   showRegistry()
  expect_equal(x_copied, 10)

})


test_that("Observable3/setReactiveS3", {

  resetRegistry()
  registry <- getRegistry()
  setShinyReactive(id = "x_1", value = 10)
  obj <- getFromRegistry("x_1")
  expect_is(obj, "Observable3")

  ## Copy //
  obj$.copy(id = "x_1_copied")
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
  obj$.unset()
  expect_false(bindingIsActive("x_1", env))
  expect_false(exists(obj$.uid, registry))
  
  ## Clean up //
  removeReactive("x_1")

})
