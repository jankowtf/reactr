context("getFromRegistryByUid/basics")
test_that("getFromRegistryByUid", {

  skip("manual only due to environment issues")
  resetRegistry()
  where <- environment()
  obj <- ReactiveObject.S3(id = "x", value = 10, where = where)
  res$.register()
  
  expect_equal(getFromRegistryByUid(obj$.uid), obj)
  resetRegistry()
  
})
