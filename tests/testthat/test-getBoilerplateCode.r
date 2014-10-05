context("getBoilerplateCode-1")
test_that("getBoilerplateCode", {

  expect_is(res <- getBoilerplateCode(
    ns = reactr::BindingContractObserved.S3()), 
    "call"
  )
  expect_is(res <- getBoilerplateCode(
    ns = reactr::BindingContractObserving.S3()), 
    "call"
  )
  expect_is(res <- getBoilerplateCode(
    ns = reactr::BindingContractMutual.S3()), 
    "call"
  )
  
  }
)
