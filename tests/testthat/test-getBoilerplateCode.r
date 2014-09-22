context("getBoilerplateCode-1")
test_that("getBoilerplateCode", {

  expect_is(res <- getBoilerplateCode(
    ns = classr::createInstance(cl = "Reactr.BindingContractMonitored.S3")), 
    "call"
  )
  expect_is(res <- getBoilerplateCode(
    ns = classr::createInstance(cl = "Reactr.BindingContractMonitoring.S3")), 
    "call"
  )
  expect_is(res <- getBoilerplateCode(
    ns = classr::createInstance(cl = "Reactr.BindingContractMutual.S3")), 
    "call"
  )
  
  }
)
