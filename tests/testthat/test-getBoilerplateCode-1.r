context("getBoilerplateCode-1")
test_that("getBoilerplateCode", {

  expect_is(res <- getBoilerplateCode(
    ns = classr::createClassInstance(cl = "Reactr.BindingContractSet.S3")), 
    "call"
  )
  expect_is(res <- getBoilerplateCode(
    ns = classr::createClassInstance(cl = "Reactr.BindingContractGet.S3")), 
    "call"
  )
  expect_is(res <- getBoilerplateCode(
    ns = classr::createClassInstance(cl = "Reactr.BindingContractCombined.S3")), 
    "call"
  )
  
  }
)
