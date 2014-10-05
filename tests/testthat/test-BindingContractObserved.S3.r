context("BindingContractObserved.S3_A")
test_that("BindingContractObserved.S3", {

  expect_is(BindingContractObserved.S3(), "BindingContractObserved.S3")
  expect_is(BindingContractObserved.S3(TRUE), "BindingContractObserved.S3")
  expect_is(BindingContractObserved.S3(binding = function(x) {x}), 
            "BindingContractObserved.S3")
  
  }
)

