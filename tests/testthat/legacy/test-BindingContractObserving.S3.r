context("BindingContractObserving.S3_A")
test_that("BindingContractObserving.S3", {

  expect_is(BindingContractObserving.S3(), "BindingContractObserving.S3")
  expect_is(BindingContractObserving.S3(TRUE), "BindingContractObserving.S3")
  expect_is(BindingContractObserving.S3(
      id = "x_2",
      where = new.env(),
      what = "x_1",
      where_what = new.env(),
      binding = function(x) {x}
    ), 
    "BindingContractObserving.S3"
  )
  
})

