context("BindingContractMutual.S3_A")
test_that("BindingContractMutual.S3", {

  expect_is(BindingContractMutual.S3(), "BindingContractMutual.S3")
  expect_is(BindingContractMutual.S3(TRUE), "BindingContractMutual.S3")
  expect_is(BindingContractMutual.S3(
      id = "x_2",
      where = new.env(),
      what = "x_1",
      where_what = new.env(),
      binding = function(x) {x}
    ), 
    "BindingContractMutual.S3"
  )
  
})

