context("resetRegistry-A")
test_that("resetRegistry", {
  
  where <- getOption("reactr")$.registry
  tmp <- rm(list = ls(where, all.names = TRUE), envir = where)
  setReactiveS3(id = "x_1", value = 10)
  setReactiveS3(id = "x_2", value = function() {
    .ref_1 = get("x_1", envir = where)
  })

  registry <- getRegistry()
  expect_true(length(ls(registry)) == 2)
  expect_true(resetRegistry())
  expect_true(!length(ls(registry)))
  
})
