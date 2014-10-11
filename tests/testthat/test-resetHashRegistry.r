context("resetHashRegistry-A")
test_that("resetHashRegistry", {
  
  where <- getOption("reactr")$.hash
  tmp <- rm(list = ls(where, all.names = TRUE), envir = where)
  setReactiveS3(id = "x_1", value = 10)
  setReactiveS3(id = "x_2", value = function() {
    .ref_1 = get("x_1", envir = where)
  })

  hash <- getHashRegistry()
  expect_true(length(ls(hash)) == 2)
  expect_true(resetHashRegistry())
  expect_true(!length(ls(hash)))
  
})
