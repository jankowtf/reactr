##------------------------------------------------------------------------------
context("showPushRefs/basics")
##------------------------------------------------------------------------------

test_that("showPushRefs", {

  setReactiveS3(id = "x_1", value = 10)
  setReactiveS3(id = "x_2", value = function() "object-ref: {id: x_1}")
  expect_equal(res <- showPushRefs(id = "x_1"), ls(getFromRegistry("x_1")$.refs_push))  
  expect_true(!length(res))  
  expect_equal(res <- showPushRefs(id = "x_2"), ls(getFromRegistry("x_2")$.refs_push))
  expect_true(!length(res))  
  
  setReactiveS3(id = "x_2", value = function() "object-ref: {id: x_1}", push = TRUE)
  expect_equal(res <- showPushRefs(id = "x_2"), ls(getFromRegistry("x_2")$.refs_push))
  expect_false(!length(res))  
  
  ## Clean up //
  rmReactive("x_1")
  rmReactive("x_2")
  
})
