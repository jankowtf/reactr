##------------------------------------------------------------------------------
context("reactiveExpression")
##------------------------------------------------------------------------------

test_that("reactiveExpression", {

  expect_is(res <- reactiveExpression(x_1 * 2), "ReactiveExpression")
  expect_is(res$fun, "function")
  expect_equal(res$fun, function() x_1 * 2)
  expect_equal(res$label, structure("reactiveExpression(x_1 * 2)", srcfile = ""))
  expect_equal(res$domain, NULL)
  
})
