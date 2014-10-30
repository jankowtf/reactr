##------------------------------------------------------------------------------
context("reactiveBinding/default")
##------------------------------------------------------------------------------

test_that("reactiveBinding/1", {

  expect_is(res <- reactiveBinding(x_1 * 2), "ReactiveBinding")
  expect_is(res$fun, "function")
  expect_equal(res$fun, function() x_1 * 2)
  expect_equal(res$label, structure("reactiveBinding(x_1 * 2)", srcfile = ""))
  expect_equal(res$domain, NULL)
  
})
