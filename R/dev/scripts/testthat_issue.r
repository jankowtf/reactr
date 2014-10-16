test_that("Example test", {
  foo <- function(id, value, where) {
    message("doing what needs to be done with respect to reactivity")
  }
  
  ## Actual test //
  x_1 <- "hello world"
  
  id_1 <- "x_1"
  ## --> defined in order not to repeat myself while testing --> 'id_1' is 
  ## reused in other places of the same 'test_that' call
  foo(id = id_2, value = function() {
    .ref_1 <- get(x = id_1)
  })
  ## --> works as long as manually stepping through the code in 'test_that'
  ## But it will fail once you remove 'id_1' and source the whole test file
})
