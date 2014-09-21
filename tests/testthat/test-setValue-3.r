context("setValue-3")
test_that("setValue", {
  
  where <- new.env()  
    
  expect_equal(
    setValue(
      id = "x_1", 
      value = 10, 
      where = where
    ),
    10
  )
  
  expect_equal(
    setValue(
      id = "x_2", 
      value = 20, 
      where = where,
      watch = "x_1"
    ),
    NULL
  )
  
  ##----------------------------------------------------------------------------
  
  where <- new.env()  
    
  expect_equal(
    setValue(
      id = "x_1", 
      value = 10,
      where = where,
      watch = "x_2"
    ),
#     10
    NULL
  )
  
  where$x_1
  ## Error when not set via 'setValue' //
#   where$x_2 <- 1
#   expect_error(where$x_1) ## not anymore because forced hash
  
  expect_equal(
    setValue(
      id = "x_2", 
      value = 100,
      where = where,
      watch = "x_1"
    ),
#     100
    NULL
  )

  where$.hash$x_1$x_1
  where$.hash$x_1$x_2
  where$.hash$x_2$x_1
  where$.hash$x_2$x_2
  ## Forced initial values //
  expect_equal(where$x_1, NULL)
  expect_equal(where$x_2, NULL)
  expect_equal(where$x_1 <- 300, 300)
  expect_equal(where$x_1, 300)
  expect_equal(where$x_2, 300)
  expect_equal(where$x_2 <- 500, 500)
  expect_equal(where$x_2, 500)
  expect_equal(where$x_1, 500)
  
  ## TODO: just need to get the hash behavior when forcing values right, 
  ## then it should be all set!
  
})
