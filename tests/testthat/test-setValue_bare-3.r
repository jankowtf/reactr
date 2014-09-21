context("setValue_bare-3")
test_that("setValue_bare", {
  
  .hash_id <- "._HASH"
  where <- new.env()  
  value <- 10 
  
  expect_equal(
    setValue_bare(
      id = "x_1", 
      value = value,
      where = where
    ),
    value
  )
  expect_equal(
    setValue_bare(
      id = "x_2", 
      where = where,
      watch = "x_1"
    ),
    value
  )
  
  expect_equal(where$x_1, value)
  expect_equal(where$x_2, value)
  
  where$x_1 <- 100
  expect_equal(where$x_1, 100)
  expect_equal(where$x_2, 100)
  
  ## Values are ignored if variable monitors another
  expect_warning(
    setValue_bare(
      id = "x_3", 
      value = 20, 
      where = where,
      watch = "x_1"
    )
  )
  expect_equal(where$x_3, where$x_1)
  
  ##----------------------------------------------------------------------------
  ## Mutual dependency //
  ##----------------------------------------------------------------------------
  
  where <- new.env()  
    
  expect_equal(
    setValue_bare(
      id = "x_1", 
      where = where,
      watch = "x_2",
      mutual = TRUE,
#       .tracelevel = 1
    ),
    NULL
  )
  
  expect_equal(
    setValue_bare(
      id = "x_2", 
      where = where,
      watch = "x_1",
      mutual = TRUE,
#       .tracelevel = 1
    ),
    NULL
  )

  ## Initially, all hash values must correspond to the digest of 'NULL' //
  hash_null <- digest::digest(NULL)
  expected <- hash_null
  expect_equal(where[[.hash_id]]$x_1$x_1, expected)
  expect_equal(where[[.hash_id]]$x_1$x_2, expected)
  expect_equal(where[[.hash_id]]$x_2$x_1, expected)
  expect_equal(where[[.hash_id]]$x_2$x_2, expected)
  
  ## Forced initial values //
  expect_equal(where$x_1, NULL)
  expect_equal(where$x_2, NULL)
  
  hash_300 <- digest::digest(300)
  expect_equal(where$x_1 <- 300, 300)
  expect_equal(where[[.hash_id]]$x_1$x_1, hash_300)
  expect_equal(where[[.hash_id]]$x_1$x_2, hash_null)
  expect_equal(where[[.hash_id]]$x_2$x_2, hash_null)
  expect_equal(where[[.hash_id]]$x_2$x_1, hash_null)
  expect_equal(where$x_1, 300)
  expect_equal(where[[.hash_id]]$x_1$x_1, hash_300)
  expect_equal(where[[.hash_id]]$x_1$x_2, hash_null)
  expect_equal(where[[.hash_id]]$x_2$x_2, hash_null)
  expect_equal(where[[.hash_id]]$x_2$x_1, hash_null)
  expect_equal(where$x_2, 300)
  expect_equal(where[[.hash_id]]$x_1$x_1, hash_300)
  expect_equal(where[[.hash_id]]$x_1$x_2, hash_300)
  expect_equal(where[[.hash_id]]$x_2$x_2, hash_300)
  expect_equal(where[[.hash_id]]$x_2$x_1, hash_300)
  hash_500 <- digest::digest(500)
  expect_equal(where$x_2 <- 500, 500)
  expect_equal(where[[.hash_id]]$x_1$x_1, hash_300)
  expect_equal(where[[.hash_id]]$x_1$x_2, hash_300)
  expect_equal(where[[.hash_id]]$x_2$x_2, hash_500)
  expect_equal(where[[.hash_id]]$x_2$x_1, hash_300)
  expect_equal(where$x_2, 500)
  expect_equal(where[[.hash_id]]$x_1$x_1, hash_300)
  expect_equal(where[[.hash_id]]$x_1$x_2, hash_300)
  expect_equal(where[[.hash_id]]$x_2$x_2, hash_500)
  expect_equal(where[[.hash_id]]$x_2$x_1, hash_300)
  expect_equal(where$x_1, 500)
  expect_equal(where[[.hash_id]]$x_1$x_1, hash_500)
  expect_equal(where[[.hash_id]]$x_1$x_2, hash_500)
  expect_equal(where[[.hash_id]]$x_2$x_2, hash_500)
  expect_equal(where[[.hash_id]]$x_2$x_1, hash_500)
  
})
