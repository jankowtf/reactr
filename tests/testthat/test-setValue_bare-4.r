context("setValue_bare-4")
test_that("setValue_bare", {

  if (FALSE) {
  
  suppressWarnings(rm(x_1))
  suppressWarnings(rm(x_2))
  suppressWarnings(rm(.hash))
  
  expect_equal(
    setValue_bare(
      id = "x_1", 
      value = 10
    ),
    10
  )
  expect_equal(
    setValue_bare(
      id = "x_2", 
      watch = "x_1"
    ),
    10
  )
  ## Values are ignored if variable monitors another //
  expect_warning(
    setValue_bare(
      id = "x_3", 
      value = 20, 
      watch = "x_1"
    )
  )
  suppressWarnings(rm(x_1))
  suppressWarnings(rm(x_2))
  suppressWarnings(rm(.hash))
  
  ##----------------------------------------------------------------------------
  ## Mutual dependency //
  ##----------------------------------------------------------------------------
  
  expect_equal(
    setValue_bare(
      id = "x_1", 
      watch = "x_2",
      mutual = TRUE,
#       .tracelevel = 1
    ),
    NULL
  )
  
  expect_equal(
    setValue_bare(
      id = "x_2", 
      watch = "x_1",
      mutual = TRUE,
#       .tracelevel = 1
    ),
    NULL
  )

  ## Initially, all hash values must correspond to the digest of 'NULL' //
  hash_null <- digest::digest(NULL)
  expected <- hash_null
  expect_equal(.hash$x_1$x_1, expected)
  expect_equal(.hash$x_1$x_2, expected)
  expect_equal(.hash$x_2$x_1, expected)
  expect_equal(.hash$x_2$x_2, expected)
  
  ## Initial values //
  expect_equal(x_1, NULL)
  expect_equal(x_2, NULL)
  
  hash_300 <- digest::digest(300)
  expect_equal(x_1 <- 300, 300)
  expect_equal(.hash$x_1$x_1, hash_300)
  expect_equal(.hash$x_1$x_2, hash_null)
  expect_equal(.hash$x_2$x_2, hash_null)
  expect_equal(.hash$x_2$x_1, hash_null)
  expect_equal(x_1, 300)
  expect_equal(.hash$x_1$x_1, hash_300)
  expect_equal(.hash$x_1$x_2, hash_null)
  expect_equal(.hash$x_2$x_2, hash_null)
  expect_equal(.hash$x_2$x_1, hash_null)
  expect_equal(x_2, 300)
  expect_equal(.hash$x_1$x_1, hash_300)
  expect_equal(.hash$x_1$x_2, hash_300)
  expect_equal(.hash$x_2$x_2, hash_300)
  expect_equal(.hash$x_2$x_1, hash_300)
  hash_500 <- digest::digest(500)
  expect_equal(x_2 <- 500, 500)
  expect_equal(.hash$x_1$x_1, hash_300)
  expect_equal(.hash$x_1$x_2, hash_300)
  expect_equal(.hash$x_2$x_2, hash_500)
  expect_equal(.hash$x_2$x_1, hash_300)
  expect_equal(x_2, 500)
  expect_equal(.hash$x_1$x_1, hash_300)
  expect_equal(.hash$x_1$x_2, hash_300)
  expect_equal(.hash$x_2$x_2, hash_500)
  expect_equal(.hash$x_2$x_1, hash_300)
  expect_equal(x_1, 500)
  expect_equal(.hash$x_1$x_1, hash_500)
  expect_equal(.hash$x_1$x_2, hash_500)
  expect_equal(.hash$x_2$x_2, hash_500)
  expect_equal(.hash$x_2$x_1, hash_500)

  suppressWarnings(rm(x_1))
  suppressWarnings(rm(x_2))
  suppressWarnings(rm(.hash))

  }

})
