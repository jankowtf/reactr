context("setReactive_bare-1")
test_that("setReactive_bare", {
  
  .hash_id <- "._HASH"
  where = new.env()  
  
  id <- "x_1"
  value <- Sys.time()
  expect_equal(
    setReactive_bare(id = "x_1", value = value, where = where, binding_type = 2),
    value
  )
  
  expect_equal(
    ls(where, all.names = TRUE),
    sort(c(".bindings", "._HASH", ".watch", "x_1"))
  )
  expect_equal(ls(where$.bindings), character())
  expect_equal(ls(where[[.hash_id]]), id)
  expect_equal(ls(where[[.hash_id]][[id]]), id)
  
  id <- "x_2"
  binding <- substitute(function(x) {
    x + 60*60*24
  })
  watch <- "x_1"
  expected <- eval(binding)(x = value)
  expect_equal(
    setReactive_bare(id = id, where = where, binding = binding, watch = watch,
             binding_type = 2),
    expected
  )
  
  expect_equal(
    ls(where, all.names = TRUE),
    sort(c(".bindings", "._HASH", ".watch", "x_1", "x_2"))
  )
  expect_equal(ls(where$.bindings), id)
  expect_equal(ls(where[[.hash_id]]), c("x_1", id))
  expect_equal(ls(where[[.hash_id]][[id]]), id)
  expect_equal(ls(where[[.hash_id]][["x_1"]]), c("x_1", id))
  
  id <- "x_2"
  expect_equal(getReactive(id = id, where = where), expected)
  
  ## Change watch value //
  value = Sys.time()
  expect_equal(
    setReactive_bare(id = "x_1", value = value, where = where, binding_type = 2),
    value
  )
  expect_true(where[[.hash_id]]$x_1$x_1 != where[[.hash_id]]$x_1$x_2)
  
  id <- "x_2"
  expect_equal(
    getReactive(id = id, where = where, binding_type = 2), 
    eval(binding)(x = value)
  )

  ##----------------------------------------------------------------------------
  ## Setting via 'makeActiveBinding' //
  ##----------------------------------------------------------------------------
  
  ## Variable that can be monitored //
  where <- new.env()
  value <- 10
  
  expect_equal(
    setReactive_bare(
      id = "x_1", 
      value = value, 
      where = where, 
      binding_type = 1
    ),
    value
  )
  expect_equal(where$x_1, value)
  expect_equal(where[[.hash_id]]$x_1$x_1, digest::digest(value))

  ## Simple assignment //
  value <- 100
  expect_equal(where$x_1 <- value, value)
  expect_equal(where$x_1, value)
  expect_equal(where[[.hash_id]]$x_1$x_1, digest::digest(value))

  ## Set again //
  value <- 10
  expect_equal(
    setReactive_bare(
      id = "x_1", 
      value = value, 
      where = where, 
      binding_type = 1
    ),
    value
  )
  expect_equal(where[[.hash_id]]$x_1$x_1, digest::digest(value))

  ## Set variable that monitors 'x_1' //
  expected <- value + 100
  binding <- function(x) {x + 100}
  
  expect_equal(
    setReactive_bare(
      id = "x_2", 
      where = where, 
      watch = "x_1", 
      binding = binding,
      binding_type = 1
    ),
    expected
  )

  expect_equal(where$x_1, value)
  expect_equal(where$x_2, expected)
  expect_equal(where$x_2, expected)

#   sapply(ls(where[[.hash_id]]$x_1), get, envir = where[[.hash_id]]$x_1)
  expect_equal(where[[.hash_id]]$x_1$x_1, digest::digest(value))
  expect_equal(where[[.hash_id]]$x_1$x_2, where[[.hash_id]]$x_1$x_1)
  
  ## Simple assignment //
  value <- 100
  expected <- binding(value)
  expect_equal(where$x_1 <- value, value)
  expect_equal(where$x_1, value)
  expect_equal(where$x_2, expected)
#   sapply(ls(where[[.hash_id]]$x_1), get, envir = where[[.hash_id]]$x_1)
  expect_equal(where[[.hash_id]]$x_1$x_1, digest::digest(value))
  expect_equal(where[[.hash_id]]$x_1$x_2, where[[.hash_id]]$x_1$x_1)
  expect_equal(where$x_2, expected)
  
  ## Set again //
  value <- 500
  expected <- value
  expect_equal(
    setReactive_bare(
      id = "x_1", 
      value = value, 
      where = where, 
      binding_type = 1
    ),
    expected
  )
  
  ## Change binding contract //
  binding <- function(x) {x + 200}
  expected <- binding(x = where$x_1)

  expect_equal(
    setReactive_bare(
      id = "x_2", 
      where = where, 
      watch = "x_1",
      binding = binding,
      binding_type = 1
    ),
    expected
  )
  
  expect_equal(where$x_2, expected)
  expect_equal(where[[.hash_id]]$x_1$x_1, digest::digest(value))
  expect_equal(where[[.hash_id]]$x_1$x_2, where[[.hash_id]]$x_1$x_2)
  
  ##----------------------------------------------------------------------------
  ## Profiling //
  ##----------------------------------------------------------------------------

  if (TRUE) {
    require(microbenchmark)
    
    where <- new.env()  
    res_1 <- microbenchmark(
      "1" = setReactive_bare(id = "x_1", value = Sys.time(), where = where,
                     binding_type = 2),
      "2" = getReactive(id = "x_1", where = where),
      "3" = setReactive_bare(id = "x_2", where = where,
        binding = substitute(function(x) {
            x + 60*60*24
          }), watch = "x_1", binding_type = 2),
      "4" = getReactive(id = "x_2", where = where),
      "5" = setReactive_bare(id = "x_1", value = Sys.time(), where = where,
                     binding_type = 2),
      "6" = getReactive(id = "x_2", where = where),
      control = list(order = "inorder")
    )
    res_1
    
    ##-----------
    
    where <- new.env()
    
    res_2 <- microbenchmark(
      "1" = setReactive_bare(id = "x_1", value = 10, where = where),
      "2" = getReactive(id = "x_1", where = where),
      "3" = setReactive_bare(id = "x_2", where = where, watch = "x_1",
        binding = function(x) {x + 100}),
      "4" = getReactive(id = "x_2", where = where),
      "5" = setReactive_bare(id = "x_1", value = 100, where = where),
      "6" = getReactive(id = "x_2", where = where),
      control = list(order = "inorder")
    )
    res_2
  }
  
})

##------------------------------------------------------------------------------

context("setReactive_bare-2")
test_that("setReactive_bare", {
  
  where = new.env()  
  id = "x_1"
  value = 10
  
  expect_equal(
    setReactive_bare(
      id = id, 
      value = value, 
      where = where
    ),
    value
  )
  expect_equal(
    setReactive_bare(
      id = id, 
      value = value, 
      where = where
    ),
    value
  )

  binding <- function(x, ...) {
    x + 100
  }
  
  watch = "x_1"
#   binding(x = where[[watch]])

  expected <- binding(value)
  expect_equal(
    setReactive_bare(
      id = "x_2", 
      where = where, 
      watch = "x_1", 
      binding = binding, 
      binding_type = 1
    ),
    expected
  )
  
  expect_equal(where$x_1, value)
  expect_equal(where$x_2, binding(where$x_1))
  where$x_1 <- 100
  expect_equal(where$x_2, binding(where$x_1))

})

##------------------------------------------------------------------------------

context("setReactive_bare-3")
test_that("setReactive_bare", {
  
  .hash_id <- "._HASH"
  where <- new.env()  
  value <- 10 
  
  expect_equal(
    setReactive_bare(
      id = "x_1", 
      value = value,
      where = where
    ),
    value
  )
  expect_equal(
    setReactive_bare(
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
    setReactive_bare(
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
    setReactive_bare(
      id = "x_1", 
      where = where,
      watch = "x_2",
      mutual = TRUE,
#       .tracelevel = 1
    ),
    NULL
  )
  
  expect_equal(
    setReactive_bare(
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

##------------------------------------------------------------------------------

context("setReactive_bare-4")
test_that("setReactive_bare", {

  if (FALSE) {
  
  suppressWarnings(rm(x_1))
  suppressWarnings(rm(x_2))
  suppressWarnings(rm(.hash))
  
  expect_equal(
    setReactive_bare(
      id = "x_1", 
      value = 10
    ),
    10
  )
  expect_equal(
    setReactive_bare(
      id = "x_2", 
      watch = "x_1"
    ),
    10
  )
  ## Values are ignored if variable monitors another //
  expect_warning(
    setReactive_bare(
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
    setReactive_bare(
      id = "x_1", 
      watch = "x_2",
      mutual = TRUE,
#       .tracelevel = 1
    ),
    NULL
  )
  
  expect_equal(
    setReactive_bare(
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
