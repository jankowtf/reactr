## Globals //
this <- environment()
## --> somehow needed when running certain 'test_that' blocks

verbose <- FALSE

test_that("Test bundle", {
#   skip("Manual only")

##------------------------------------------------------------------------------
context("setReactiveS3/in parent.frame")
##------------------------------------------------------------------------------

test_that("setReactiveS3/explicit 'where'", {
  
  .debug <- FALSE

  where <- environment()
  resetRegistry()
  rmReactive("x_1")
  
  value <- Sys.time()
  expect_equal(
    setReactiveS3(id = "x_1", value = value, .debug = .debug),
    value
  )
  
  if (.debug) {
    ls(getRegistry())
    x_1$.value
    x_1$.uid
    ls(x_1$.registry)
    ls(x_1$.registry[[x_1$.uid]])
    x_1$.registry[[x_1$.uid]][[x_1$.uid]]
    x_1$.value
    x_1$.registry[[x_1$.uid]][[x_1$.uid]]
  } else {
    expect_equal(x_1, value)
    uid_1 <- computeObjectUid(id = "x_1", where = where)
    expect_true(".id" %in% ls(getRegistry()[[uid_1]], all.names = TRUE))
    expect_true(".where" %in% ls(getRegistry()[[uid_1]], all.names = TRUE))
    expect_is(regobj_1 <- getRegistry()[[uid_1]], "ReactiveObject.S3")
    expect_equal(regobj_1$.id, "x_1")
    expect_equal(regobj_1$.value, value)
    expect_equal(regobj_1$.checksum, digest::digest(value))
    
    value_2 <- Sys.time()
    expect_equal(x_1 <- value_2, value_2)
    expect_equal(x_1, value_2)
    expect_equal(regobj_1$.value, value_2)
    expect_equal(regobj_1$.checksum, digest::digest(value_2))
  }
  
  expect_equal(
    setReactiveS3(id = "x_2", value = function() {
      .ref_1 <- get(x = "x_1", envir = where)
      .ref_1 + 60*60*24
    }),
    x_1 + 60*60*24
  )
  
  if (.debug) {
    x_1$.value
    x_2$.value
    (x_1 <- Sys.time())
    x_2$.value
  } else {
    expect_equal(x_1, value_2)
    expect_equal(x_2, value_2 + 60*60*24)
    uid_2 <- computeObjectUid(id = "x_2", where = where)
    expect_is(regobj_2 <- getRegistry()[[uid_2]], "ReactiveObject.S3")
    expect_equal(regobj_2$.id, "x_2")
    expect_true(exists(uid_1, regobj_2$.refs_pull))
    expect_equal(regobj_2$.refs_pull[[uid_1]], regobj_1)
    expect_equal(regobj_2$.checksum, digest::digest(x_1 + 60*60*24))
    
    (x_1 <- Sys.time())
    expect_equal(x_2, x_1 + 60*60*24)
    expect_equal(regobj_2$.checksum, digest::digest(x_1 + 60*60*24))
  }
  
  ## Clean up //
  rmReactive("x_1")
  rmReactive("x_2")
#   resetRegistry()
  
})

test_that("setReactiveS3/no explicit 'where'", {
  
  value <- 10
  expect_equal(
    setReactiveS3(id = "x_1", value = value),
    value
  )
  expect_equal(
    setReactiveS3(id = "x_2", value = function() {
      .ref_1 <- get(x = "x_1", inherits = FALSE)
      .ref_1 * 2
    }),
    x_1 * 2
  )
  expect_equal(x_1, value)
  expect_equal(x_2, value * 2)
  (x_1 <- 100)
  expect_equal(x_2, x_1 * 2)
  
  ## Clean up //
  rmReactive("x_1")
  rmReactive("x_2")
  
})
 
test_that("setReactiveS3/set dependent object", {
  
  verbose <- FALSE
  value <- 10
  expect_equal(
    setReactiveS3(id = "x_1", value = value, verbose = verbose),
    value
  )
  expect_equal(
    setReactiveS3(id = "x_2", value = function() {
      .ref_1 <- get(x = "x_1", inherits = FALSE)
      .ref_1 * 2
    }, verbose = verbose),
    x_1 * 2
  )
  expect_equal(x_2, x_1 * 2)
  ## Change value of dependent object //
  (x_2 <- 100)
  expect_equal(x_2, 10 * 2) ## Set value is disregarded
  (x_1 <- 20)
  expect_equal(x_2, 20 * 2) ## update (x_2:x_1:20)
  
  ## Clean up //
  rmReactive("x_1")
  rmReactive("x_2")
  
})

test_that("setReactiveS3/threeway", {

  .debug <- FALSE
  expect_equal(
    setReactiveS3(id = "x_1", value = 10),
    10
  )
  expect_equal(
    setReactiveS3(id = "x_2", value = function() {
      .ref_1 <- get("x_1", inherits = FALSE)
      .ref_1 * 2
    }),
    x_1 * 2
  )
  setReactiveS3(id = "x_3", value = function() {
    .ref_1 <- get(x = "x_1")
    .ref_2 <- get(x = "x_2")
    .ref_1 + .ref_2 * 2
  })
  
  if (.debug) {
    x_1$.value
    x_2$.value
    x_3$.value
    (x_1 <- Sys.time())
    x_3$.value ## --> affects 'x_2' and 'x_3' as they both depend on 'x_1'
    x_2$.value
    x_1$.value
  
    (x_2 <- Sys.time())
    x_3$.value
    x_2$.value
    x_1$.value
  } else {
    expect_equal(x_3, x_1 + x_2 * 2)
    (x_1 <- 100)
    expect_equal(x_3, x_1 + x_2 * 2)
    ## --> affects 'x_2' and 'x_3' as they both depend on 'x_1'
    (x_2 <- 500)
    expect_equal(x_2, 200) ## Set value is disregarded
    expect_equal(x_3, x_1 + x_2 * 2) 
    ## --> affects only 'x_3'
    (x_3 <- 1000)
    expect_equal(x_3, 500) ## Set value is disregarded
    expect_equal(x_3, x_1 + x_2 * 2) 
    ## --> affects only 'x_3'
    x_1 <- 10
    expect_equal(x_2, x_1 * 2)
    expect_equal(x_3, x_1 + x_2 * 2) 
  }
  
  ## Clean up //
  rmReactive("x_1")
  rmReactive("x_2")
  rmReactive("x_3")
#   resetRegistry()
  
})

##------------------------------------------------------------------------------
context("setReactiveS3/bidirectional")
##------------------------------------------------------------------------------

test_that("setReactiveS3/bidirectional/identity", {
  
  verbose <- FALSE
  setReactiveS3(id = "x_1", value = function() {
    .ref_1 <- get(x = "x_2")
    .ref_1
  }, verbose = verbose)
  expect_equal(x_1, NULL)
  setReactiveS3(id = "x_2", value = function() {
    .ref_1 <- get(x = "x_1")
    .ref_1
  }, verbose = verbose)
  expect_equal(x_2, NULL)

  if (FALSE) {
    x_1$.registry[[x_1$.uid]][[x_1$.uid]]
    x_1$.registry[[x_1$.uid]][[x_2$.uid]]
    x_2$.registry[[x_2$.uid]][[x_2$.uid]]
    x_2$.registry[[x_2$.uid]][[x_1$.uid]]
  
    x_1 <- 10
    x_2$.value
    x_1$.value
    (x_2 <- 20)
    x_1$.value
    x_2$.value
  }
  
  expect_equal(x_1 <- 10, 10)
  expect_equal(x_2, 10)
  expect_equal(x_1, 10)
  expect_equal(x_1, x_2)
  expect_equal(x_2, x_1)
  ## Update, `x_1`, `x_2` //
  expect_equal(x_2 <- 1, 1)
  expect_equal(x_1, 1) ## update (x_1:x_2:1)
  expect_equal(x_2, 1) ## update (x_2:x_1:1)
  expect_equal(x_1, 1) ## cache
  expect_equal(x_1, x_2) ## cache
  expect_equal(x_2, x_1) ## cache
  ## Update, `x_2`, `x_1` //
  expect_equal(x_2 <- 2, 2)
  expect_equal(x_2, 2) ## cache
  expect_equal(x_1, 2) ## update (x_1:x_2:2)
  expect_equal(x_2, 2) ## update (x_2:x_1:2)
  expect_equal(x_1, 2) ## cache
  expect_equal(x_2, 2) ## cache 
  ## Double update before explicit request, `x_1`, `x_1`, `x_2` //
  expect_equal(x_2 <- 3, 3)
  expect_equal(x_1 <- 4, 4)
  expect_equal(x_1 , 4) ## update (x_1:x_2:3:x_1:4)
  expect_equal(x_1 , 4) ## update (x_1:x_2:4)
  expect_equal(x_2 , 4) ## cache
  ## Double update before explicit request, `x_1`, `x_2`, `x_1` //
  expect_equal(x_2 <- 1, 1)
  expect_equal(x_1 <- 2, 2)
  expect_equal(x_1 , 2) ## update (x_1:x_2:1:x_1:2)
  expect_equal(x_2 , 2) ## update (x_1:x_2:2)
  expect_equal(x_1 , 2) ## cache
  ## Double update before explicit request, `x_2`, `x_2, `x_1` //
  expect_equal(x_2 <- 3, 3)
  expect_equal(x_1 <- 4, 4)
  expect_equal(x_2 , 3) ## update (x_2:x_1:4:x_2:3)
  expect_equal(x_2 , 3) ## update (x_2:x_1:3)
  expect_equal(x_1 , 3) ## cache
  ## Double update before explicit request, `x_2`, `x_1`, `x_2` //
  expect_equal(x_2 <- 1, 1)
  expect_equal(x_1 <- 2, 2)
  expect_equal(x_2 , 1) ## update (x_2:x_1:2:x_2:1)
  expect_equal(x_1 , 1) ## cache
  expect_equal(x_2 , 1) ## update (x_2:x_1:1)

  ## Clean up //
  rmReactive("x_1")
  rmReactive("x_2")
#   resetRegistry()
  
})

test_that("setReactiveS3/bidirectional/identity/wait", {
  
  setReactiveS3(id = "x_1", value = function() {
    .ref_1 <- get(x = "x_2")
    .ref_1
  })
  expect_equal(x_1, NULL)
  setReactiveS3(id = "x_3", value = function() {
    .ref_1 <- get(x = "x_2")
    .ref_1
  })
  expect_equal(x_3, NULL)
  setReactiveS3(id = "x_2", value = function() {
    .ref_1 <- get(x = "x_1")
    .ref_1
  })
  expect_equal(x_2, NULL)

  if (FALSE) {
    x_1$.registry[[x_1$.uid]][[x_1$.uid]]
    x_1$.registry[[x_1$.uid]][[x_2$.uid]]
    x_2$.registry[[x_2$.uid]][[x_2$.uid]]
    x_2$.registry[[x_2$.uid]][[x_1$.uid]]
  
    x_1 <- 10
    x_2$.value
    x_1$.value
    (x_2 <- 20)
    x_1$.value
    x_2$.value
  }
  
  expect_equal(x_1 <- 10, 10)
  expect_equal(x_2, x_1)
  expect_equal(x_1, x_2)
  expect_equal(x_3, x_2)
  expect_equal(x_3, x_1)
  expect_equal(x_1, x_2)
  expect_equal(x_2, x_1)
  expect_equal(x_2 <- 20, 20)
  expect_equal(x_1, x_2)
  expect_equal(x_2, x_1)
  expect_equal(x_3, x_1)
  expect_equal(x_3, x_2)
  
  ## Clean up //
  rmReactive("x_1")
  rmReactive("x_2")
  rmReactive("x_3")
#   resetRegistry()
  
})

test_that("setReactiveS3/bidirectional/function/steady", {

  setReactiveS3(id = "x_1", value = function() {
    .ref_1 <- get(x = "x_2")
    .ref_1 * 2
  })
  setReactiveS3(id = "x_2", value = function() {
    .ref_1 <- get(x = "x_1")
    .ref_1 / 2
  })
  
  if (FALSE) {
    x_1$.value
    x_2$.value
    
    (x_1 <- 10)
    x_1$.value
    x_2$.value
    x_1$.value
    x_2$.value
    (x_2 <- 10)
    x_1$.value
    x_2$.value
    x_1$.value
    x_2$.value
  }
    
  expect_equal(x_1, numeric())
  expect_equal(x_2, numeric())
  expect_equal(x_1 <- 10, 10)
  expect_equal(x_1, 10)
  expect_equal(x_2, x_1 / 2)
  expect_equal(x_1, x_2 * 2)
  expect_equal(x_2, x_1 / 2)
  expect_equal(x_1, x_2 * 2)
  
  expect_equal(x_2 <- 100, 100)
  expect_equal(x_2, 100)
  expect_equal(x_1, x_2 * 2)
  expect_equal(x_2, x_1 / 2)
  expect_equal(x_2, x_1 / 2)
  expect_equal(x_1, x_2 * 2)
  
  expect_equal(x_2 <- 10, 10)
  expect_equal(x_2, x_1 / 2)
  expect_equal(x_1, x_2 * 2)
  expect_equal(x_2, x_1 / 2)
  expect_equal(x_2, x_1 / 2)
  expect_equal(x_1, x_2 * 2)
  
  ## Clean up //
  rmReactive("x_1")
  rmReactive("x_2")
#   resetRegistry()
  
})

test_that("setReactiveS3/bidirectional/function/unsteady", {

  verbose <- FALSE
  setReactiveS3(id = "x_1", value = function() {
    .ref_1 <- get(x = "x_2")
    .ref_1 
  }, verbose = verbose)
  setReactiveS3(id = "x_2", value = function() {
    .ref_1 <- get(x = "x_1")
    .ref_1 * 2
  }, verbose = verbose)

  expect_equal(x_1, numeric())
  expect_equal(x_2, numeric())
  expect_equal(x_1 <- 10, 10)
  expect_equal(x_1, 10)
  expect_equal(x_2, x_1 * 2)
  expect_equal(x_1, x_2)
  expect_equal(x_2, x_1 * 2)
  expect_equal(x_2, x_1 * 2)
  
  expect_equal(x_2 <- 100, 100)
  expect_equal(x_2, x_1 * 2)
  expect_equal(x_1, x_2)
  expect_equal(x_2, x_1 * 2)
  expect_equal(x_1, x_2)
  
  expect_equal(x_2 <- 500, 500)
  expect_equal(x_2, 1000)
  expect_equal(x_2, x_1 * 2)
  ## --> equal until 'x_1' changes again
  expect_equal(x_2, x_1 * 2)
  expect_equal(x_1 <- 100, 100)
#   expect_equal(x_1, x_2)
  expect_equal(x_2, x_1 * 2)
  
  ## Clean up //
  rmReactive("x_1")
  rmReactive("x_2")
#   resetRegistry()
  
})

test_that("setReactiveS3/bidirectional/function/unsteady", {

  setReactiveS3(id = "x_1", value = function() {
    .ref_1 <- get(x = "x_2")
    .ref_1 * 2
  })
  setReactiveS3(id = "x_2", value = function() {
    .ref_1 <- get(x = "x_1")
    .ref_1 * 2
  })

  expect_equal(x_1, numeric())
  expect_equal(x_2, numeric())
  expect_equal(x_1 <- 10, 10)
  expect_equal(x_1, 10)
  expect_equal(x_2, x_1 * 2)
  expect_equal(x_1, x_2 * 2)
  expect_equal(x_2, x_1 * 2)
  expect_equal(x_1_last <- x_1, x_2 * 2)
  
  expect_equal(x_2 <- 100, 100)
  expect_equal(x_2, 400)
  expect_equal(x_2, x_1 * 2)
  expect_equal(x_1, x_2 * 2)
  
  ## Clean up //
  rmReactive("x_1")
  rmReactive("x_2")
#   resetRegistry()
  
})
  
##------------------------------------------------------------------------------
context("setReactiveS3/late bidirectional")
##------------------------------------------------------------------------------

test_that("setReactiveS3/late bidirectional", {
  
  verbose <- FALSE
  setReactiveS3(id = "x_1", value = 10, verbose = verbose)
  expect_equal(x_1, 10)
  setReactiveS3(id = "x_2", value = function() {
    .ref_1 <- get(x = "x_1")
    .ref_1
  }, verbose = verbose)
  expect_equal(x_2, 10)

  expect_equal(x_1 <- 10, 10)
  expect_equal(x_2, 10)
  expect_equal(x_1, 10)
  expect_equal(x_1, x_2)
  expect_equal(x_2, x_1)
  
  ## Transform to bi-directional //
  setReactiveS3(id = "x_1", value = function() {
    .ref_1 <- get(x = "x_2")
    .ref_1
  }, verbose = verbose)
  
  reg_1 <- getFromRegistry("x_1")
  reg_2 <- getFromRegistry("x_2")
#   ls(reg_1$.refs_pull)
#   ls(reg_2$.refs_pull)
  expect_true(reg_1$.has_bidir)
  expect_true(reg_2$.has_bidir)
  
  ## Update, `x_1`, `x_2` //
  expect_equal(x_1, 10)
  expect_equal(x_2, 10) ## update due to reset
  expect_equal(x_2 <- 1, 1)
  expect_equal(x_1, 1) ## update (x_1:x_2:1)
  expect_equal(x_2, 1) ## update (x_2:x_1:1)
  expect_equal(x_1, 1) ## cache
  expect_equal(x_1, x_2) ## cache
  expect_equal(x_2, x_1) ## cache
  ## Update, `x_2`, `x_1` //
  expect_equal(x_2 <- 2, 2)
  expect_equal(x_2, 2) ## cache
  expect_equal(x_1, 2) ## update (x_1:x_2:2)
  expect_equal(x_2, 2) ## update (x_2:x_1:2)
  expect_equal(x_1, 2) ## cache
  expect_equal(x_2, 2) ## cache 
  ## Double update before explicit request, `x_1`, `x_1`, `x_2` //
  expect_equal(x_2 <- 3, 3)
  expect_equal(x_1 <- 4, 4)
  expect_equal(x_1 , 4) ## update (x_1:x_2:3:x_1:4)
  expect_equal(x_1 , 4) ## update (x_1:x_2:4)
  expect_equal(x_2 , 4) ## cache
  ## Double update before explicit request, `x_1`, `x_2`, `x_1` //
  expect_equal(x_2 <- 1, 1)
  expect_equal(x_1 <- 2, 2)
  expect_equal(x_1 , 2) ## update (x_1:x_2:1:x_1:2)
  expect_equal(x_2 , 2) ## update (x_1:x_2:2)
  expect_equal(x_1 , 2) ## cache
  ## Double update before explicit request, `x_2`, `x_2, `x_1` //
  expect_equal(x_2 <- 3, 3)
  expect_equal(x_1 <- 4, 4)
  expect_equal(x_2 , 3) ## update (x_2:x_1:4:x_2:3)
  expect_equal(x_2 , 3) ## update (x_2:x_1:3)
  expect_equal(x_1 , 3) ## cache
  ## Double update before explicit request, `x_2`, `x_1`, `x_2` //
  expect_equal(x_2 <- 1, 1)
  expect_equal(x_1 <- 2, 2)
  expect_equal(x_2 , 1) ## update (x_2:x_1:2:x_2:1)
  expect_equal(x_1 , 1) ## cache
  expect_equal(x_2 , 1) ## update (x_2:x_1:1)

  ## Clean up //
  rmReactive("x_1")
  rmReactive("x_2")
  
})

##------------------------------------------------------------------------------
context("setReactiveS3/in specific environment")
##------------------------------------------------------------------------------

where_1 <- new.env()
test_that("setReactiveS3/scenario 1", {

  skip("environment issues")
  where_1 <- new.env()
  value <- 10
  setReactiveS3(id = "x_1", value = value, where = where_1)
  expect_equal(where_1$x_1, value)
  setReactiveS3(id = "x_2", value = function() {
    .ref_1 <- get(x = "x_1", envir = where_1)
    .ref_1 + 10
  }, where_1 = where_1)
  expect_equal(x_2, where_1$x_1 + 10)
  
  ## Clean up //
  suppressWarnings(rm(where_1))
  rmReactive("x_2")
  resetRegistry()
  
})

test_that("setReactiveS3/scenario 2", {
  
  where <- new.env()

  setReactiveS3(id = "x_1", value = 10)
  expect_equal(x_1, 10)
  setReactiveS3(id = "x_2", value = function() {
    .ref_1 <- get(x = "x_1")
    .ref_1 + 10
  }, where = where)
  expect_equal(where$x_2, NULL)
  
  ## Clean up //
  suppressWarnings(rm(where))
  rmReactive("x_1")
#   resetRegistry()
  
})  

where_1 <- new.env()
where_2 <- new.env()
test_that("setReactiveS3/scenario 3", {
  
  skip("environment issues")
  where_1 <- new.env()
  where_2 <- new.env()
  setReactiveS3(id = "x_1", value = 10, where = where_1)
  expect_equal(where_1$x_1, 10)
  setReactiveS3(id = "x_2", value = function() {
    print(ls(where_1))
    .ref_1 <- get(x = "x_1", envir = where_1)
    
    .ref_1 + 10
  }, where = where_2, where_1 = where_1)
  expect_equal(where_2$x_2, where_1$x_1 + 10)
  
  suppressWarnings(rm(where_1))
  suppressWarnings(rm(where_2))
  resetRegistry()
  
})

where_1 <- new.env()
where_2 <- new.env()
where_3 <- new.env()
test_that("setReactiveS3/scenario 4", {
  
  skip("environment issues")
  where_1 <- new.env()
  where_2 <- new.env()
  where_3 <- new.env()
  
  setReactiveS3(id = "x_1", value = 10, where = where_1)
  expect_equal(where_1$x_1, 10)
  setReactiveS3(id = "x_2", value = function() {
    .ref_1 <- get(x = "x_1", envir = where_1)
    .ref_1 + 10
  }, where = where_2, where_1 = where_1)
# print("DEBUG")
  expect_equal(where_2$x_2, where_1$x_1 + 10)
  setReactiveS3(id = "x_3", value = function() {
    .ref_1 <- get(x = "x_1", envir = where_1)
    .ref_2 <- get(x = "x_2", envir = where_2)
    sum(.ref_1, .ref_2) + 100
  }, where = where_3, where_1 = where_1, where_2 = where_2)
  expect_equal(where_3$x_3, sum(where_2$x_2, where_1$x_1) + 100)
  
  ## Clean up //
  suppressWarnings(rm(where_1))
  suppressWarnings(rm(where_2))
  suppressWarnings(rm(where_3))
  resetRegistry()
  
})

##------------------------------------------------------------------------------
context("setReactiveS3/references via arguments")
##------------------------------------------------------------------------------

where_1 <- new.env()
where_2 <- new.env()
test_that("setReactiveS3/via arguments/scenario 1", {
  
  setReactiveS3(id = "x_1", value = 10, where = where_1)
  expect_equal(where_1$x_1, 10)
  setReactiveS3(id = "x_2", value = function(
    refs = list(x_1 = list(id = "x_1", where = where_1))) {
    x_1 + 10
  }, where = where_2)

  expect_equal(where_2$x_2, where_1$x_1 + 10)
  expect_equal(where_1$x_1, 10)
  where_1$x_1 <- 20
  expect_equal(where_2$x_2, where_1$x_1 + 10)
  
  ## Clean up //
  suppressWarnings(rm(where_1))
  suppressWarnings(rm(where_2))
  resetRegistry()

}) 
 
test_that("setReactiveS3/via arguments/scenario 2", {
  
  where_1 <- new.env()
  where_2 <- new.env()
  
  value <- 10
  setReactiveS3(id = "x_1", value = value, where = where_1)
#   ls(getRegistry())
  setReactiveS3(id = "x_2", value = value, where = where_2)
  setReactiveS3(id = "x_3", value = function(
      refs = list(x_1 = list(id = "x_1", where = where_1), 
                  x_2 = list(id = "x_2", where = where_2)
      )) {
      out <- x_1 + x_2 + 100
    }
  )
  expect_equal(x_3, (where_1$x_1 + where_2$x_2 + 100))
  
  expect_equal(where_1$x_1, 10)
  expect_equal(where_2$x_2, 10)
  expect_equal(x_3, where_1$x_1 + where_2$x_2 + 100)
  (where_1$x_1 <- 100)
  expect_equal(where_1$x_1, 100)
  expect_equal(where_2$x_2, 10)
  expect_equal(x_3, where_1$x_1 + where_2$x_2 + 100)
  
  ## Clean up //
  suppressWarnings(rm(where_1))
  suppressWarnings(rm(where_2))
  rmReactive("x_3")
  resetRegistry()

})  
  
##------------------------------------------------------------------------------
context("setReactiveS3: strictness (instantiation)")
##------------------------------------------------------------------------------

test_that("setReactiveS3: strictness", {
  
  expect_equal(setReactiveS3(id = "x_1", value = 10, strict = 0), 10)
  rmReactive("x_1")
  resetRegistry()
  expect_warning(expect_equal(setReactiveS3(id = "x_1", value = 10, strict = 1), 10))
  rmReactive("x_1")
  resetRegistry()
  expect_error(setReactiveS3(id = "x_1", value = 10, strict = 2))
  
  rmReactive("x_1")
  resetRegistry()
  expect_equal(setReactiveS3(id = "x_1", value = 10), 10)
  expect_warning(setReactiveS3(id = "x_1", value = 10, strict = 1))
  expect_error(setReactiveS3(id = "x_1", value = 10, strict = 2))
  
  rmReactive("x_1")
  resetRegistry()
  x_1 <- 10
  expect_equal(setReactiveS3(id = "x_1", value = 10), 10)
  rmReactive("x_1")
  resetRegistry()
  x_1 <- 10
  expect_warning(setReactiveS3(id = "x_1", value = 10, strict = 1))
  rmReactive("x_1")
  resetRegistry()
  x_1 <- 10
  expect_error(setReactiveS3(id = "x_1", value = 10, strict = 2))
  
  ## Clean up //
  rmReactive("x_1")
  
})

##------------------------------------------------------------------------------
context("setReactiveS3: strictness (get)")
##------------------------------------------------------------------------------

test_that("setReactiveS3: strictness (get)", {
  
  ## Non-strict //
  suppressWarnings(rm(x_1, envir = parent.frame()))
  suppressWarnings(rm(x_2, envir = parent.frame()))
  resetRegistry()
  setReactiveS3(id = "x_1", value = 10)
  setReactiveS3(id = "x_2", 
    value = function() {
      .ref_1 <- get(x = "x_1") 
      .ref_1 * 2
    }
  )
  
  if (FALSE) {
    x_1 <- 20
    x_2
    registry <- getRegistry()
    ls(registry)
    uid <- computeObjectUid(id = "x_2", where)
    ls(registry[[uid]][[uid]])
  }
  expect_equal(x_2, 20)
  rmReactive("x_1")
  expect_equal(x_2, 20)
  expect_equal(x_2, 20)
  
  ## Strict (get): 0 //
  suppressWarnings(rm(x_1, envir = parent.frame()))
  suppressWarnings(rm(x_2, envir = parent.frame()))
  resetRegistry()
  setReactiveS3(id = "x_1", value = 10)
  setReactiveS3(id = "x_2", 
    value = function() {
      .ref_1 <- get(x = "x_1") 
      .ref_1 * 2
    }, strict_get = 0)
  
  expect_equal(x_2, 20)
  rmReactive("x_1")
  expect_equal(x_2, 20)
  expect_equal(x_2, 20)
  
  ## Strict (get): 1 //
  suppressWarnings(rm(x_1, envir = parent.frame()))
  suppressWarnings(rm(x_2, envir = parent.frame()))
  resetRegistry()
  setReactiveS3(id = "x_1", value = 10)
  setReactiveS3(id = "x_2", 
    value = function() {
      .ref_1 <- get(x = "x_1") 
      .ref_1 * 2
    }, strict_get = 1)
  
  expect_equal(x_2, 20)
  rmReactive("x_1")
  expect_warning(expect_equal(x_2, NULL))
  expect_warning(expect_equal(x_2, NULL))
  
  ## Strict (get): 2 //
  suppressWarnings(rm(x_1, envir = parent.frame()))
  suppressWarnings(rm(x_2, envir = parent.frame()))
  resetRegistry()
  setReactiveS3(id = "x_1", value = 10)
  setReactiveS3(id = "x_2", 
    value = function() {
      .ref_1 <- get(x = "x_1") 
      .ref_1 * 2
    }, strict_get = 2)
  
  expect_equal(x_2, 20)
  rmReactive("x_1")
  expect_error(x_2)
  expect_error(x_2)
  
  ## Clean up //
  rmReactive("x_1")
  rmReactive("x_2")
  
})

##------------------------------------------------------------------------------
context("setReactiveS3: strictness (set)")
##------------------------------------------------------------------------------

test_that("setReactiveS3(): strictness (set)", {
  
  expect_equal(
    setReactiveS3(id = "x_1", value = 10),
    10
  )
  expect_equal(
    setReactiveS3(id = "x_2", value = function() {
      .ref_1 <- get("x_1", inherits = FALSE)
      .ref_1 * 2
    }),
    x_1 * 2
  )
  
  ## Condition handling //
  expect_equal(x_2, x_1 * 2)
  (x_2 <- 100)
  expect_equal(x_2, x_1 * 2)
  x_1 <- 20
  expect_equal(x_2, x_1 * 2)
  x_1 <- 10
  
 
  ## Strict 1: ignore with warning //
  expect_equal(
    setReactiveS3(id = "x_2", value = function() {
      .ref_1 <- get("x_1", inherits = FALSE)
      .ref_1 * 2
    }, strict_set = 1),
    x_1 * 2
  )
  
  ## Condition handling //
  expect_equal(x_2, x_1 * 2)
  expect_warning(x_2 <- 100)
  expect_equal(x_2, 20)
  
  ## Strict 2: error //
  expect_equal(
    setReactiveS3(id = "x_2", value = function() {
      .ref_1 <- get("x_1", inherits = FALSE)
      .ref_1 * 2
    }, strict_set = 2),
    x_1 * 2
  )
  
  ## Condition handling //
  expect_equal(x_2, x_1 * 2)
  expect_error(x_2 <- 100)
  expect_equal(x_2, 20)
  
  ## Clean up //
  rmReactive("x_1")
  rmReactive("x_2")
  
})

test_that("setReactiveS3: intentional error on update", {
  
  skip("manual only due to explicit code refactoring")
  ## NOTE
  ## Requires that an explicit error is introduced in the update part!!
  resetRegistry()
  where <- new.env()
  setReactiveS3(id = "x_1", value = 10)
  expect_error(setReactiveS3(id = "x_2", value = function() {
    ## State references //
    .ref_1 <- get(x = "x_1", envir = where)
    ## Do something with the references //
    .ref_1 * 2
  }))
  x_1 <- 100  
  x_2
  
})

test_that("setReactiveS3: self-reference", {
  
  setReactiveS3(id = "x_1", value = 10)
  expect_error(setReactiveS3(id = "x_1", value = function() {
    .ref_1 <- get(x = "x_1")
    .ref_1 * 2
  }))
  
  ## Clean up //
  rmReactive("x_1")
  
})

##------------------------------------------------------------------------------
context("setReactiveS3/yaml")
##------------------------------------------------------------------------------

test_that("setReactiveS3/yaml/where", {
  
  where <- environment()
  setReactiveS3(id = "x_1", value = 10, where = where)
  setReactiveS3(id = "x_2", 
    value = function() {
      ## object-ref: {id: x_1, where: where}
      
      ## Do something    
      x_1 * 2
    }
  )
  expect_equal(x_1, 10)
  expect_equal(x_2, x_1 * 2)
  x_1 <- 20
  expect_equal(x_1, 20)
  expect_equal(x_2, x_1 * 2)
  
  setReactiveS3(id = "x_2", 
    value = function() {
      ## object-ref: {id: x_1, where: where}
      
      ## Do something with the references //
      x_1 * 2
    }
  )
  
  ## Clean up //
  rmReactive("x_1")
  rmReactive("x_2")
  suppressWarnings(rm(where))
  
  where_1 <- new.env()
  setReactiveS3(id = "x_1", value = 10, where = where_1)
  setReactiveS3(id = "x_2", 
    value = function() {
      ## object-ref: {id: x_1, where: where}
      
      ## Do something with the references //
      x_1 * 2
    }
  )
  
  ## Clean up //
  rmReactive("x_1")
  rmReactive("x_2")
  suppressWarnings(rm(where_1))
  
})

test_that("setReactiveS3/yaml/no where", {

  setReactiveS3(id = "x_1", value = 10)
  setReactiveS3(id = "x_2", 
    value = function() {
      ## object-ref: {id: x_1}
      
      ## Do something with the references //
      x_1 * 2
    }
  )
  expect_equal(x_1, 10)
  expect_equal(x_2, x_1 * 2)
  x_1 <- 20
  expect_equal(x_1, 20)
  expect_equal(x_2, x_1 * 2)
  
  ## Clean up //
  rmReactive("x_1")
  rmReactive("x_2")
 
})

test_that("setReactiveS3/yaml/where/as", {
  
  where <- environment()
  setReactiveS3(id = "x_1", value = 10, where = where)
  setReactiveS3(id = "x_2", 
    value = function(where = parent.frame()) {
      ## object-ref: {id: x_1, where: where, as: ref_x_1}
   
      ## Do something with the references //
      ref_x_1 * 2
    }
  )
  expect_equal(x_1, 10)
  expect_equal(x_2, x_1 * 2)
  x_1 <- 20
  expect_equal(x_1, 20)
  expect_equal(x_2, x_1 * 2)
  
  ## Clean up //
  rmReactive("x_1")
  rmReactive("x_2")
  suppressWarnings(rm(where))
  
})

test_that("setReactiveS3/yaml/where/messed up", {
  
  where <- environment()
  setReactiveS3(id = "x_1", value = 10, where = where)
  setReactiveS3(id = "x_2", 
    value = function() {
      ## object-ref:     {id: x_1, where:  where, as: ref_x_1}
   
      ## Do something with the references //
      ref_x_1 * 2
    }
  )
  expect_equal(x_1, 10)
  expect_equal(x_2, x_1 * 2)
  x_1 <- 20
  expect_equal(x_1, 20)
  expect_equal(x_2, x_1 * 2)
  
  ## Clean up //
  rmReactive("x_1")
  rmReactive("x_2")
  suppressWarnings(rm(where))
  
})  
  
test_that("setReactiveS3/yaml/where/mixed", {
  
  skip("fix #12")
  ## Markup and actual code //
  ## Actual code always takes precedence over markup!
  where <- environment()
  setReactiveS3(id = "x_1", value = 10, where = where)
  setReactiveS3(id = "x_2", 
    value = function() {
      ## object-ref: {id: x_2, where: where, as: REF_1}
      ref_1 <- where$x_1
      
      ## Do something with the references //
      ref_1 * 2
    }
  )
  expect_equal(x_1, 10)
  expect_equal(x_2, x_1 * 2)
  x_1 <- 20
  expect_equal(x_1, 20)
  expect_equal(x_2, x_1 * 2)
  
  ## Clean up //
  rmReactive("x_1")
  rmReactive("x_2")
  suppressWarnings(rm(where))
  
})

#################################################################################
#################################################################################
#################################################################################

## Testing the different ways of specifying/recognizing references //

##------------------------------------------------------------------------------
context("setReactiveS3/recognition/._ref_*")
##------------------------------------------------------------------------------

test_that("setReactiveS3/recognition/.ref_*", {
  
  value <- 10
  expect_equal(
    setReactiveS3(id = "x_1", value = value),
    value
  )
  x_1
  expect_equal(
    setReactiveS3(id = "x_2", value = function() {
      .ref_1 <- get(x = "x_1")
    }),
    x_1
  )
  expect_equal(
    setReactiveS3(id = "x_3", value = function()
      .ref_1 <- get(x = "x_1")
    ),
    x_1
  )
  expect_equal(
    setReactiveS3(id = "x_4", value = function()
      .ref_1 <- get("x_1")
    ),
    x_1
  )
  
  ## Clean up //
  rmReactive("x_1")
  rmReactive("x_2")
  rmReactive("x_3")
  rmReactive("x_4")
  
})

##------------------------------------------------------------------------------
context("setReactiveS3/recognition/args")
##------------------------------------------------------------------------------

test_that("setReactiveS3/recognition/args", {
  
  value <- 10
  expect_equal(
    setReactiveS3(id = "x_1", value = value),
    value
  )
  expect_equal(
    setReactiveS3(id = "x_2", value = function(
      refs = list(x_1 = list(id = "x_1"))) {
      x_1
    }),
    x_1
  )
  expect_equal(
    setReactiveS3(id = "x_3", 
      value = function(refs = list(x_1 = list(id = "x_1"))) x_1
    ),
    x_1
  )
  expect_equal(
    setReactiveS3(id = "x_4", value = function(
      refs = list(x_1 = list(id = "x_1")))
      x_1
    ),
    x_1
  )
  
  rmReactive("x_1")
  rmReactive("x_2")
  rmReactive("x_3")
  rmReactive("x_4")
  
})

##------------------------------------------------------------------------------
context("setReactiveS3/recognition/yaml")
##------------------------------------------------------------------------------

test_that("setReactiveS3/recognition/yaml", {
  
  value <- 10
  expect_equal(
    setReactiveS3(id = "x_1", value = value),
    value
  )
  expect_equal(
    setReactiveS3(id = "x_2", value = function() {
      "object-ref: {id: x_1}"
      x_1
    }),
    x_1
  )
  expect_equal(
    setReactiveS3(id = "x_3", value = function() {
      ## object-ref: {id: x_1}
      x_1
    }),
    x_1
  )
  expect_equal(
    setReactiveS3(id = "x_4", value = function()
      "object-ref: {id: x_1}"
    ),
    x_1
  )
  expect_equal(
    setReactiveS3(id = "x_5", value = function()
      ## object-ref: {id: x_1}
      x_1
    ),
    x_1
  )
  expect_equal(
    setReactiveS3(id = "x_6", value = function() {
      "object-ref: {id: x_1, as: ref_1}"
      ref_1
    }),
    x_1
  )
  expect_equal(
    setReactiveS3(id = "x_7", value = function()
      ## object-ref: {id: x_1, as: ref_1}
      ref_1
    ),
    x_1
  )
  expect_equal(
    setReactiveS3(id = "x_8", value = function() {
      "object-ref: {id: x_1, where: where, as: ref_1}"
      "object-ref: {id: x_2, as: ref_2}"
      ref_1 + ref_2
    }),
    x_1 + x_2 
  )
  expect_equal(
    setReactiveS3(id = "x_9", value = function()
      ## object-ref: {id: x_1, where: where, as: ref_1}
      ## object-ref: {id: x_2, as: ref_2}
      ref_1 + ref_2
    ),
    x_1 + x_2 
  )
  
  rmReactive("x_1")
  rmReactive("x_2")
  rmReactive("x_3")
  rmReactive("x_4")
  rmReactive("x_5")
  rmReactive("x_6")
  rmReactive("x_7")
  rmReactive("x_8")
  rmReactive("x_9")
  
})

##------------------------------------------------------------------------------
context("setReactiveS3/push")
##------------------------------------------------------------------------------

test_that("setReactiveS3/push", {
  
  expect_equal(
    setReactiveS3(id = "x_1", value = 10),
    10
  )
  path_testfile <- file.path(tempdir(), "pushtest.txt")
  suppressWarnings(file.remove(path_testfile))
  
  expect_equal(
    setReactiveS3(id = "x_2", value = function() {
      "object-ref: {id: x_1}"
      msg <- paste0("[", Sys.time(), 
        "] I'm simulating a database update or something like that")
      write(msg, file = file.path(tempdir(), "pushtest.txt"), append = TRUE)
      x_1
    }, strict_get = 2, push = TRUE),
    10
  )
  
  ## Actual push //
  (x_1 <- 100)
  uid_1 <- computeObjectUid("x_1")
  uid_2 <- computeObjectUid("x_2")
  reg_1 <- getFromRegistryByUid(uid_1)
  reg_2 <- getFromRegistryByUid(uid_2)
  expect_false(reg_2$.is_running_push)
  expect_false(reg_2$.has_pushed)
  expect_true(exists(uid_2, reg_1$.refs_push, inherits = FALSE))
  expect_equal(length(readLines(path_testfile)), 2)
  
  (x_1 <- 200)
  expect_equal(length(readLines(path_testfile)), 3)
  
  (x_1 <- 100)
  expect_equal(length(readLines(path_testfile)), 4)
  
  unsetReactive("x_1")
  expect_error(x_2)

  ## Clean up //
  rmReactive("x_1")
  rmReactive("x_2")
  
})

##------------------------------------------------------------------------------
context("setReactiveS3/integrity")
##------------------------------------------------------------------------------

test_that("ensureIntegrity", {

#   resetRegistry()
  setReactiveS3(id = "x_1", value = 10, verbose = verbose)
  setReactiveS3(id = "x_2", 
    value = function() "object-ref: {id: x_1}", strict_get = 1,
    verbose = verbose
  )
  x_2
  setReactiveS3(id = "x_1", value = 20)
  expect_equal(x_2, 20)
  uid_1 <- computeObjectUid("x_1")
  expect_equal(
    getFromRegistry("x_2")$.refs_pull[[uid_1]],
    getFromRegistry("x_1")
  )
  
  if (FALSE) {
    require(microbenchmark)
    microbenchmark(
      "update" = x_2,
      "cache 1" = x_2,
      "cache 2" = x_2
    )
    
    resetRegistry()
    setReactiveS3(id = "x_1", value = 10)
    setReactiveS3(id = "x_2", integrity = FALSE, 
                  value = function() "object-ref: {id: x_1}", strict_get = 1)
    x_2
    x_1 <- 100
    x_2
    setReactiveS3(id = "x_1", value = 20)
    (47 - 24)/10^9
    require(microbenchmark)
    microbenchmark(
      "update" = x_2,
      "cache 1" = x_2,
      "cache 2" = x_2
    )
  }
  ## Clean up //
  rmReactive("x_1")
  rmReactive("x_2")
  
})

##------------------------------------------------------------------------------
context("setReactiveS3/typed")
##------------------------------------------------------------------------------

test_that("ensureIntegrity/typed", {

#   resetRegistry()
  setReactiveS3(id = "x_1", value = 10)
  expect_equal(x_1 <- TRUE, TRUE)
  setReactiveS3(id = "x_1", value = 10, typed = TRUE)
  expect_error(x_1 <- TRUE)
  
  ## Clean up //
  rmReactive("x_1")
  
})

test_that("ensureIntegrity/typed/for initial NULL", {
  
#   resetRegistry()
  setReactiveS3(id = "x_1")
  x_1
  expect_equal(x_1 <- TRUE, TRUE)
  setReactiveS3(id = "x_1", typed = TRUE)
  expect_equal(x_1 <- TRUE, TRUE)
  ## --> overwriting initial `NULL` is perfectly fine
  
  ## Clean up //
  rmReactive("x_1")
  
})

##------------------------------------------------------------------------------
context("setReactiveS3/no cache")
##------------------------------------------------------------------------------

test_that("setReactiveS3/no cache", {

  resetRegistry()
  setReactiveS3(id = "x_1", value = 10, cache = FALSE)
  setReactiveS3(id = "x_2", value = function() "object-ref: {id: x_1}",
                cache = FALSE)
  x_1 <- 20
  expect_equal(x_2, 20)
  expect_equal(showRegistry(), character())
  
  ## Clean up //
  rmReactive("x_1")
  rmReactive("x_2")

})

})
