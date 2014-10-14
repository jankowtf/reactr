##------------------------------------------------------------------------------
context("setReactiveS3/in parent.frame")
##------------------------------------------------------------------------------

this <- environment()

test_that("setReactiveS3/explicit 'where'", {
  
  .debug <- FALSE

  where <- environment()
  
  value <- Sys.time()
  expect_equal(
    setReactiveS3(id = "x_1", value = value, .debug = .debug),
    value
  )
  
  if (.debug) {
    ls(getHashRegistry())
    x_1$value
    x_1$uid
    ls(x_1$hash)
    ls(x_1$hash[[x_1$uid]])
    x_1$hash[[x_1$uid]][[x_1$uid]]
    x_1$value
    x_1$hash[[x_1$uid]][[x_1$uid]]
  } else {
    expect_equal(x_1, value)
    value_2 <- Sys.time()
    expect_equal(x_1 <- value_2, value_2)
    expect_equal(x_1, value_2)
    uid <- getReactiveUid(id = "x_1", where = environment())
    expect_true("id" %in% ls(getHashRegistry()[[uid]]))
    expect_true("where" %in% ls(getHashRegistry()[[uid]]))
  }
  
  expect_equal(
    setReactiveS3(id = "x_2", value = function() {
      .ref_1 <- get(x = "x_1", envir = where)
      .ref_1 + 60*60*24
    }),
    x_1 + 60*60*24
  )
  
  if (.debug) {
    x_1$value
    x_2$value
    (x_1 <- Sys.time())
    x_2$value
  } else {
    expect_equal(x_1, value_2)
    expect_equal(x_2, value_2 + 60*60*24)
    (x_1 <- Sys.time())
    expect_equal(x_2, x_1 + 60*60*24)
  }
  
  ## Clean up //
  suppressWarnings(rm(x_1))
  suppressWarnings(rm(x_2))
  resetHashRegistry()
  
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
  suppressWarnings(rm(x_1))
  suppressWarnings(rm(x_2))
  resetHashRegistry()
  
})
 
test_that("setReactiveS3/set dependent object", {
  
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
  expect_equal(x_2, x_1 * 2)
  (x_2 <- 100)
  expect_equal(x_2, 100)
  expect_true(x_2 != x_1 * 2)
  (x_1 <- 20)
  expect_equal(x_2, x_1 * 2)
  
  ## Clean up //
  suppressWarnings(rm(x_1))
  suppressWarnings(rm(x_2))
  resetHashRegistry()
  
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
    x_1$value
    x_2$value
    x_3$value
    (x_1 <- Sys.time())
    x_3$value ## --> affects 'x_2' and 'x_3' as they both depend on 'x_1'
    x_2$value
    x_1$value
  
    (x_2 <- Sys.time())
    x_3$value
    x_2$value
    x_1$value
  } else {
    expect_equal(x_3, x_1 + x_2 * 2)
    (x_1 <- 100)
    expect_equal(x_3, x_1 + x_2 * 2)
    ## --> affects 'x_2' and 'x_3' as they both depend on 'x_1'
    (x_2 <- 500)
    expect_equal(x_2, 500)
    expect_equal(x_3, x_1 + x_2 * 2) 
    ## --> affects only 'x_3'
    x_1 <- 10
    expect_equal(x_2, x_1 * 2)
    expect_equal(x_3, x_1 + x_2 * 2) 
  }
  
  ## Clean up //
  suppressWarnings(rm(x_1))
  suppressWarnings(rm(x_2))
  suppressWarnings(rm(x_3))
  resetHashRegistry()
  
})

##------------------------------------------------------------------------------
context("setReactiveS3/bidirectional")
##------------------------------------------------------------------------------

test_that("setReactiveS3/bidirectional/identity", {
  
  setReactiveS3(id = "x_1", value = function() {
    .ref_1 <- get(x = "x_2")
    .ref_1
  })
  expect_equal(x_1, NULL)
  setReactiveS3(id = "x_2", value = function() {
    .ref_1 <- get(x = "x_1")
    .ref_1
  })
  expect_equal(x_2, NULL)

  if (FALSE) {
    x_1$hash[[x_1$uid]][[x_1$uid]]
    x_1$hash[[x_1$uid]][[x_2$uid]]
    x_2$hash[[x_2$uid]][[x_2$uid]]
    x_2$hash[[x_2$uid]][[x_1$uid]]
  
    x_1 <- 10
    x_2$value
    x_1$value
    (x_2 <- 20)
    x_1$value
    x_2$value
  }
  
  expect_equal(x_1 <- 10, 10)
  expect_equal(x_2, x_1)
  expect_equal(x_1, x_2)
  expect_equal(x_1, x_2)
  expect_equal(x_2, x_1)
  expect_equal(x_2 <- 20, 20)
  expect_equal(x_1, x_2)
  expect_equal(x_2, x_1)
  
  ## Clean up //
  suppressWarnings(rm(x_1))
  suppressWarnings(rm(x_2))
  resetHashRegistry()
  
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
    x_1$value
    x_2$value
    
    (x_1 <- 10)
    x_1$value
    x_2$value
    x_1$value
    x_2$value
    (x_2 <- 10)
    x_1$value
    x_2$value
    x_1$value
    x_2$value
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
  expect_equal(x_2, 10)
  expect_equal(x_1, x_2 * 2)
  expect_equal(x_2, x_1 / 2)
  expect_equal(x_2, x_1 / 2)
  expect_equal(x_1, x_2 * 2)
  
  ## Clean up //
  suppressWarnings(rm(x_1))
  suppressWarnings(rm(x_2))
  resetHashRegistry()
  
})

test_that("setReactiveS3/bidirectional/function/unsteady", {

  setReactiveS3(id = "x_1", value = function() {
    .ref_1 <- get(x = "x_2")
    .ref_1 
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
  expect_equal(x_1, x_2)
  expect_equal(x_2, x_1 * 2)
  expect_equal(x_2, x_1 * 2)
  
  expect_equal(x_2 <- 100, 100)
  expect_equal(x_2, 100)
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
  suppressWarnings(rm(x_1))
  suppressWarnings(rm(x_2))
  resetHashRegistry()
  
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
  suppressWarnings(rm(x_1))
  suppressWarnings(rm(x_2))
  resetHashRegistry()
  
})
  
##------------------------------------------------------------------------------
context("setReactiveS3/in specific environment")
##------------------------------------------------------------------------------

test_that("setReactiveS3/scenario 1", {
  
  skip("manual only due to environment issues")
  where <- new.env()
  
  value <- 10
  setReactiveS3(id = "x_1", value = value, where = where)
  expect_equal(where$x_1, value)
  setReactiveS3(id = "x_2", value = function() {
    .ref_1 <- get(x = "x_1", envir = where)
    .ref_1 + 10
  })
  expect_equal(x_2, where$x_1 + 10)
  
  ## Clean up //
  suppressWarnings(rm(where))
  suppressWarnings(rm(x_2))
  resetHashRegistry()
  
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
  suppressWarnings(rm(x_1))
  resetHashRegistry()
  
})  

test_that("setReactiveS3/scenario 3", {
  
  where_1 <- new.env()
  where_2 <- new.env()
  this <- environment()
  
  setReactiveS3(id = "x_1", value = 10, where = where_1)
  expect_equal(where_1$x_1, 10)
  setReactiveS3(id = "x_2", value = function() {
    .ref_1 <- get(x = "x_1", envir = this$where_1)
    
    .ref_1 + 10
  }, where = where_2)
  expect_equal(where_2$x_2, where_1$x_1 + 10)
  
  suppressWarnings(rm(where_1))
  suppressWarnings(rm(where_2))
  resetHashRegistry()
  
})

test_that("setReactiveS3/scenario 4", {
  
  where_1 <- new.env()
  where_2 <- new.env()
  where_3 <- new.env()
  this <- environment()
  
  setReactiveS3(id = "x_1", value = 10, where = where_1)
  expect_equal(where_1$x_1, 10)
  setReactiveS3(id = "x_2", value = function() {
    .ref_1 <- get(x = "x_1", envir = this$where_1)
    .ref_1 + 10
  }, where = where_2)
  expect_equal(where_2$x_2, where_1$x_1 + 10)
  setReactiveS3(id = "x_3", value = function() {
    .ref_1 <- get(x = "x_1", envir = this$where_1)
    .ref_2 <- get(x = "x_2", envir = this$where_2)
    sum(.ref_1, .ref_2) + 100
  }, where = where_3)
  expect_equal(where_3$x_3, sum(where_2$x_2, where_1$x_1) + 100)
  
  ## Clean up //
  suppressWarnings(rm(where_1))
  suppressWarnings(rm(where_2))
  suppressWarnings(rm(where_3))
  resetHashRegistry()
  
})

##------------------------------------------------------------------------------
context("setReactiveS3/references via arguments")
##------------------------------------------------------------------------------

test_that("setReactiveS3/via arguments/scenario 1", {
  
  where_1 <- new.env()
  where_2 <- new.env()
  this <- environment()
  resetHashRegistry()
#   ls(getHashRegistry())
  setReactiveS3(id = "x_1", value = 10, where = this$where_1)
  expect_equal(where_1$x_1, 10)
  setReactiveS3(id = "x_2", value = function(
    refs = list(x_1 = list(id = "x_1", where = this$where_1))) {
    x_1 + 10
  }, where = this$where_2)

  expect_equal(where_2$x_2, where_1$x_1 + 10)
  expect_equal(where_1$x_1, 10)
  where_1$x_1 <- 20
  expect_equal(where_2$x_2, where_1$x_1 + 10)
  
  ## Clean up //
  suppressWarnings(rm(where_1))
  suppressWarnings(rm(where_2))
  resetHashRegistry()

}) 
 
test_that("setReactiveS3/via arguments/scenario 2", {
  
  where_1 <- new.env()
  where_2 <- new.env()
  this <- environment()
  
  value <- 10
  setReactiveS3(id = "x_1", value = value, where = where_1)
#   ls(getHashRegistry())
  setReactiveS3(id = "x_2", value = value, where = where_2)
  setReactiveS3(id = "x_3", value = function(
      refs = list(x_1 = list(id = "x_1", where = this$where_1), 
                  x_2 = list(id = "x_2", where = this$where_2)
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
  suppressWarnings(rm(x_3))
  resetHashRegistry()

})  
  
##------------------------------------------------------------------------------
context("setReactiveS3: strictness")
##------------------------------------------------------------------------------

test_that("setReactiveS3: strictness", {
  
  expect_equal(setReactiveS3(id = "x_1", value = 10, strict = 0), 10)
  suppressWarnings(rm(x_1))
  resetHashRegistry()
  expect_warning(expect_equal(setReactiveS3(id = "x_1", value = 10, strict = 1), 10))
  suppressWarnings(rm(x_1))
  resetHashRegistry()
  expect_error(setReactiveS3(id = "x_1", value = 10, strict = 2))
  
  suppressWarnings(rm(x_1))
  resetHashRegistry()
  expect_equal(setReactiveS3(id = "x_1", value = 10), 10)
  expect_warning(setReactiveS3(id = "x_1", value = 10, strict = 1))
  expect_error(setReactiveS3(id = "x_1", value = 10, strict = 2))
  
  suppressWarnings(rm(x_1))
  resetHashRegistry()
  x_1 <- 10
  expect_equal(setReactiveS3(id = "x_1", value = 10), 10)
  suppressWarnings(rm(x_1))
  resetHashRegistry()
  x_1 <- 10
  expect_warning(setReactiveS3(id = "x_1", value = 10, strict = 1))
  suppressWarnings(rm(x_1))
  resetHashRegistry()
  x_1 <- 10
  expect_error(setReactiveS3(id = "x_1", value = 10, strict = 2))
  
  ## Clean up //
  suppressWarnings(rm(x_1))
  resetHashRegistry()
  
})

test_that("setReactiveS3: strictness (get)", {
  
  ## Non-strict //
  suppressWarnings(rm(x_1, envir = parent.frame()))
  suppressWarnings(rm(x_2, envir = parent.frame()))
  resetHashRegistry()
  setReactiveS3(id = "x_1", value = 10)
  setReactiveS3(id = "x_2", 
    value = function() {
      .ref_1 <- get(x = "x_1") 
      .ref_1 * 2
    })
  
  if (FALSE) {
    x_1 <- 20
    x_2
    hash <- getHashRegistry()
    ls(hash)
    uid <- getReactiveUid(id = "x_2", where)
    ls(hash[[uid]][[uid]])
  }
  
  removeReactive("x_1")
  expect_equal(x_2, 20)
  expect_equal(x_2, 20)
  
  ## Strict (get): 0 //
  suppressWarnings(rm(x_1, envir = parent.frame()))
  suppressWarnings(rm(x_2, envir = parent.frame()))
  resetHashRegistry()
  setReactiveS3(id = "x_1", value = 10)
  setReactiveS3(id = "x_2", 
    value = function() {
      .ref_1 <- get(x = "x_1") 
      .ref_1 * 2
    }, strict_get = 0)
  
  removeReactive("x_1")
  expect_equal(x_2, 20)
  expect_equal(x_2, 20)
  
  ## Strict (get): 1 //
  suppressWarnings(rm(x_1, envir = parent.frame()))
  suppressWarnings(rm(x_2, envir = parent.frame()))
  resetHashRegistry()
  setReactiveS3(id = "x_1", value = 10)
  setReactiveS3(id = "x_2", 
    value = function() {
      .ref_1 <- get(x = "x_1") 
      .ref_1 * 2
    }, strict_get = 1)
  
  removeReactive("x_1")
  expect_warning(expect_equal(x_2, NULL))
  expect_warning(expect_equal(x_2, NULL))
  
  ## Strict (get): 2 //
  suppressWarnings(rm(x_1, envir = parent.frame()))
  suppressWarnings(rm(x_2, envir = parent.frame()))
  resetHashRegistry()
  setReactiveS3(id = "x_1", value = 10)
  setReactiveS3(id = "x_2", 
    value = function() {
      .ref_1 <- get(x = "x_1") 
      .ref_1 * 2
    }, strict_get = 2)
  
  removeReactive("x_1")
  expect_error(x_2)
  expect_error(x_2)
  
  ## Clean up //
  suppressWarnings(rm(x_1))
  suppressWarnings(rm(x_2))
  resetHashRegistry()
  
})

test_that("setReactiveS3(): strictness (set)", {
  
  suppressWarnings(rm(x_1))
  suppressWarnings(rm(x_2))
  resetHashRegistry()
  
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
  expect_equal(x_2, 100)
  x_1 <- 20
  expect_equal(x_2, x_1 * 2)
  x_1 <- 10
  
  ## Strict: 1 //
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
  
  ## Strict: 2 //
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
  suppressWarnings(rm(x_1))
  suppressWarnings(rm(x_2))
  resetHashRegistry()
  
})

test_that("setReactiveS3: intentional error on update", {
  
  skip("manual only due to explicit code refactoring")
  ## NOTE
  ## Requires that an explicit error is introduced in the update part!!
  resetHashRegistry()
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
  setReactiveS3(id = "x_1", value = function() {
    .ref_1 <- get(x = "x_1")
    .ref_1 * 2
  })
  expect_equal(x_1, 20)
  x_1 <- 10
  expect_equal(x_1, 10)
  
  ## Clean up //
  suppressWarnings(rm(x_1))
  resetHashRegistry()
  
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
  suppressWarnings(rm(x_1))
  suppressWarnings(rm(x_2))
  suppressWarnings(rm(where))
  resetHashRegistry()
  
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
  suppressWarnings(rm(x_1))
  suppressWarnings(rm(x_2))
  suppressWarnings(rm(where_1))
  resetHashRegistry()
  
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
  suppressWarnings(rm(x_1))
  suppressWarnings(rm(x_2))
  resetHashRegistry()
 
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
  suppressWarnings(rm(x_1))
  suppressWarnings(rm(x_2))
  suppressWarnings(rm(where))
  resetHashRegistry()
  
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
  suppressWarnings(rm(x_1))
  suppressWarnings(rm(x_2))
  suppressWarnings(rm(where))
  resetHashRegistry()
  
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
  suppressWarnings(rm(x_1))
  suppressWarnings(rm(x_2))
  suppressWarnings(rm(where))
  resetHashRegistry()
  
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
  suppressWarnings(rm(x_1))
  suppressWarnings(rm(x_2))
  suppressWarnings(rm(x_3))
  suppressWarnings(rm(x_4))
  resetHashRegistry()
  
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
  this <- environment()
  expect_equal(
    setReactiveS3(id = "x_2", value = function(
      refs = list(x_1 = list(id = "x_1", where = this$where))) {
      x_1
    }),
    x_1
  )
  expect_equal(
    setReactiveS3(id = "x_3", value = function(
      refs = list(x_1 = list(id = "x_1", where = this$where)))
      x_1
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
  
  rm(x_1)
  rm(x_2)
  rm(x_3)
  rm(x_4)
  resetHashRegistry()
  
})

##------------------------------------------------------------------------------
context("setReactiveS3/recognition/yaml*")
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
  
  rm(x_1)
  rm(x_2)
  rm(x_3)
  rm(x_4)
  rm(x_5)
  rm(x_6)
  rm(x_7)
  rm(x_8)
  rm(x_9)
  resetHashRegistry()
  
})


