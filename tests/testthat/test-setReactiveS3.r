context("setReactiveS3-A")

test_that("in parent.frame()", {
  
  .debug <- FALSE
  
  ## Make sure 'x_1' and 'x_2' are removed:
  resetHashRegistry()
  where <- environment()
  suppressWarnings(rm(x_1))
  suppressWarnings(rm(x_2))
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
      ## State references //
      .ref_1 <- get(x = "x_1", envir = where)
      ## Do something with the references //
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
  
  ##----------
  
  ## W/o explicit 'where' //
  suppressWarnings(rm(y_1))
  suppressWarnings(rm(y_2))
  value <- 10
  
  expect_equal(
    setReactiveS3(id = "y_1", value = value),
    value
  )
  expect_equal(
    setReactiveS3(id = "y_2", value = function() {
      ## State references //
      .ref_1 <- get(x = "y_1", inherits = FALSE)
      ## Do something with the references //
      .ref_1 * 2
    }),
    y_1 * 2
  )
  expect_equal(y_1, value)
  expect_equal(y_2, value * 2)
  (y_1 <- 100)
  expect_equal(y_2, y_1 * 2)
  
  ##----------------------------------------------------------------------------

  setReactiveS3(id = "x_3", value = function() {
    .ref_1 <- get(x = "x_1", envir = environment())
    .ref_2 <- get(x = "x_2", envir = environment())
    
    message(paste0("Dependency 1: ", .ref_1))
    message(paste0("Dependency 2: ", .ref_2))
    message(paste0("Difference: ", .ref_2 - .ref_1))
    .ref_2 + 60*60*24
  })

#   setReactiveS3("x_3", value = reactive({
#     message(x_1)
#     message(x_2)
#     out <- x_2 + 60*60*24
#     message(paste0("Difference: ", out - x_1))
#     out
#   }))

  
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
    x_1
    x_2
    x_3
    (x_1 <- Sys.time())
    x_3 ## --> affects 'x_2' and 'x_3' as they both depend on 'x_1'
    x_2
    x_1
  
    (x_2 <- Sys.time())
    x_3 ## --> affects only 'x_3'
    x_2
    x_1
  }

  ## Messing things up //
  x_2 <- 100
  if (.debug) {
    x_1$value ## not affected
    x_3$value ## affected --> try-error
  } else {
    x_1
    x_3
  }
  
  ##----------------------------------------------------------------------------
  ## Mutual //
  ##----------------------------------------------------------------------------

  ## Identity //
  rm(list = ls(getHashRegistry(), all.names = TRUE), 
     envir = getHashRegistry())
  suppressWarnings(rm(x_1))
  suppressWarnings(rm(x_2))
  suppressWarnings(rm(x_3))

  setReactiveS3(id = "x_1", value = function() {
    ## State references //
    .ref_1 <- get(x = "x_2", envir = environment())
    
    ## Do something with the references //
    .ref_1
  })
  if (.debug) {
    x_1$value
  } else {
    x_1
  }
  setReactiveS3(id = "x_2", value = function() {
    ## State references //
    .ref_1 <- get(x = "x_1", envir = environment())
    
    ## Do something with the references //
    .ref_1
  })
  if (.debug) {
    x_2$value
  } else {
    x_2
  }

  if (.debug) {
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
  } else {
    x_1 <- 10
    x_2
    x_1
    x_2
    x_1
    (x_2 <- 20)
    x_1
    x_2
    x_1
    x_2
  }

  ## Transformation 1 //
  rm(list = ls(getHashRegistry(), all.names = TRUE), 
     envir = getHashRegistry())
  suppressWarnings(rm(x_1))
  suppressWarnings(rm(x_2))
  
  setReactiveS3(id = "x_1", value = function() {
    ## State references //
    .ref_1 <- get(x = "x_2", envir = environment())
    
    ## Do something with the references //
    .ref_1 * 2
  })
  if (.debug) {
    x_1$value
  } else {
    x_1
  }
  setReactiveS3(id = "x_2", value = function() {
    ## State references //
    .ref_1 <- get(x = "x_1", envir = environment())
    
    ## Do something with the references //
    .ref_1 / 2
  })
  if (.debug) {
    x_2$value
  } else {
    x_2
  }

  if (.debug) {
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
  } else {
    x_1
    x_2
    
    (x_1 <- 10)
    x_1
    x_2
    x_1
    x_2
    x_1
    (x_2 <- 10)
    x_1
    x_2
    x_1
    x_2
  }

  ## Transformation 2 //
  rm(list = ls(getHashRegistry(), all.names = TRUE), 
     envir = getHashRegistry())
  suppressWarnings(rm(x_1))
  suppressWarnings(rm(x_2))
  
  setReactiveS3(id = "x_1", value = function() {
    ## State references //
    .ref_1 <- get(x = "x_2", envir = environment())
    
    ## Do something with the references //
    .ref_1 + 10
  })
  if (.debug) {
    x_1$value
  } else {
    x_1
  }
  setReactiveS3(id = "x_2", value = function() {
    ## State references //
    .ref_1 <- get(x = "x_1", envir = environment())
    
    ## Do something with the references //
    .ref_1
  })
  if (.debug) {
    x_2$value
  } else {
    x_2
  }

  if (.debug) {
    x_1$value
    x_2$value
  
    (x_2 <- 10)
    x_1$value
    x_2$value
    x_1$value
    x_2$value
    x_1$value
    x_2$value
    (x_2 <- 10)
    x_1$value
    x_2$value
    x_1$value
    x_2$value
    (x_2 <- 10)
    ## --> set to '10'
    ## Even though 'x_2' is bound to 'x_1' by 'x_1 = x_2', the explicit value
    ## takes precedence and is used until 'x_1' is updated again!
    x_2$value
    x_2$value
    x_1$value
    x_2$value
  } else {
    x_1
    x_2
  
    (x_2 <- 10)
    x_1
    x_2
    x_1
    x_2
    x_1
    x_2
    (x_2 <- 10)
    x_1
    x_2
    x_1
    x_2
    (x_2 <- 10)
    ## --> set to '10'
    ## Even though 'x_2' is bound to 'x_1' by 'x_1 = x_2', the explicit value
    ## takes precedence and is used until 'x_1' is updated again!
    x_2
    x_2
    x_1
    x_2
  }

  ## Transformation 3 //
  rm(list = ls(getHashRegistry(), all.names = TRUE), 
     envir = getHashRegistry())
  suppressWarnings(rm(x_1))
  suppressWarnings(rm(x_2))
  
  setReactiveS3(id = "x_1", value = function() {
    ## State references //
    .ref_1 <- get(x = "x_2", envir = environment())
    
    ## Do something with the references //
    .ref_1 + 10
  })
  if (.debug) {
    x_1$value
  } else {
    x_1
  }
  setReactiveS3(id = "x_2", value = function() {
    ## State references //
    .ref_1 <- get(x = "x_1", envir = environment())
    
    ## Do something with the references //
    .ref_1 + 10
  })
  if (.debug) {
    x_2$value
  } else {
    x_2
  }

  if (.debug) {
    x_1$value
    x_2$value
  
    (x_2 <- 10)
    x_1$value
    x_2$value
    x_1$value
    x_2$value
    x_1$value
    x_2$value
    (x_2 <- 10)
    x_1$value
    x_2$value
    x_1$value
    x_2$value
    (x_2 <- 10)
    ## --> set to '10', but 'x_2' is bound to 'x_1' by 'x_1 = x_2'!
    x_2$value
    ## --> that's why this does not result in '10', but in '20' 
    ## (the last cached value of 'x_1' as 'x_1 = x_2 + 10')
    x_1$value
  } else {
    x_1
    x_2
  
    (x_1 <- 10)
    x_1
    x_2
    x_1
    x_2
    x_1
    x_2
    (x_2 <- 10)
    x_2
    x_1
    x_2
    x_1
    x_2
    (x_1 <- 10)
    (x_2 <- 10)
    x_1
    x_2
  }

#   setReactiveS3("x_1", value = 10)
#   setReactiveS3("x_2", value = reactive(x_1 + 10))
#   setReactiveS3("x_3", value = reactive(x_1 + x_2))
#   x_1
#   x_2
#   x_3
#   x_1 <- 100
#   x_2
#   x_3
#   
#   suppressWarnings(rm(x_1))
#   suppressWarnings(rm(x_2))
#   suppressWarnings(rm(x_3))
#   id="x_1"
#   value <- reactive(x_2 + 10)
#   setReactiveS3("x_1", value = reactive(x_2))
#   setReactiveS3("x_2", value = reactive(x_1))
#   x_1 <- 10
#   x_1
})

test_that("different environments", {
  
  skip("manual only due to environment issues")
  where <- new.env()
  value <- Sys.time()
  
  setReactiveS3(id = "x_1", value = value, where = where)
  expect_equal(where$x_1, value)
  setReactiveS3(id = "x_2", value = function() {
    ## State references //
    .ref_1 <- get(x = "x_1", envir = where)
    
    ## Do something with the references //
    .ref_1 + 60*60*24
  })
  expect_equal(x_2, where$x_1 + 60*60*24)
  
  suppressWarnings(rm(where))
  suppressWarnings(rm(x_2))
  resetHashRegistry()
  
  ##----------
  
  where <- new.env()
  value <- Sys.time()
  
  setReactiveS3(id = "x_1", value = value)
  expect_equal(x_1, value)
  setReactiveS3(id = "x_2", value = function() {
    ## State references //
    .ref_1 <- get(x = "x_1")
    
    ## Do something with the references //
    .ref_1 + 60*60*24
  }, where = where)
  expect_equal(where$x_2, x_1 + 60*60*24)
  
  suppressWarnings(rm(where))
  suppressWarnings(rm(x_1))
  resetHashRegistry()
  
  ##----------
  
  where_1 <- new.env()
  where_2 <- new.env()
  value <- Sys.time()
  
  setReactiveS3(id = "x_1", value = value, where = where_1)
  expect_equal(where_1$x_1, value)
  setReactiveS3(id = "x_2", value = function() {
    ## State references //
    .ref_1 <- get(x = "x_1", envir = where_1)
    
    ## Do something with the references //
    .ref_1 + 60*60*24
  }, where = where_2)
  expect_equal(where_2$x_2, where_1$x_1 + 60*60*24)
  
  suppressWarnings(rm(where_1))
  suppressWarnings(rm(where_2))
  resetHashRegistry()
  
  ##----------
  
  where_1 <- new.env()
  where_2 <- new.env()
  where_3 <- new.env()
  value <- 10
  
  setReactiveS3(id = "x_1", value = value, where = where_1)
  expect_equal(where_1$x_1, value)
  setReactiveS3(id = "x_2", value = function() {
    ## State references //
    .ref_1 <- get(x = "x_1", envir = where_1)
    
    ## Do something with the references //
    .ref_1 + 10
  }, where = where_2)
  expect_equal(where_2$x_2, where_1$x_1 + 10)
  setReactiveS3(id = "x_3", value = function() {
    ## State references //
    .ref_1 <- get(x = "x_1", envir = where_1)
    .ref_2 <- get(x = "x_2", envir = where_2)
    
    ## Do something with the references //
    sum(.ref_1, .ref_2) + 100
  }, where = where_3)
  expect_equal(where_3$x_3, sum(where_2$x_2, where_1$x_1) + 100)
  
  suppressWarnings(rm(where_1))
  suppressWarnings(rm(where_2))
  suppressWarnings(rm(where_3))
  resetHashRegistry()
  
})

test_that("alternative specification of environments", {
  skip("only manually due to environment issues")
## TODO: better removal  
#   value <- Sys.time()
#   where <- environment()
#   setReactiveS3(id = "x_1", value = value, force = TRUE)
#   expect_equal(x_1, value)
#   setReactiveS3(id = "x_2", value = function(
#     refs = list(x_1 = where)) x_1, force = TRUE)
#   expect_equal(x_2, x_1)
#   expect_equal(x_1, value)
#   x_1 <- Sys.time()
#   expect_equal(x_2, x_1)
#   
#   suppressWarnings(rm(x_1))
#   suppressWarnings(rm(x_2))
#   resetHashRegistry()
  
  ##----------
  
  where_1 <- new.env()
  where_2 <- new.env()
  value <- Sys.time()
  setReactiveS3(id = "x_1", value = value, where = where_1)
  expect_equal(where_1$x_1, value)
  setReactiveS3(id = "x_2", value = function(
    refs = list(x_1 = where_1)) {
    x_1 + 60*60*24
  }, where = where_2)
  expect_equal(where_2$x_2, where_1$x_1 + 60*60*24)
  expect_equal(where_1$x_1, value)
  where_1$x_1 <- Sys.time()
  expect_equal(where_2$x_2, where_1$x_1 + 60*60*24)
  
  suppressWarnings(rm(where_1))
  suppressWarnings(rm(where_2))
  resetHashRegistry()
  
  ##----------
  
  where_1 <- new.env()
  where_2 <- new.env()
  value <- 10
  setReactiveS3(id = "x_1", value = value, where = where_1)
  ls(getHashRegistry())
  setReactiveS3(id = "x_2", value = value, where = where_2)
  setReactiveS3(id = "x_3", value = function(
      refs = list(x_1 = where_1, x_2 = where_2)
    ) {
      ## Code will be automatically inserted here  
    
      ## Do something with the references //
      out <- x_1 + x_2 + 100
    },
    .debug = FALSE
  )
  expect_equal(x_3, (where_1$x_1 + where_2$x_2 + 100))
  
  expect_equal(where_1$x_1, 10)
  expect_equal(where_2$x_2, 10)
  expect_equal(x_3, where_1$x_1 + where_2$x_2 + 100)
  (where_1$x_1 <- 100)
  expect_equal(where_1$x_1, 100)
  expect_equal(where_2$x_2, 10)
  expect_equal(x_3, where_1$x_1 + where_2$x_2 + 100)
  
  suppressWarnings(rm(where_1))
  suppressWarnings(rm(where_2))
  suppressWarnings(rm(x_3))
  resetHashRegistry()
  
  ##----------
  
  where <- new.env()
  value <- Sys.time()
  
  setReactiveS3(id = "x_1", value = value)
  expect_equal(x_1, value)
  setReactiveS3(id = "x_2", value = function(refs = list(x_1 = environment())) {
    x_1 + 60*60*24
  }, where = where)
  expect_equal(where$x_2, x_1 + 60*60*24)
  expect_equal(where$x_2, value + 60*60*24)
  x_1 <- Sys.time()
  expect_equal(where$x_2, x_1 + 60*60*24)
  
  suppressWarnings(rm(where))
  suppressWarnings(rm(x_1))
  resetHashRegistry()
  
  ##----------
  
  where_1 <- new.env()
  where_2 <- new.env()
  value <- Sys.time()
  
  setReactiveS3(id = "x_1", value = value, where = where_1)
  expect_equal(where_1$x_1, value)
  setReactiveS3(id = "x_2", value = function(refs = list(x_1 = where_1)) {
    x_1 + 60*60*24
  }, where = where_2)
  expect_equal(where_2$x_2, where_1$x_1 + 60*60*24)
  where_1$x_1 <- Sys.time()
  expect_equal(where_2$x_2, where_1$x_1 + 60*60*24)
  
  suppressWarnings(rm(where_1))
  suppressWarnings(rm(where_2))
  resetHashRegistry()
  
  ##----------
  
  where_1 <- new.env()
  where_2 <- new.env()
  where_3 <- new.env()
  value <- 10
  
  setReactiveS3(id = "x_1", value = value, where = where_1)
  expect_equal(where_1$x_1, value)
  setReactiveS3(id = "x_2", value = function(refs = list(x_1 = where_1)) {
    x_1 + 10
  }, where = where_2)
  expect_equal(where_2$x_2, where_1$x_1 + 10)
  setReactiveS3(id = "x_3", value = function(refs = list(x_1 = where_1, 
                                                         x_2 = where_2)
   ) {
    sum(x_1, x_2) + 100
  }, where = where_3)
  expect_equal(where_3$x_3, sum(where_2$x_2, where_1$x_1) + 100)
  where_1$x_1 <- 100
  expect_equal(where_3$x_3, sum(where_2$x_2, where_1$x_1) + 100)
  where_2$x_2 <- 100
  expect_equal(where_3$x_3, sum(where_2$x_2, where_1$x_1) + 100)
  
  suppressWarnings(rm(where_1))
  suppressWarnings(rm(where_2))
  suppressWarnings(rm(where_3))
  resetHashRegistry()
  
})

test_that("setReactiveS3: strictness", {
  
  ## Non-strict //
  resetHashRegistry()
  where <- environment()
  setReactiveS3(id = "x_1", value = 10)
  setReactiveS3(id = "x_2", 
    value = function() {
      .ref_1 <- get(x = "x_1", envir = where) 
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
  
  ## Strict 0 //
  resetHashRegistry()
  where <- environment()
  setReactiveS3(id = "x_1", value = 10)
  setReactiveS3(id = "x_2", 
    value = function() {
      .ref_1 <- get(x = "x_1", envir = where) 
      .ref_1 * 2
    }, strict = 0)
  
  removeReactive("x_1")
  expect_equal(x_2, 20)
  expect_equal(x_2, 20)
  
  ## Strict 1 //
  resetHashRegistry()
  where <- environment()
  setReactiveS3(id = "x_1", value = 10)
  setReactiveS3(id = "x_2", 
    value = function() {
      .ref_1 <- get(x = "x_1", envir = where) 
      .ref_1 * 2
    }, strict = 1)
  
  removeReactive("x_1")
  expect_equal(x_2, NULL)
  expect_equal(x_2, NULL)
  
  ## Strict 2 //
  resetHashRegistry()
  where <- environment()
  setReactiveS3(id = "x_1", value = 10)
  setReactiveS3(id = "x_2", 
    value = function() {
      .ref_1 <- get(x = "x_1", envir = where) 
      .ref_1 * 2
    }, strict = 2)
  
  removeReactive("x_1")
  expect_error(x_2)
  expect_error(x_2)
  
})

test_that("setReactiveS3: force", {
  
  ## Strict 2 //
  resetHashRegistry()
  where <- environment()
  rm(x_1)
  setReactiveS3(id = "x_1", value = 10, strict = 2)
  x_1 <- 10
  setReactiveS3(id = "x_1", value = 10, strict = 2)
  setReactiveS3(id = "x_1", value = 10)
  setReactiveS3(id = "x_1", value = 20, strict = 2)
  setReactiveS3(id = "x_1", value = 20)
  
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
  
  suppressWarnings(rm(x_1))
  suppressWarnings(rm(x_2))
  resetHashRegistry()
  where <- new.env()
  setReactiveS3(id = "x_1", value = 10)
  setReactiveS3(id = "x_1", value = function() {
    ## State references //
    .ref_1 <- get(x = "x_1", envir = where)
    ## Do something with the references //
    .ref_1 * 2
  })
  expect_equal(x_1, 20)
  x_1 <- 10
  expect_equal(x_1, 10)
  
})

test_that("setReactiveS3: reference markup", {
  
  ## With explicit 'where' //
  suppressWarnings(rm(x_1))
  suppressWarnings(rm(x_2))
  resetHashRegistry()
  where <- environment()
  setReactiveS3(id = "x_1", value = 10, where = where)
  setReactiveS3(id = "x_2", 
    value = function(where = parent.frame()) {
      ## [@reactive-ref: x_1 in where]
      
      ## Do something with the references //
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
      ## [@reactive-ref: x_1 in where]
      
      ## Do something with the references //
      x_1 * 2
    }
  )
  suppressWarnings(rm(x_1))
  suppressWarnings(rm(x_2))
  
  where_1 <- new.env()
  setReactiveS3(id = "x_1", value = 10, where = where_1)
  setReactiveS3(id = "x_2", 
    value = function() {
      ## [@reactive-ref: x_1 in where_1]
      
      ## Do something with the references //
      x_1 * 2
    }
  )
  
  ##----------
  
  ## W/o explicit 'where' //
  suppressWarnings(rm(x_1))
  suppressWarnings(rm(x_2))
  suppressWarnings(rm(where))
  resetHashRegistry()

  setReactiveS3(id = "x_1", value = 10)
  setReactiveS3(id = "x_2", 
    value = function() {
      ## [@reactive-ref: x_1]
      
      ## Do something with the references //
      x_1 * 2
    }
  )
  expect_equal(x_1, 10)
  expect_equal(x_2, x_1 * 2)
  x_1 <- 20
  expect_equal(x_1, 20)
  expect_equal(x_2, x_1 * 2)
  
  ##----------
  
  ## With 'as' //
  suppressWarnings(rm(x_1))
  suppressWarnings(rm(x_2))
  resetHashRegistry()
  where <- environment()
  setReactiveS3(id = "x_1", value = 10, where = where)
  setReactiveS3(id = "x_2", 
    value = function(where = parent.frame()) {
      ## [@reactive-ref: x_1 in where as ref_x_1]
   
      ## Do something with the references //
      ref_x_1 * 2
    }
  )
  expect_equal(x_1, 10)
  expect_equal(x_2, x_1 * 2)
  x_1 <- 20
  expect_equal(x_1, 20)
  expect_equal(x_2, x_1 * 2)
  
  ##----------
  
  ## Messed up format //
  suppressWarnings(rm(x_1))
  suppressWarnings(rm(x_2))
  resetHashRegistry()
  where <- environment()
  setReactiveS3(id = "x_1", value = 10, where = where)
  setReactiveS3(id = "x_2", 
    value = function() {
      ## [@reactive-ref:     x_1in    where asref_x_1]
   
      ## Do something with the references //
      ref_x_1 * 2
    }
  )
  expect_equal(x_1, 10)
  expect_equal(x_2, x_1 * 2)
  x_1 <- 20
  expect_equal(x_1, 20)
  expect_equal(x_2, x_1 * 2)
  
  ##----------
  
  ## Markup and actual code //
  ## Actual code always takes precedence over markup!
  suppressWarnings(rm(x_1))
  suppressWarnings(rm(x_2))
  resetHashRegistry()
  where <- environment()
  setReactiveS3(id = "x_1", value = 10, where = where)
  setReactiveS3(id = "x_2", 
    value = function() {
      ## [@reactive-ref: x_2 in where as REF_1]
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
  
})

#################################################################################
#################################################################################
#################################################################################

## Testing the different ways of specifying/recognizing references //

test_that("setReactiveS3: recognition based on '.ref_*'", {
  
  value <- 10
  expect_equal(
    setReactiveS3(id = "x_1", value = value),
    value
  )
  x_1
  expect_equal(
    setReactiveS3(id = "x_2", value = function() {
      .ref_1 <- get(x = "x_1", inherits = FALSE)
    }),
    x_1
  )
  expect_equal(
    setReactiveS3(id = "x_3", value = function()
      .ref_1 <- get(x = "x_1", inherits = FALSE)
    ),
    x_1
  )
  expect_equal(
    setReactiveS3(id = "x_4", value = function()
      .ref_1 <- get("x_1", inherits = FALSE)
    ),
    x_1
  )
  
  rm(x_1)
  rm(x_2)
  rm(x_3)
  rm(x_4)
  resetHashRegistry()
  
})

test_that("setReactiveS3: recognition based on 'deps'", {
  
  value <- 10
  expect_equal(
    setReactiveS3(id = "x_1", value = value),
    value
  )
  where <- environment()
  expect_equal(
    setReactiveS3(id = "x_2", value = function(refs = list(x_1 = where)) {
      x_1
    }),
    x_1
  )
  expect_equal(
    setReactiveS3(id = "x_3", value = function(refs = list(x_1 = where))
      x_1
    ),
    x_1
  )
  expect_equal(
    setReactiveS3(id = "x_4", value = function(refs = list(x_1 = NULL))
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

test_that("setReactiveS3: recognition based on '[@' markup", {
  
  value <- 10
  expect_equal(
    setReactiveS3(id = "x_1", value = value),
    value
  )
  expect_equal(
    setReactiveS3(id = "x_2", value = function() {
      ## [@reactive-ref: x_1]
      x_1
    }),
    x_1
  )
  expect_equal(
    setReactiveS3(id = "x_3", value = function()
      ## [@reactive-ref: x_1]
      x_1
    ),
    x_1
  )
  expect_equal(
    setReactiveS3(id = "x_4", value = function()
      ## [@reactive-ref: x_1 as ref_1]
      ref_1
    ),
    x_1
  )
  expect_equal(
    setReactiveS3(id = "x_5", value = function()
      ## [@reactive-ref: x_1 in where as ref_1]
      ## [@reactive-ref: x_2 as ref_2]
      ref_1 + ref_2
    ),
    x_1 + x_2 
  )
  expect_error(
    setReactiveS3(id = "x_6", value = function()
      ## [@reactive-ref: x_1 as ref_1 in where]
      ## [@reactive-ref: x_2 as ref_2]
      ref_1 + ref_2
    )
  )
  
  rm(x_1)
  rm(x_2)
  rm(x_3)
  rm(x_4)
  resetHashRegistry()
  
})
