context("setReactiveS3-A")
test_that("in .GlobalEnv", {
  
  .debug <- FALSE
  
  ## Make sure 'x_1' and 'x_2' are removed:
  resetHashRegistry()
  suppressWarnings(rm(x_1))
  suppressWarnings(rm(x_2))
  setReactiveS3(id = "x_1", value = Sys.time(), .debug = .debug)

  if (.debug) {
    x_1$value
    x_1$uid
    ls(x_1$hash)
    ls(x_1$hash[[x_1$uid]])
    x_1$hash[[x_1$uid]][[x_1$uid]]
    x_1$value
    x_1$hash[[x_1$uid]][[x_1$uid]]
  } else {
    x_1
    x_1 <- Sys.time()
    x_1
    uid <- getReactiveUid(id = "x_1", where = environment())
    expect_true("id" %in% ls(getHashRegistry()[[uid]]))
    expect_true("where" %in% ls(getHashRegistry()[[uid]]))
  }
  
  setReactiveS3(id = "x_2", value = function() {
    ## State dependencies //
    .react_1 <- get(x = "x_1", envir = environment())
    
    ## Do something with the dependencies //
    .react_1 + 60*60*24
  })
#   setReactiveS3(id = "x_2", value = reactive(x_1 + 60*60*24))
  
  if (.debug) {
    x_1$value
    x_2$value
    (x_1 <- Sys.time())
    x_2$value
  } else {
    x_1
    x_2
    (x_1 <- Sys.time())
    x_2
  }

  ##----------------------------------------------------------------------------

  setReactiveS3(id = "x_3", value = function() {
    .react_1 <- get(x = "x_1", envir = environment())
    .react_2 <- get(x = "x_2", envir = environment())
    
    message(paste0("Dependency 1: ", .react_1))
    message(paste0("Dependency 2: ", .react_2))
    message(paste0("Difference: ", .react_2 - .react_1))
    .react_2 + 60*60*24
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
    ## State dependencies //
    .react_1 <- get(x = "x_2", envir = environment())
    
    ## Do something with the dependencies //
    .react_1
  })
  if (.debug) {
    x_1$value
  } else {
    x_1
  }
  setReactiveS3(id = "x_2", value = function() {
    ## State dependencies //
    .react_1 <- get(x = "x_1", envir = environment())
    
    ## Do something with the dependencies //
    .react_1
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
    ## State dependencies //
    .react_1 <- get(x = "x_2", envir = environment())
    
    ## Do something with the dependencies //
    .react_1 * 2
  })
  if (.debug) {
    x_1$value
  } else {
    x_1
  }
  setReactiveS3(id = "x_2", value = function() {
    ## State dependencies //
    .react_1 <- get(x = "x_1", envir = environment())
    
    ## Do something with the dependencies //
    .react_1 / 2
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
    ## State dependencies //
    .react_1 <- get(x = "x_2", envir = environment())
    
    ## Do something with the dependencies //
    .react_1 + 10
  })
  if (.debug) {
    x_1$value
  } else {
    x_1
  }
  setReactiveS3(id = "x_2", value = function() {
    ## State dependencies //
    .react_1 <- get(x = "x_1", envir = environment())
    
    ## Do something with the dependencies //
    .react_1
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
    ## State dependencies //
    .react_1 <- get(x = "x_2", envir = environment())
    
    ## Do something with the dependencies //
    .react_1 + 10
  })
  if (.debug) {
    x_1$value
  } else {
    x_1
  }
  setReactiveS3(id = "x_2", value = function() {
    ## State dependencies //
    .react_1 <- get(x = "x_1", envir = environment())
    
    ## Do something with the dependencies //
    .react_1 + 10
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
    ## State dependencies //
    .react_1 <- get(x = "x_1", envir = where)
    
    ## Do something with the dependencies //
    .react_1 + 60*60*24
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
    ## State dependencies //
    .react_1 <- get(x = "x_1")
    
    ## Do something with the dependencies //
    .react_1 + 60*60*24
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
    ## State dependencies //
    .react_1 <- get(x = "x_1", envir = where_1)
    
    ## Do something with the dependencies //
    .react_1 + 60*60*24
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
    ## State dependencies //
    .react_1 <- get(x = "x_1", envir = where_1)
    
    ## Do something with the dependencies //
    .react_1 + 10
  }, where = where_2)
  expect_equal(where_2$x_2, where_1$x_1 + 10)
  setReactiveS3(id = "x_3", value = function() {
    ## State dependencies //
    .react_1 <- get(x = "x_1", envir = where_1)
    .react_2 <- get(x = "x_2", envir = where_2)
    
    ## Do something with the dependencies //
    sum(.react_1, .react_2) + 100
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
#     deps = list(x_1 = where)) x_1, force = TRUE)
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
    deps = list(x_1 = where_1)) {
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
      deps = list(x_1 = where_1, x_2 = where_2)
    ) {
      ## Code will be automatically inserted here  
    
      ## Do something with the dependencies //
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
  setReactiveS3(id = "x_2", value = function(deps = list(x_1 = environment())) {
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
  setReactiveS3(id = "x_2", value = function(deps = list(x_1 = where_1)) {
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
  setReactiveS3(id = "x_2", value = function(deps = list(x_1 = where_1)) {
    x_1 + 10
  }, where = where_2)
  expect_equal(where_2$x_2, where_1$x_1 + 10)
  setReactiveS3(id = "x_3", value = function(deps = list(x_1 = where_1, 
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
