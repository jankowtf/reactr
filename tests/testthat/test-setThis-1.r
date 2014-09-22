context("setThis-1")
test_that("setThis", {
  
  .hash_id <- "._HASH"
  where = new.env()  
  
  id <- "x_1"
  value <- Sys.time()
  expect_equal(
    setThis(id = "x_1", value = value, where = where, binding_type = 2),
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
    setThis(id = id, where = where, binding = binding, watch = watch,
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
  expect_equal(getThis(id = id, where = where), expected)
  
  ## Change watch value //
  value = Sys.time()
  expect_equal(
    setThis(id = "x_1", value = value, where = where, binding_type = 2),
    value
  )
  expect_true(where[[.hash_id]]$x_1$x_1 != where[[.hash_id]]$x_1$x_2)
  
  id <- "x_2"
  expect_equal(
    getThis(id = id, where = where, binding_type = 2), 
    eval(binding)(x = value)
  )

  ##----------------------------------------------------------------------------
  ## Setting via 'makeActiveBinding' //
  ##----------------------------------------------------------------------------
  
  ## Variable that can be monitored //
  where <- new.env()
  value <- 10
  
  expect_equal(
    setThis(
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
    setThis(
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
    setThis(
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
    setThis(
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
    setThis(
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
      "1" = setThis(id = "x_1", value = Sys.time(), where = where,
                     binding_type = 2),
      "2" = getThis(id = "x_1", where = where),
      "3" = setThis(id = "x_2", where = where,
        binding = substitute(function(x) {
            x + 60*60*24
          }), watch = "x_1", binding_type = 2),
      "4" = getThis(id = "x_2", where = where),
      "5" = setThis(id = "x_1", value = Sys.time(), where = where,
                     binding_type = 2),
      "6" = getThis(id = "x_2", where = where),
      control = list(order = "inorder")
    )
    res_1
    
    ##-----------
    
    where <- new.env()
    
    res_2 <- microbenchmark(
      "1" = setThis(id = "x_1", value = 10, where = where),
      "2" = getThis(id = "x_1", where = where),
      "3" = setThis(id = "x_2", where = where, watch = "x_1",
        binding = function(x) {x + 100}),
      "4" = getThis(id = "x_2", where = where),
      "5" = setThis(id = "x_1", value = 100, where = where),
      "6" = getThis(id = "x_2", where = where),
      control = list(order = "inorder")
    )
    res_2

     #-----------------

    foo <- function(x) {
      x + 10
    }
    setGeneric(name = "bar", signature=c("x"), 
               def = function(x) standardGeneric)
    setMethod(f = "bar", signature = c(x = "ANY"), definition = function(x) {
      x + 10
    })
    
    require("compiler")
    setGeneric(name = "foobar", signature=c("x"), 
               def = cmpfun(function(x) standardGeneric))
    setMethod(f = "foobar", signature = c(x = "ANY"), 
              definition = cmpfun(function(x) {
      x + 10
    }))
    res_3 <- microbenchmark(
      "foo" = foo(x = 10),
      "bar" = bar(x = 10),
      "foobar" = foobar(x = 10)
    )
    res_3
  }
  
})
