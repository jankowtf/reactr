context("setValue-1")
test_that("setValue", {
  
  envir = new.env()  
  
  id <- "x_1"
  value <- Sys.time()
  expect_equal(
    setValue(id = "x_1", value = value, envir = envir, binding_type = 2),
    value
  )
  
  expect_equal(
    ls(envir, all.names = TRUE),
    c(".bindings", ".hash", ".watch", "x_1")
  )
  expect_equal(ls(envir$.bindings), character())
  expect_equal(ls(envir$.hash), id)
  expect_equal(ls(envir$.hash[[id]]), id)
  
  id <- "x_2"
  binding <- substitute(function(x) {
    x + 60*60*24
  })
  watch <- "x_1"
  expected <- eval(binding)(x = value)
  expect_equal(
    setValue(id = id, envir = envir, binding = binding, watch = watch,
             binding_type = 2),
    expected
  )
  
  expect_equal(
    ls(envir, all.names = TRUE),
    c(".bindings", ".hash", ".watch", "x_1", "x_2")
  )
  expect_equal(ls(envir$.bindings), id)
  expect_equal(ls(envir$.hash), c("x_1", id))
  expect_equal(ls(envir$.hash[[id]]), id)
  expect_equal(ls(envir$.hash[["x_1"]]), c("x_1", id))
  
  id <- "x_2"
  expect_equal(getValue(id = id, envir = envir), expected)
  
  ## Change watch value //
  value = Sys.time()
  expect_equal(
    setValue(id = "x_1", value = value, envir = envir, binding_type = 2),
    value
  )
  expect_true(envir$.hash$x_1$x_1 != envir$.hash$x_1$x_2)
  
  ## TODO
  if (FALSE) {
    id <- "x_2"
    expect_equal(
      getValue(id = id, envir = envir, binding_type = 2), 
      eval(binding)(x = value)
    )
  }
 
  ##----------------------------------------------------------------------------
  ## Setting via 'makeActiveBinding' //
  ##----------------------------------------------------------------------------
  
  ## Variable that can be monitored //
  envir <- new.env()
  binding <- substitute(
    local({
      VALUE <- NULL
      function(v) {
        if (!exists(id, envir$.hash)) {
          assign(id, new.env(), envir = envir$.hash)
        }
        if (!missing(v)) {
#           envir$.hash[[id]][[id]] <- digest::digest(v)
          VALUE <<- v
        }
        ## Ensure hash value //
        assign(id, digest::digest(VALUE), envir$.hash[[id]])
        VALUE
      }
    }),
    list(VALUE = as.name("value"))
  )

  value <- 10

#   setValue(
#       id = "x_1", 
#       value = value, 
#       envir = envir, 
#       binding = binding,
#       binding_type = 1
#     )

  expect_equal(
    setValue(
      id = "x_1", 
      value = value, 
      envir = envir, 
      binding = binding,
      binding_type = 1
    ),
    value
  )
  expect_equal(envir$x_1, value)
  expect_equal(envir$.hash$x_1$x_1, digest::digest(value))

  ## Simple assignment //
  value <- 100
  expect_equal(envir$x_1 <- value, value)
  expect_equal(envir$x_1, value)
  expect_equal(envir$.hash$x_1$x_1, digest::digest(value))

  ## Set again //
  value <- 10
  expect_equal(
    setValue(
      id = "x_1", 
      value = value, 
      envir = envir, 
      binding = binding,
      binding_type = 1
    ),
    value
  )
  expect_equal(envir$.hash$x_1$x_1, digest::digest(value))

  ## Set variable that monitors 'x_1' //
  binding <- substitute(
    local({
      ## Initial value //
      if (  exists(watch, envir = envir, FALSE) &
            !is.null(get(watch, envir = envir, FALSE))
      ) {
        VALUE <- get(watch, envir, inherits = FALSE) + 100
      } else {
        VALUE <- NULL
      }
      
      ## Ensure hash value transfer //
      if (  exists(watch, envir = envir$.hash[[watch]], inherits = FALSE) &&
            !exists(id, envir = envir$.hash[[watch]], inherits = FALSE
      )) {
        assign(
          id, 
          get(watch, envir = envir$.hash[[watch]]),
          envir$.hash[[watch]]
        )
      }

      function(v) {
        if (  exists(watch, envir = envir, FALSE) &
              !is.null(get(watch, envir = envir, FALSE))
        ) {
          
          ## Ensure hash value transfer //
# #           if (!exists(id, envir = envir$.hash[[watch]], inherits = FALSE)) {
#             assign(
#               id, 
#               get(watch, envir = envir$.hash[[watch]]),
#               envir$.hash[[watch]]
#             )
#           }
          
          if (missing(v)) {
            hash_0 <- envir$.hash[[watch]][[watch]]
            hash_1 <- envir$.hash[[watch]][[id]]
            message(hash_0)
            message(hash_1)
            if (hash_0 != hash_1) {
#               message("monitored variable has changed:")
#               message("updating")
              VALUE <<- envir[[watch]] + 100
              envir$.hash[[watch]][[id]] <- hash_0
            }
          }
        }
        VALUE
      }
    }),
    list(VALUE = as.name("value"))
  )
  
  expected <- value + 100
  expect_equal(
    setValue(
      id = "x_2", 
      envir = envir, 
      watch = "x_1", 
      binding = binding, 
      binding_type = 1
    ),
    expected
  )

  expect_equal(envir$x_1, value)
  expect_equal(envir$x_2, expected)
  expect_equal(envir$x_2, expected)

#   sapply(ls(envir$.hash$x_1), get, envir = envir$.hash$x_1)
  expect_equal(envir$.hash$x_1$x_1, digest::digest(value))
  expect_equal(envir$.hash$x_1$x_2, envir$.hash$x_1$x_1)
  
  ## Simple assignment //
  value <- 100
  expected <- value + 100
  expect_equal(envir$x_1 <- value, value)
  expect_equal(envir$x_1, value)
  expect_equal(envir$x_2, expected)
#   sapply(ls(envir$.hash$x_1), get, envir = envir$.hash$x_1)
  expect_equal(envir$.hash$x_1$x_1, digest::digest(value))
  expect_equal(envir$.hash$x_1$x_2, envir$.hash$x_1$x_1)
  expect_equal(envir$x_2, expected)
  
  ## Set again //
  value <- 10
  expected <- value
  expect_equal(
    setValue(
      id = "x_1", 
      value = value, 
      envir = envir, 
      binding = binding,
      binding_type = 1
    ),
    expected
  )
  expected <- value + 100
  expect_equal(envir$x_2, expected)
  expect_equal(envir$.hash$x_1$x_1, digest::digest(value))
  expect_equal(envir$.hash$x_1$x_2, envir$.hash$x_1$x_2)
  
  ##----------------------------------------------------------------------------
  ## Profiling //
  ##----------------------------------------------------------------------------

  if (TRUE) {
    require(microbenchmark)
    
    envir <- new.env()  
    
#     watch <- "x_1"
  
    res_1 <- microbenchmark(
      "1" = setValue(id = "x_1", value = Sys.time(), envir = envir,
                     binding_type = 2),
      "2" = getValue(id = "x_1", envir = envir),
      "3" = setValue(id = "x_2", envir = envir,
        binding = substitute(function(x) {
            x + 60*60*24
          }), watch = "x_1", binding_type = 2),
      "4" = getValue(id = "x_2", envir = envir),
      "5" = setValue(id = "x_1", value = Sys.time(), envir = envir,
                     binding_type = 2),
      "6" = getValue(id = "x_2", envir = envir),
      control = list(order = "inorder")
    )
    res_1
    
    ##-----------
    
    envir <- new.env()
    b1 <- substitute(
      local({
        VALUE <- NULL
        function(v) {
          if (!exists(id, envir$.hash)) {
            assign(id, new.env(), envir = envir$.hash)
          }
          if (!missing(v)) {
  #           envir$.hash[[id]][[id]] <- digest::digest(v)
            VALUE <<- v
          }
          ## Ensure hash value //
          assign(id, digest::digest(VALUE), envir$.hash[[id]])
          VALUE
        }
      }),
      list(VALUE = as.name("value"))
    )
    b2 <- substitute(
      local({
        ## Initial value //
        if (  exists(watch, envir = envir, FALSE) &
              !is.null(get(watch, envir = envir, FALSE))
        ) {
          VALUE <- get(watch, envir, inherits = FALSE) + 100
        } else {
          VALUE <- NULL
        }
        
        ## Ensure hash value transfer //
        if (  exists(watch, envir = envir$.hash[[watch]], inherits = FALSE) &&
              !exists(id, envir = envir$.hash[[watch]], inherits = FALSE
        )) {
          assign(
            id, 
            get(watch, envir = envir$.hash[[watch]]),
            envir$.hash[[watch]]
          )
        }
  
        function(v) {
          if (  exists(watch, envir = envir, FALSE) &
                !is.null(get(watch, envir = envir, FALSE))
          ) {
            if (missing(v)) {
              hash_0 <- envir$.hash[[watch]][[watch]]
              hash_1 <- envir$.hash[[watch]][[id]]
#               message(hash_0)
#               message(hash_1)
              if (hash_0 != hash_1) {
  #               message("monitored variable has changed:")
  #               message("updating")
                VALUE <<- envir[[watch]] + 100
                envir$.hash[[watch]][[id]] <- hash_0
              }
            }
          }
          VALUE
        }
      }),
      list(VALUE = as.name("value"))
    )
    res_2 <- microbenchmark(
      "1" = setValue(id = "x_1", value = 10, envir = envir, binding = b1,
        binding_type = 1),
      "2" = getValue(id = "x_1", envir = envir),
      "3" = setValue(id = "x_2", envir = envir, binding = b2, watch = "x_1"),
      "4" = getValue(id = "x_2", envir = envir),
      "5" = setValue(id = "x_1", value = 100, envir = envir),
      "6" = getValue(id = "x_2", envir = envir),
      control = list(order = "inorder")
    )
    res_2

     #-----------------

    foo <- function(x) {
      x + 10
    }
    setGeneric(name = "bar", signature=c("x"), def = function(x) standardGeneric)
    setMethod(f = "bar", signature = c(x = "ANY"), definition = function(x) {
      x + 10
    })
    res_3 <- microbenchmark(
      "1" = foo(x = 10),
      "2" = bar(x = 10)
    )
    res_3
  }
  
})
