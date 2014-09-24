x_1 <- new.env()  
x_2 <- new.env()  
  
setReactive(id = "field_1", value = Sys.time(), envir = x_1)
getThis(id = "x_1", envir = envir)

binding <- substitute(function(x) {
  x + 60*60*24
})
setReactive(id = "x_2", envir = envir, binding = binding, watch = "x_1", 
         binding_type = 2)
getThis(id = "x_2", envir = envir)  
  
## Change value of monitored variable //
setReactive(id = "x_1", value = Sys.time(), envir = envir, binding_type = 2)
getThis(id = "x_1", envir = envir)  
getThis(id = "x_2", envir = envir)    
 
##----------------------------------------------------------------------------
## Setting via 'makeActiveBinding' //
##----------------------------------------------------------------------------
  
## Variable that can be monitored //
envir <- new.env()
b1 <- substitute(
  local({
    VALUE <- NULL
    function(v) {
      if (!exists(id, envir[[.hash_id]])) {
        assign(id, new.env(), envir = envir[[.hash_id]])
      }
      if (!missing(v)) {
        VALUE <<- v
      }
      ## Ensure hash value //
      assign(id, digest::digest(VALUE), envir[[.hash_id]][[id]])
      VALUE
    }
  }),
  list(VALUE = as.name("value"))
)

setReactive(
  id = "x_1", 
  value = 10, 
  envir = envir, 
  binding = b1,
  binding_type = 1
)
envir$x_1

## Set variable that monitors 'x_1' //
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
    if (  exists(watch, envir = envir[[.hash_id]][[watch]], inherits = FALSE) &&
          !exists(id, envir = envir[[.hash_id]][[watch]], inherits = FALSE
    )) {
      assign(
        id, 
        get(watch, envir = envir[[.hash_id]][[watch]]),
        envir[[.hash_id]][[watch]]
      )
    }

    function(v) {
      if (  exists(watch, envir = envir, FALSE) &
            !is.null(get(watch, envir = envir, FALSE))
      ) {        
        if (missing(v)) {
          hash_0 <- envir[[.hash_id]][[watch]][[watch]]
          hash_1 <- envir[[.hash_id]][[watch]][[id]]
#           message(hash_0)
#           message(hash_1)
          if (hash_0 != hash_1) {
#             message("monitored variable has changed:")
#             message("updating")
            VALUE <<- envir[[watch]] + 100
            envir[[.hash_id]][[watch]][[id]] <- hash_0
          }
        }
      }
      VALUE
    }
  }),
  list(VALUE = as.name("value"))
)

setReactive(
  id = "x_2", 
  envir = envir, 
  watch = "x_1", 
  binding = b2, 
  binding_type = 1
)

envir$x_1
envir$x_2
envir$x_2
envir$x_1 <- 10
envir$x_1
envir$x_2
envir$x_2

setReactive(
  id = "x_1", 
  value = 100, 
  envir = envir, 
  binding = binding,
  binding_type = 1
)
envir$x_2
getThis("x_2", envir = envir)

##----------------------------------------------------------------------------
## Profiling //
##----------------------------------------------------------------------------

require("microbenchmark")

envir <- new.env()  
res_1 <- microbenchmark(
  "1" = setReactive(id = "x_1", value = Sys.time(), envir = envir, binding_type = 2),
  "2" = getThis(id = "x_1", envir = envir),
  "3" = setReactive(id = "x_2", envir = envir,
    binding = substitute(function(x) {
        x + 60*60*24
      }), watch = "x_1", binding_type = 2),
  "4" = getThis(id = "x_2", envir = envir),
  "5" = setReactive(id = "x_1", value = Sys.time(), envir = envir,
                 binding_type = 2),
  "6" = getThis(id = "x_2", envir = envir),
  control = list(order = "inorder", warmup = 10)
)
res_1

##-----------

envir <- new.env()
res_2 <- microbenchmark(
  "1" = setReactive(id = "x_1", value = 10, envir = envir, binding = b1,
    binding_type = 1),
  "2" = getThis(id = "x_1", envir = envir),
  "3" = setReactive(id = "x_2", envir = envir, binding = b2, watch = "x_1"),
  "4" = getThis(id = "x_2", envir = envir),
  "5" = setReactive(id = "x_1", value = 100, envir = envir),
  "6" = getThis(id = "x_2", envir = envir),
  control = list(order = "inorder", warmup = 10)
)
res_2

res_1
res_2
