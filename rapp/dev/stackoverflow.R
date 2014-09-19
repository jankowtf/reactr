setValue <- function(
  id,
  value,
  envir,
  observe = NULL,
  binding = NULL,
  ...
) {

  ## Auxiliary environments //
  if (!exists(".bindings", envir, inherits = FALSE)) {
    assign(".bindings", new.env(), envir)
  }    
  if (!exists(".hash", envir, inherits = FALSE)) {
    assign(".hash", new.env(), envir)
  }
  if (!exists(".observe", envir, inherits = FALSE)) {
    assign(".observe", new.env(), envir)
  }
  if (!exists(id, envir$.hash, inherits = FALSE)) {
    assign(id, new.env(), envir$.hash)  
  }
  
  ## Decide what type of value we have //
  if (!is.null(observe) && !is.null(binding)) {
    has_binding <- TRUE
  } else {
    has_binding <- FALSE
  }
  
  ## Set //
  if (has_binding) {
  ## Value with binding //
    ## Get hash value of observed value //
    assign(id, get(observe, envir$.hash[[observe]]), envir$.hash[[observe]])
    ## Compute actual value:
    out <- binding(x = get(observe, envir))
    ## Store Actual value:
    assign(id, out, envir)
    ## Store hash value :
    assign(id, digest::digest(out), envir$.hash[[id]])
    ## Store binding :
    assign(id, binding, envir$.bindings)    
    ## Store observed value:
    assign(id, observe, envir$.observe)    
  } else {
  ## Regular value without binding //
    ## Store actual value:
    out <- assign(id, value, envir)
    ## Store hash value:
    assign(id, digest::digest(value), envir$.hash[[id]])
  }
  
  return(out)

}

getValue <- function(
  id,
  envir,
  ...
) {

  ## Check for observed value //
  observe <- envir$.observe[[id]]
  
  ## Get //
  if (!is.null(observe)) {
  ## Check if any of observed values have changed //
  ## Note: currently only tested with bindings that only 
  ## take one observed value 
    idx <- sapply(observe, function(ii) {
      hash_0 <- get(ii, envir$.hash[[ii]], inherits = FALSE)
      hash_1 <- get(id, envir$.hash[[ii]], inherits = FALSE)
      hash_0 != hash_1
    })

    ## Update required //
    if (any(idx)) {
      out <- setValue(
        id = id, 
        envir = envir, 
        binding = get(id, envir$.bindings, inherits = FALSE),
        observe = observe
      )
    } else {
      out <- get(id, envir, inherits = FALSE)
    }
  } else {
    out <- get(id, envir, inherits = FALSE)
  }
      
  return(out)
    
}

##------------------------------------------------------------------------------
## Apply //
##------------------------------------------------------------------------------

envir <- new.env()  

## Set regular value //
setValue(id = "x_1", value = Sys.time(), envir = envir)
getValue(id = "x_1", envir = envir)

## Set value with binding to observed variable 'x_1' //
setValue(
  id = "x_2", 
  envir = envir,
  binding = function(x) {
    x + 60*60*24
  }, 
  observe = "x_1"
)
## As long as observed variable does not change, 
## value of 'x_2' will also not change
getValue(id = "x_2", envir = envir)

## Change value of observed variable 'x_1' //
setValue(id = "x_1", value = Sys.time(), envir = envir)
## Value of 'x_2' will change according to binding function:
getValue(id = "x_2", envir = envir)

##------------------------------------------------------------------------------
## Profiling //
##------------------------------------------------------------------------------

require(microbenchmark)

envir <- new.env()  
binding <- function(x) {
  x + 60*60*24
}

microbenchmark(
  "1" = setValue(id = "x_1", value = Sys.time(), envir = envir),
  "2" = getValue(id = "x_1", envir = envir),
  "3" = setValue(id = "x_2", envir = envir,
    binding = binding, observe = "x_1"),
  "4" = getValue(id = "x_2", envir = envir),
  "5" = setValue(id = "x_1", value = Sys.time(), envir = envir),
  "6" = getValue(id = "x_2", envir = envir)
)
# Unit: microseconds
#  expr     min       lq   median       uq      max neval
#     1 108.620 111.8275 115.4620 130.2155 1294.881   100
#     2   4.704   6.4150   6.8425   7.2710   17.106   100
#     3 178.324 183.6705 188.5880 247.1735  385.300   100
#     4  43.620  49.3925  54.0965  92.7975  448.591   100
#     5 109.047 112.0415 114.1800 159.2945  223.654   100
#     6  43.620  47.6815  50.8895 100.9225  445.169   100

##------------------------------------------------------------------------------
## Ansers 
##------------------------------------------------------------------------------

?makeActiveBinding
# tcl/tk GUI:
# https://github.com/floybix/playwith/tree/master/R

# locking environments
e <- new.env()
assign("x", 1, envir = e)
get("x", envir = e)
lockEnvironment(e)
get("x", envir = e)
assign("x", 2, envir = e)
try(assign("y", 2, envir = e)) # error

# locking bindings
e <- new.env()
assign("x", 1, envir = e)
get("x", envir = e)
lockBinding("x", e)
try(assign("x", 2, envir = e)) # error
unlockBinding("x", e)
assign("x", 2, envir = e)
get("x", envir = e)

# active bindings
f <- local( {
    x <- 1
    function(v) {
       if (missing(v))
           cat("get\n")
       else {
           cat("set\n")
           x <<- v
       }
       x
    }
})
makeActiveBinding("fred", f, .GlobalEnv)
bindingIsActive("fred", .GlobalEnv)
fred
fred <- 2
fred

###################


# #' @title
# #' Set Value (character,ANY,environment,missing,call)
# #'
# #' @description 
# #' See generic: \code{\link[reactr]{setValue}}
# #'      
# #' @inheritParams setValue
# #' @param id \code{\link{character}}.
# #' @param value \code{\link{missing}}.
# #' @param envir \code{\link{environment}}.
# #' @param watch \code{\link{character}}.
# #' @param binding \code{\link{call}}.
# #' @return \code{\link{ANY}}. Value of \code{value} or the return value 
# #'    of the function inside \code{binding}.
# #' @example inst/examples/setValue.r
# #' @seealso \code{
# #'    Generic: \link[reactr]{setValue}
# #' }
# #' @template author
# #' @template references
# #' @export
# setMethod(
#   f = "setValue", 
#   signature = signature(
#     id = "character",
#     value = "missing",
#     envir = "environment",
#     watch = "character",
#     binding = "call"
#   ), 
#   definition = function(
#     id,
#     value,
#     envir,
#     watch,
#     binding,
#     binding_type,
#     ...
#   ) {
# 
#   .mthd <- selectMethod("setValue",
#     signature=c(
#       id = class(id),
#       value = "ANY", 
#       envir = class(envir),
#       watch = class(watch),
#       binding = class(binding)
#     )
#   )
#   return(.mthd(
#     id = id,
#     value = TRUE,
#     envir = envir,
#     watch = watch,
#     binding = binding,
#     binding_type = binding_type,
#     ...
#   ))
#   
#   }
# )


