#' @title
#' Set Value (Generic)
#'
#' @description 
#' Sets a variable value in an environment.
#' 
#' @details
#' If \code{binding_type = 1} (the current default), then the 
#' actual binding is established via \code{\link[base]{makeActiveBinding}}. 
#' This implies that the binding function \code{binding} needs to have a 
#' specific structure as required by both \code{\link[base]{makeActiveBinding}}
#' and \code{setValue} itself. This in turn depends on what type of variable 
#' you want to set: a variable that should be \emph{monitored} by other variables 
#' (\emph{monitored}) or a variable that \emph{monitors} another 
#' variable (\emp{monitoring}). 
#' 
#' \itemize{
#'    \item{for \emph{monitored}} {
#'      
#'    }
#'    \item{for \emph{monitoring}} {
#'    }
#' }
#' 
#' @note 
#' \enumerate{
#'    \item{
#'    As binding functions require the use of \code{\link[base]{local}}, they need
#' to be wrapped by \code{\link[base]{substitute}} in order to delay their actual
#' evaluation to the actual call of \code{\link{makeActiveBinding}} inside
#' this function.}
#'    \item{
#'    When choosing \code{binding_type = "makeActiveBinding"}, it is recommended
#'    to also provide binding functions for "regular" variables as this will
#'    take care of all the details for the variable turning into a monitored 
#'    variable later on. If you don't do that, you have to explicitly take care
#'    of reassigning the respective variable accordingly before it can be 
#'    monitored by another variable
#'    }  
#' }
#'     
#' @param id \strong{Signature argument}.
#'    Object containing path-like ID information.
#' @param value \strong{Signature argument}.
#'    Object containing value information.
#' @param envir \strong{Signature argument}.
#'    Object containing environment information.
#' @param watch \code{\link{character}}.
#'    Name of a variable to watch.
#' @param binding \code{\link{call}}  
#'    Function that defines the binding relationship between \code{id} and 
#'    \code{watch}. See details.
#' @param binding_type \code{\link{integer}} or \code{\link{numerical}}.
#' \itemize{
#'    \item{\code{1}:} {
#'    use \code{\link{makeActiveBinding}} to establish the binding
#'    }
#'    \item{\code{2}:} {
#'    use custom way of establishing the binding
#'    }
#' }
#' @template threedot
#' @example inst/examples/setValue.r
#' @seealso \code{
#'   	\link[reactr]{setValue-character-ANY-environment-method}
#' }
#' @template author
#' @template references
#' @export 
setGeneric(
  name = "setValue",
  signature = c(
    "id",
    "value",
    "envir",
    "watch",
    "binding"
  ),
  def = function(
    id,
    value = NULL,
    envir = new.env(),
    watch,
    binding,
    binding_type = 1,
    ...
  ) {
    standardGeneric("setValue")       
  }
)

#' @title
#' Set Value (ANY,ANY,ANY,ANY,ANY)
#'
#' @description 
#' See generic: \code{\link[reactr]{setValue}}
#'      
#' @inheritParams setValue
#' @param id \code{\link{ANY}}.
#' @param value \code{\link{ANY}}.
#' @param envir \code{\link{ANY}}.
#' @param watch \code{\link{ANY}}.
#' @param binding \code{\link{ANY}}.
#' @return \code{\link{ANY}}. Value of \code{value} or the return value 
#'    of the function inside \code{binding}.
#' @example inst/examples/setValue.r
#' @seealso \code{
#'    Generic: \link[reactr]{setValue}
#' }
#' @template author
#' @template references
#' @export
setMethod(
  f = "setValue", 
  signature = signature(
    id = "character",
    value = "ANY",
    envir = "environment",
    watch = "missing",
    binding = "missing"
  ), 
  definition = function(
    id,
    value,
    envir,
    watch,
    binding,
    binding_type,
    ...
  ) {
  
  mc <- match.call()
  mc[[1]] <- quote(setValue)
  eval(mc)
  
  }
)

#' @title
#' Set Value (character,ANY,environment,missing,missing)
#'
#' @description 
#' See generic: \code{\link[reactr]{setValue}}
#'      
#' @inheritParams setValue
#' @param id \code{\link{character}}.
#' @param value \code{\link{ANY}}.
#' @param envir \code{\link{environment}}.
#' @param watch \code{\link{missing}}.
#' @param binding \code{\link{missing}}.
#' @return \code{\link{ANY}}. Value of \code{value} or the return value 
#'    of the function inside \code{binding}.
#' @example inst/examples/setValue.r
#' @seealso \code{
#'    Generic: \link[reactr]{setValue}
#' }
#' @template author
#' @template references
#' @export
setMethod(
  f = "setValue", 
  signature = signature(
    id = "character",
    value = "ANY",
    envir = "environment",
    watch = "missing",
    binding = "missing"
  ), 
  definition = function(
    id,
    value,
    envir,
    watch,
    binding,
    binding_type,
    ...
  ) {
  
  return(setValue(
    id = id,
    value = value,
    envir = envir,
    watch = character(),
    binding = substitute(expression()),
    binding_type = binding_type,
    ...
  ))
  
  }
)

#' @title
#' Set Value (character,ANY,environment,missing,call)
#'
#' @description 
#' See generic: \code{\link[reactr]{setValue}}
#'      
#' @inheritParams setValue
#' @param id \code{\link{character}}.
#' @param value \code{\link{ANY}}.
#' @param envir \code{\link{environment}}.
#' @param watch \code{\link{missing}}.
#' @param binding \code{\link{call}}.
#' @return \code{\link{ANY}}. Value of \code{value} or the return value 
#'    of the function inside \code{binding}.
#' @example inst/examples/setValue.r
#' @seealso \code{
#'    Generic: \link[reactr]{setValue}
#' }
#' @template author
#' @template references
#' @export
setMethod(
  f = "setValue", 
  signature = signature(
    id = "character",
    value = "ANY",
    envir = "environment",
    watch = "missing",
    binding = "call"
  ), 
  definition = function(
    id,
    value,
    envir,
    watch,
    binding,
    binding_type,
    ...
  ) {

  return(setValue(
    id = id,
    value = value,
    envir = envir,
    watch = character(),
    binding = binding,
    binding_type = binding_type,
    ...
  ))
  
  }
)

#' @title
#' Set Value (character,ANY,environment,missing,call)
#'
#' @description 
#' See generic: \code{\link[reactr]{setValue}}
#'      
#' @inheritParams setValue
#' @param id \code{\link{character}}.
#' @param value \code{\link{ANY}}.
#' @param envir \code{\link{environment}}.
#' @param watch \code{\link{character}}.
#' @param binding \code{\link{function}}.
#' @return \code{\link{ANY}}. Value of \code{value} or the return value 
#'    of the function inside \code{binding}.
#' @example inst/examples/setValue.r
#' @seealso \code{
#'    Generic: \link[reactr]{setValue}
#' }
#' @template author
#' @template references
#' @export
setMethod(
  f = "setValue", 
  signature = signature(
    id = "character",
    value = "ANY",
    envir = "environment",
    watch = "character",
    binding = "function"
  ), 
  definition = function(
    id,
    value,
    envir,
    watch,
    binding,
    binding_type,
    ...
  ) {

  return(setValue(
    id = id,
    value = value,
    envir = envir,
    watch = watch,
    binding = substitute(BINDING, list(BINDING = binding)),
    binding_type = binding_type,
    ...
  ))
  
  }
)



#' @title
#' Set Value (character,ANY,environment,character,call)
#'
#' @description 
#' See generic: \code{\link[reactr]{setValue}}
#'   	 
#' @inheritParams setValue
#' @param id \code{\link{character}}.
#' @param value \code{\link{ANY}}.
#' @param envir \code{\link{environment}}.
#' @param watch \code{\link{character}}.
#' @param binding \code{\link{call}}.
#' @return \code{\link{ANY}}. Value of \code{value} or the return value 
#'    of the function inside \code{binding}.
#' @example inst/examples/setValue.r
#' @seealso \code{
#'    Generic: \link[reactr]{setValue}
#' }
#' @template author
#' @template references
#' @aliases setValue-main-method
#' @export
setMethod(
  f = "setValue", 
  signature = signature(
    id = "character",
    value = "ANY",
    envir = "environment",
    watch = "character",
    binding = "call"
  ), 
  definition = function(
    id,
    value,
    envir,
    watch,
    binding,
    binding_type,
    ...
  ) {

  ## Validate binding type //
#   binding_type <- as.numeric(match.arg(as.character(binding_type), c("1", "2")))
    binding_type
  ## Ensure environment that caches hash keys //
  if (!exists(".hash", envir, inherits = FALSE)) {
    assign(".hash", new.env(), envir)
  }    

  if (binding_type == 1) {
    has_binding <- try(bindingIsActive(id, envir), silent = TRUE)
    if (inherits(has_binding, "try-error")) {
      has_binding <- FALSE
    } else {
      has_binding <- has_binding
    }
    if (!has_binding) {
    ## Register variable with binding for the first time //
      ## Note: this only takes care of "registering" the variable!    
      makeActiveBinding(id, eval(binding), envir)  
#       update_var <- FALSE
    } else {
#       update_var <- TRUE
      out <- assign(id, value, envir = envir)
    }
    if (!length(watch) || has_binding) {
      ## Actually set desired value or pdate previously set variable //
      out <- assign(id, value, envir = envir)
    } else {
      out <- get(id, envir = envir, inherits = FALSE)
    }
  } else if (binding_type == 2) {
    ## Ensure auxiliary environments used for caching //
    if (!exists(id, envir$.hash, inherits = FALSE)) {
      assign(id, new.env(), envir$.hash)  
    }
    if (!exists(".bindings", envir, inherits = FALSE)) {
      assign(".bindings", new.env(), envir)
    }    
    if (!exists(".watch", envir, inherits = FALSE)) {
      assign(".watch", new.env(), envir)
    }
    
    ## Find out what kind of variable should be set //
    if (!length(watch) && deparse(binding)[1] == "expression()") {
      has_binding <- FALSE
    } else {
      has_binding <- TRUE
    }
    
    if (has_binding) {
    ## Variable monitors another variable //
      ## Retrieve hash key for monitored variable and transfer:
      assign(id, get(watch, envir$.hash[[watch]]), envir$.hash[[watch]])
      ## Compute value based on binding function:
      out <- eval(binding)(x = get(watch, envir))
      ## Cache current binding value:
      assign(id, out, envir)
      ## Compute and cache own hash value:
      assign(id, digest::digest(out), envir$.hash[[id]])
      ## Cache binding function:
      assign(id, binding, envir$.bindings)    
      ## Cache name of monitored variable:
      assign(id, watch, envir$.watch)    
    } else {
    ## Variable does not monitor another variable //
      ## Set:
      out <- assign(id, value, envir)
      ## Compute and cache hash value:
      assign(id, digest::digest(value), envir$.hash[[id]])
    }
  } else {
    stop(paste0("Something went wrong with 'binding_type'"))
  }
  
  return(out)
  
  }
)
