#' @title
#' Set Value (generic)
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
#' @param where \strong{Signature argument}.
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
#'   	\link[reactr]{setValue-character-ANY-environment-character-call-method}
#' }
#' @template author
#' @template references
#' @export 
setGeneric(
  name = "setValue",
  signature = c(
    "id",
    "value",
    "where",
    "watch",
    "binding"
  ),
  def = function(
    id,
    value = NULL,
    where = new.env(),
    watch = character(),
    binding = substitute(expression()),
    binding_type = 1,
    ...
  ) {
    standardGeneric("setValue")       
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
#' @param where \code{\link{environment}}.
#' @param watch \code{\link{missing}}.
#' @param binding \code{\link{missing}}.
#' @return See method
#'    \code{\link[reactr]{setValue-character-ANY-environment-character-call-method}}.
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
    where = "environment",
    watch = "missing",
    binding = "missing"
  ), 
  definition = function(
    id,
    value,
    where,
    watch,
    binding,
    binding_type,
    ...
  ) {
  
  return(setValue(
    id = id,
    value = value,
    where = where,
    watch = watch,
    binding = substitute(binding),
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
#' @param where \code{\link{environment}}.
#' @param watch \code{\link{missing}}.
#' @param binding \code{\link{call}}.
#' @return See method
#'    \code{\link[reactr]{setValue-character-ANY-environment-character-call-method}}.
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
    where = "environment",
    watch = "missing",
    binding = "call"
  ), 
  definition = function(
    id,
    value,
    where,
    watch,
    binding,
    binding_type,
    ...
  ) {

  return(setValue(
    id = id,
    value = value,
    where = where,
    watch = watch,
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
#' @param where \code{\link{environment}}.
#' @param watch \code{\link{character}}.
#' @param binding \code{\link{missing}}.
#' @return See method
#'    \code{\link[reactr]{setValue-character-ANY-environment-character-call-method}}.
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
    where = "environment",
    watch = "character",
    binding = "missing"
  ), 
  definition = function(
    id,
    value,
    where,
    watch,
    binding,
    binding_type,
    ...
  ) {

  return(setValue(
    id = id,
    value = value,
    where = where,
    watch = watch,
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
#' @param where \code{\link{environment}}.
#' @param watch \code{\link{character}}.
#' @param binding \code{\link{function}}.
#' @return See method
#'    \code{\link[reactr]{setValue-character-ANY-environment-character-call-method}}.
#' @example inst/examples/setValue.r
#' @seealso \code{
#'    Generic: \link[reactr]{setValue}
#' }
#' @template author
#' @template references
#' @export
#' @import classr
setMethod(
  f = "setValue", 
  signature = signature(
    id = "character",
    value = "ANY",
    where = "environment",
    watch = "character",
    binding = "function"
  ), 
  definition = function(
    id,
    value,
    where,
    watch,
    binding,
    binding_type,
    ...
  ) {

  return(setValue(
    id = id,
    value = value,
    where = where,
    watch = watch,
    binding = getBoilerplateCode(ns = classr::createClassInstance(
      cl = "Reactr.BindingContractGet.S3")),
    binding_type = binding_type,
    .binding = binding,
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
#' @param where \code{\link{environment}}.
#' @param watch \code{\link{character}}.
#' @param binding \code{\link{call}}.
#' @param .binding \code{\link{function}}. 
#'    Internal argument that should not be set explicitly.
#'    The value at runtime will correspond to the function that has been 
#'    provided via argument \code{binding}.
#' @return \code{\link{ANY}}. Value of \code{value} or the return value 
#'    of the binding function.
#' @example inst/examples/setValue.r
#' @seealso \code{
#'    Generic: \link[reactr]{setValue}
#' }
#' @template author
#' @template references
#' @aliases setValue-method_main 
#' @export
#' @import digest
#' @import classr
setMethod(
  f = "setValue", 
  signature = signature(
    id = "character",
    value = "ANY",
    where = "environment",
    watch = "character",
    binding = "call"
  ), 
  definition = function(
    id,
    value,
    where,
    watch,
    binding,
    binding_type,
    .binding = NULL,
    ...
  ) {

  ## Validate binding type //
#   binding_type <- as.numeric(match.arg(as.character(binding_type), c("1", "2")))
    binding_type
    force_value <- FALSE
    ## Binding interface //
    if (  deparse(binding) %in% c("expression()", "substitute(expression())") && 
          binding_type == 1
    ) {
      if (!length(watch)) {
        binding <- getBoilerplateCode(ns = classr::createClassInstance(
          cl = "Reactr.BindingContractSet.S3"))
      } else {
        if (length(value)) {
          ## Variables that binding boilerplate needs to find //
          .binding <- function(x) {x}
          force_value <- TRUE
          value_force <- value
# print(force_value)          
          binding <- getBoilerplateCode(ns = classr::createClassInstance(
            cl = "Reactr.BindingContractCombined.S3"))
# print(binding)          
        } else {
# print("HÄH")          
          .binding <- function(x) {x}
          binding <- getBoilerplateCode(ns = classr::createClassInstance(
            cl = "Reactr.BindingContractGet.S3"))
        }
      }
    }
    
#     print(binding)
#     print(list(...))
#     print(.binding)
  ## Implement binding interface //
#   bindingFunction <- binding
#   binding <- processBoilerplateCode(fun = binding)
    
  ## Ensure environment that caches hash keys //
  if (!exists(".hash", envir = where, inherits = FALSE)) {
    assign(".hash", new.env(), envir = where)
  }    
  ensureHashRegistryState(id = id, where = where)

  if (binding_type == 1) {
    has_binding <- try(bindingIsActive(id, where), silent = TRUE)
    if (inherits(has_binding, "try-error")) {
      has_binding <- FALSE
    } else {
      has_binding <- has_binding
    }
    ensureHashRegistryState(id = id, watch = watch, where = where)
    if (!has_binding) {
    ## Register variable with binding for the first time //
#       ensureHashRegistryState(id = id, watch = watch, where = where)
      ## Note: this only takes care of "registering" the variable!    
      makeActiveBinding(id, eval(binding), where)  
    } else {
      out <- assign(id, value, envir = where)
    }

    if (!length(watch) || has_binding) {
      ## Actually set desired value or pdate previously set variable //
      out <- assign(id, value, envir = where)
    } else {
      out <- get(id, envir = where, inherits = FALSE)
    }
## Ensure reset of forced value //
if (force_value) {
  force_value <- FALSE
}
print("OK until here")
  } else if (binding_type == 2) {
    ## Ensure auxiliary environments used for caching //
    if (!exists(id, envir = where$.hash, inherits = FALSE)) {
      assign(id, new.env(), envir = where$.hash)  
    }
    if (!exists(".bindings", envir = where, inherits = FALSE)) {
      assign(".bindings", new.env(), envir = where)
    }    
    if (!exists(".watch", envir = where, inherits = FALSE)) {
      assign(".watch", new.env(), envir = where)
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
      assign(
        id, 
        value = get(watch, envir = where$.hash[[watch]], inherits = FALSE), 
        envir = where$.hash[[watch]]
      )
      ## Compute value based on binding function:
      out <- eval(binding)(x = get(watch, envir = where, inherits = FALSE))
      ## Cache current binding value:
      assign(id, out, envir = where)
      ## Compute and cache own hash value:
      assign(id, digest::digest(out), envir = where$.hash[[id]])
      ## Cache binding function:
      assign(id, binding, envir = where$.bindings)    
      ## Cache name of monitored variable:
      assign(id, watch, envir = where$.watch)    
    } else {
    ## Variable does not monitor another variable //
      ## Set:
      out <- assign(id, value, envir = where)
      ## Compute and cache hash value:
      assign(id, digest::digest(value), envir = where$.hash[[id]])
    }
  } else {
    stop(paste0("Something went wrong with 'binding_type'"))
  }
  
  return(out)
  
  }
)
