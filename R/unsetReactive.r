#' @title
#' Unset Reactive Object
#'
#' @description 
#' Removes the reactive binding from an object
#' See main method 
#' \code{\link{unsetReactive-character-environment-method}}
#' 
#' @section Implications with respect to observing variables:
#' If other reactive variables have been observing the reactive variable that
#' has been unset, from this point on they will simply return the last value
#' that has been cached.
#' 
#' @note
#' The main S4 method is 
#' \code{\link[reactr]{unsetReactive-character-environment-method}}.
#'     
#' @param id \strong{Signature argument}.
#'    Object containing path-like ID information.
#' @param where \strong{Signature argument}.
#'    Object containing location information.
#' @param .hash_id \code{\link{character}}.
#'    Name of the auxiliary environment for caching hash values. 
#'    Default: \code{"._HASH"}. Keep it unless this name is already taken in 
#'    either \code{where} or \code{where_watch}.
#' @param .tracelevel \code{\link{numeric}}.
#'    Verbosity level for tracing purposes. Value of \code{0} means 
#'    \emph{no tracing} whereas values of \code{> 0} can be used to fine 
#'    control tracing. The trace level can also be set as a global option when
#'    using package \code{tracer} (\strong{not functional yet}).
#' @template threedot
#' @example inst/examples/unsetReactive.r
#' @seealso \code{
#'   	\link[reactr]{unsetReactive-character-environment-method}
#' }
#' @template author
#' @template references
#' @export 
setGeneric(
  name = "unsetReactive",
  signature = c(
    "id",
    "where"
  ),
  def = function(
    id,
    where = .GlobalEnv,
    .hash_id = "._HASH",
    .tracelevel = 0,
    ...
  ) {
    standardGeneric("unsetReactive")       
  }
)

#' @title
#' Unset Reactive Object
#'
#' @description 
#' See generic: \code{\link[reactr]{unsetReactive}}
#'      
#' @inheritParams unsetReactive
#' @param id \code{\link{character}}.
#' @param where \code{\link{missing}}.
#'    Internal argument that should not be set explicitly.
#'    The value at runtime will correspond to the function that has been 
#'    provided via argument \code{binding}.
#' @return See method
#'    \code{\link[reactr]{unsetReactive-character-environment-method}}
#' @example inst/examples/unsetReactive.r
#' @seealso \code{
#'    Generic: \link[reactr]{unsetReactive}
#' }
#' @template author
#' @template references
#' @aliases unsetReactive-method_main 
#' @export
setMethod(
  f = "unsetReactive", 
  signature = signature(
    id = "character",
    where = "missing"
  ), 
  definition = function(
    id,
    where,
    .hash_id,
    .tracelevel,
    ...
  ) {
  
  return(unsetReactive(
    id = id,
    where = where,
    .hash_id = .hash_id,
    .tracelevel = .tracelevel,
    ...
  ))
  
  }
)

#' @title
#' Unset Reactive Object
#'
#' @description 
#' See generic: \code{\link[reactr]{unsetReactive}}
#'   	 
#' @inheritParams unsetReactive
#' @param id \code{\link{character}}.
#' @param where \code{\link{environment}}.
#'    Internal argument that should not be set explicitly.
#'    The value at runtime will correspond to the function that has been 
#'    provided via argument \code{binding}.
#' @return \code{\link{logical}}. \code{TRUE}.
#' @example inst/examples/unsetReactive.r
#' @seealso \code{
#'    Generic: \link[reactr]{unsetReactive}
#' }
#' @template author
#' @template references
#' @aliases unsetReactive-method_main 
#' @export
setMethod(
  f = "unsetReactive", 
  signature = signature(
    id = "character",
    where = "environment"
  ), 
  definition = function(
    id,
    where,
    .hash_id,
    .tracelevel,
    ...
  ) {

  if (exists(id, envir = where, inherits = FALSE)) {
    has_binding <- try(bindingIsActive(id, where))
    if (inherits(has_binding, "try-error")) {
      has_binding <- FALSE
    } 
    if (has_binding) {
      tmp <- get(id, envir = where, inherits = FALSE)
      rm(list = id, envir = where, inherits = FALSE)
      assign(id, tmp, where)
      ## Remove hash registry entry //
      removeFromHashRegistry(id = id, where = where, .hash_id = .hash_id, ...)
    }
  }
    
  return(TRUE)
  
  }
)
