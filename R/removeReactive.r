#' @title
#' Remove Reactive Object
#'
#' @description 
#' Removes a reactive object from its environment. 
#' 
#' @details
#' \strong{Note that tis is different from unsetting a reactive object 
#' via \code{\link[reactr]{removeReactive}}}. It is equivalent to 
#' \code{\link[base]{rm}} with a previous call to 
#' \code{\link[reactr]{unsetReactive}}. 
#' 
#' @section Implications with respect to observing variables:
#' If other reactive variables have been observing the reactive variable that
#' has been removed, from this point on they will simply return the last value
#' that has been cached if \code{strict = FALSE} or \code{NULL} if 
#' \code{strict = TRUE} when the observing object was set via 
#' \code{\link[reactr]{setReactive}}
#' 
#' @note
#' The main S4 method is 
#' \code{\link[reactr]{removeReactive-character-environment-method}}.
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
#' @example inst/examples/removeReactive.r
#' @seealso \code{
#'   	\link[reactr]{removeReactive-character-environment-method}
#' }
#' @template author
#' @template references
#' @export 
setGeneric(
  name = "removeReactive",
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
    standardGeneric("removeReactive")       
  }
)

#' @title
#' Unset Reactive Object
#'
#' @description 
#' See generic: \code{\link[reactr]{removeReactive}}
#'      
#' @inheritParams removeReactive
#' @param id \code{\link{character}}.
#' @param where \code{\link{missing}}.
#'    Internal argument that should not be set explicitly.
#'    The value at runtime will correspond to the function that has been 
#'    provided via argument \code{binding}.
#' @return See method
#'    \code{\link[reactr]{removeReactive-character-environment-method}}
#' @example inst/examples/removeReactive.r
#' @seealso \code{
#'    Generic: \link[reactr]{removeReactive}
#' }
#' @template author
#' @template references
#' @aliases removeReactive-method_main 
#' @export
setMethod(
  f = "removeReactive", 
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
  
  return(removeReactive(
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
#' See generic: \code{\link[reactr]{removeReactive}}
#'   	 
#' @inheritParams removeReactive
#' @param id \code{\link{character}}.
#' @param where \code{\link{environment}}.
#'    Internal argument that should not be set explicitly.
#'    The value at runtime will correspond to the function that has been 
#'    provided via argument \code{binding}.
#' @return \code{\link{logical}}. \code{TRUE}.
#' @example inst/examples/removeReactive.r
#' @seealso \code{
#'    Generic: \link[reactr]{removeReactive}
#' }
#' @template author
#' @template references
#' @aliases removeReactive-method_main 
#' @export
setMethod(
  f = "removeReactive", 
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
      unsetReactive(id = id, where = where, .hash_id = .hash_id)
      rm(list = id, envir = where, inherits = FALSE)
    }
  }
    
  return(TRUE)
  
  }
)
