#' @title
#' Unset Reactive Object (generic)
#'
#' @description 
#' Removes the reactive \strong{character} from an object, i.e. its 
#' binding(s) to other objects and the ability for other objects to have 
#' bindings to it. It is equivalent to transforming the object to one that 
#' has been assigned via \code{\link[base]{assign}} or \code{\link[base]{<-}} 
#' instead of \code{\link[reactr]{setReactiveS3}} and thus in turn 
#' by \code{\link[base]{makeActiveBinding}}. 
#' 
#' Note that it is \strong{not} equivalent to removing/deleting the object! 
#' See \code{\link[reactr]{removeReactive}} for this purpose.
#' 
#' @section Implications with respect to observing variables:
#' 
#' If other reactive variables have been observing the reactive variable that
#' has been unset, from this point on they will simply return the last value
#' that has been cached. \strong{So there is no actual reactive binding anymore}.
#' 
#' @note
#' The main S4 method is 
#' \code{\link[reactr]{unsetReactive-character-environment-method}}.
#'     
#' @param id \strong{Signature argument}.
#'    Object containing path-like ID information.
#' @param where \strong{Signature argument}.
#'    Object containing location information.
#' @template threedots
#' @example inst/examples/unsetReactive.r
#' @seealso \code{
#'   	\link[reactr]{unsetReactive-character-environment-method},
#'    \link[reactr]{unsetReactiveByUid}
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
    where = parent.frame(),
    ...
  ) {
    standardGeneric("unsetReactive")       
  }
)

#' @title
#' Unset Reactive Object (character-missing)
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
#'    Generic: \link[reactr]{unsetReactive},
#'    \link[reactr]{unsetReactiveByUid}
#' }
#' @template author
#' @template references
#' @aliases unsetReactive-method_main 
#' @export
#' @aliases unsetReactive-character-missing-method
setMethod(
  f = "unsetReactive", 
  signature = signature(
    id = "character",
    where = "missing"
  ), 
  definition = function(
    id,
    where,
    ...
  ) {
  
  return(unsetReactive(
    id = id,
    where = where,
    ...
  ))
  
  }
)

#' @title
#' Unset Reactive Object (character-environment)
#'
#' @description 
#' See generic: \code{\link[reactr]{unsetReactive}}
#'   	 
#' @inheritParams unsetReactive
#' @param id \code{\link{character}}.
#' @param where \code{\link{environment}}.
#' @return \code{\link{logical}}. \code{TRUE}: success; \code{FALSE}: object
#'    was not a reactive one or failure to unset.
#' @example inst/examples/unsetReactive.r
#' @seealso \code{
#'    Generic: \link[reactr]{unsetReactive},
#'    \link[reactr]{unsetReactiveByUid}
#' }
#' @template author
#' @template references
#' @aliases unsetReactive-method_main 
#' @export
#' @aliases unsetReactive-character-environment-method
setMethod(
  f = "unsetReactive", 
  signature = signature(
    id = "character",
    where = "environment"
  ), 
  definition = function(
    id,
    where,
    ...
  ) {

  out <- FALSE
  if (!length(id)) {
    stop(paste0("Provide an ID"))
  } else {
    out <- unsetReactiveByUid(uid = getObjectUid(id = id, where = where)    )
  }
    
  return(out)
  
  }
)
