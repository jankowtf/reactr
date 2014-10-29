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
#' See \code{\link[reactr]{rmReactive}} for this purpose.
#' 
#' @section Implications with respect objects depending on this object:
#' 
#' If other reactive objects have been relying on this reactive variable, 
#' from this point on they will simply return the last value
#' that has been cached. \strong{So there is no actual reactive binding anymore}.
#' 
#' @note
#' The main S4 method is 
#' \code{\link[reactr]{unsetReactiveByUid-character-method}}.
#'     
#' @param uid \strong{Signature argument}.
#'    Object containing UID information.
#' @template threedots
#' @example inst/examples/unsetReactiveByUid.r
#' @seealso \code{
#'   	\link[reactr]{unsetReactiveByUid-character-method},
#'     \link[reactr]{unsetReactive}
#' }
#' @template author
#' @template references
#' @export 
setGeneric(
  name = "unsetReactiveByUid",
  signature = c(
    "uid"
  ),
  def = function(
    uid,
    ...
  ) {
    standardGeneric("unsetReactiveByUid")       
  }
)

#' @title
#' Unset Reactive Object (character)
#'
#' @description 
#' See generic: \code{\link[reactr]{unsetReactiveByUid}}
#'      
#' @inheritParams unsetReactiveByUid
#' @param id \code{\link{character}}.
#' @return \code{\link{logical}}. 
#'    \code{TRUE}: successfully unset;
#'    \code{FALSE}: failed to unset.
#' @example inst/examples/unsetReactiveByUid.r
#' @seealso \code{
#'    Generic: \link[reactr]{unsetReactiveByUid},
#'     \link[reactr]{unsetReactive}
#' }
#' @template author
#' @template references
#' @aliases unsetReactiveByUid-method_main 
#' @export
#' @aliases unsetReactiveByUid-character-method
setMethod(
  f = "unsetReactiveByUid", 
  signature = signature(
    uid = "character"
  ), 
  definition = function(
    uid,
    ...
  ) {  
    
  reg <- getFromRegistryByUid(uid = uid)
  if (!is.null(reg)) {
    out <- reg$.unset()
  } else {
    out <- TRUE
  }
  
  }
)

