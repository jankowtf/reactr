#' @title
#' Unset Reactive Object
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
#' @template threedot
#' @example inst/examples/unsetReactiveByUid.r
#' @seealso \code{
#'   	\link[reactr]{unsetReactiveByUid-character-method}
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
#' Unset Reactive Object
#'
#' @description 
#' See generic: \code{\link[reactr]{unsetReactiveByUid}}
#'      
#' @inheritParams unsetReactiveByUid
#' @param id \code{\link{character}}.
#' @param where \code{\link{missing}}.
#'    Internal argument that should not be set explicitly.
#'    The value at runtime will correspond to the function that has been 
#'    provided via argument \code{binding}.
#' @return See method
#'    \code{\link[reactr]{unsetReactiveByUid-character-method}}
#' @example inst/examples/unsetReactiveByUid.r
#' @seealso \code{
#'    Generic: \link[reactr]{unsetReactiveByUid}
#' }
#' @template author
#' @template references
#' @aliases unsetReactiveByUid-method_main 
#' @export
setMethod(
  f = "unsetReactiveByUid", 
  signature = signature(
    uid = "character"
  ), 
  definition = function(
    uid,
    ...
  ) {
  
  ## Get actual location from hash registry //
  subenv <- getHashRegistry()[[uid]]
  if (is.null(sub)) {
    stop(paste0("No entry in registry hash for UID: ", uid))
  }
  id <- subenv$id
  if (is.null(id)) {
    stop(paste0("No ID stored in registry hash for UID: ", uid))
  }
  where <- subenv$where
  if (is.null(where)) {
    stop(paste0("No location stored in registry hash for UID: ", uid))
  }
    
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
      removeFromHashRegistryByUid(uid = uid)
    }
  }  
  
  }
)

