#' @title
#' Remove Reactive Object (generic)
#'
#' @description 
#' Removes a reactive object from its environment. 
#' 
#' @details
#' \strong{Note that tis is different from unsetting a reactive object 
#' via \code{\link[reactr]{removeReactiveByUid}}}. It is equivalent to 
#' \code{\link[base]{rm}} with a previous call to 
#' \code{\link[reactr]{unsetReactiveByUid}}. 
#' 
#' @section Implications with respect to observing variables:
#' If other reactive variables have been observing the reactive variable that
#' has been removed, from this point on they will simply return the last value
#' that has been cached if \code{strict = FALSE} or \code{NULL} if 
#' \code{strict = TRUE} when the observing object was set via 
#' \code{\link[reactr]{setReactiveS3}}
#' 
#' @note
#' The main S4 method is 
#' \code{\link[reactr]{removeReactiveByUid-character-method}}.
#'     
#' @param uid \strong{Signature argument}.
#'    Object containing UID information.
#' @template threedots
#' @example inst/examples/removeReactiveByUid.r
#' @seealso \code{
#'   	\link[reactr]{removeReactiveByUid-character-method}
#' }
#' @template author
#' @template references
#' @export 
setGeneric(
  name = "removeReactiveByUid",
  signature = c(
    "uid"
  ),
  def = function(
    uid,
    ...
  ) {
    standardGeneric("removeReactiveByUid")       
  }
)

#' @title
#' Remove Reactive Object (character)
#'
#' @description 
#' See generic: \code{\link[reactr]{removeReactiveByUid}}
#'      
#' @inheritParams removeReactiveByUid
#' @param uid \code{\link{character}}.
#' @return See method
#'    \code{\link[reactr]{removeReactiveByUid-character-method}}
#' @example inst/examples/removeReactiveByUid.r
#' @seealso \code{
#'    Generic: \link[reactr]{removeReactiveByUid}
#' }
#' @template author
#' @template references
#' @aliases removeReactiveByUid-method_main 
#' @export
#' @aliases removeReactiveByUid-character-method
setMethod(
  f = "removeReactiveByUid", 
  signature = signature(
    uid = "character"
  ), 
  definition = function(
    uid,
    ...
  ) {
    
  ## Get actual location from registry //
  subenv <- getRegistry()[[uid]]
  if (is.null(subenv)) {
    stop(paste0("No entry in registry for UID: ", uid))
  }
  id <- subenv$.id
  if (is.null(id)) {
    stop(paste0("No ID stored in registry for UID: ", uid))
  }
  where <- subenv$.where
  if (is.null(where)) {
    stop(paste0("No location stored in registry for UID: ", uid))
  }  
  
  out <- FALSE
  if (exists(id, envir = where, inherits = FALSE)) {
    has_binding <- try(bindingIsActive(id, where), silent = TRUE)
    if (inherits(has_binding, "try-error")) {
      has_binding <- FALSE
    } 
    if (has_binding) {
      try(unsetReactiveByUid(uid = uid))
      rm(list = id, envir = where, inherits = FALSE)
      out <- TRUE
    }
  }
  return(out)
  
  }
)
