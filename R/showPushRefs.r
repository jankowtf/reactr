#' @title
#' Show Push References (generic)
#'
#' @description 
#' Convenience function to show the UIDs of registered push references.
#' 
#' @section Main S4 method:
#' 
#' \code{\link[reactr]{showPushRefs-character-environment-method}}
#'   	
#' @param id \strong{Signature argument}.
#'    Object containing name/ID information.
#' @param where \strong{Signature argument}.
#'    Object containing location information.
#' @template threedots
#' @example inst/examples/showPushRefs.r
#' @seealso \code{
#'   	\link[reactr]{showPushRefs-environment-method}
#' }
#' @template author
#' @template references
#' @export
setGeneric(
  name = "showPushRefs",
  signature = c(
    "id",
    "where"
  ),
  def = function(
    id,
    where = parent.frame(),
    ...
  ) {
    standardGeneric("showPushRefs")       
  }
)

#' @title
#' Show Push References (missing)
#'
#' @description 
#' See generic: \code{\link[reactr]{showPushRefs}}
#'      
#' @inheritParams showPushRefs
#' @param id \code{\link{character}}.
#' @param where \code{\link{missing}}.
#' @return See method
#'    \code{\link[reactr]{showPushRefs-character-environment-method}}.
#' @example inst/examples/showPushRefs.r
#' @seealso \code{
#'    \link[reactr]{showPushRefs-environment-method}
#' }
#' @template author
#' @template references
#' @aliases showPushRefs-missing-method
#' @export
setMethod(
  f = "showPushRefs", 
  signature = signature(
    id = "character",
    where = "missing"
  ), 
  definition = function(
    id,
    where,
    ...
  ) {
  
  showPushRefs(
    id = id,
    where = where,
    ...
  )
  
  }
)

#' @title
#' Show Push References (environment)
#'
#' @description 
#' See generic: \code{\link[reactr]{showPushRefs}}
#'      
#' @inheritParams showPushRefs
#' @param id \code{\link{character}}.
#' @param where \code{\link{environment}}.
#' @return \code{\link{logical}}. \code{TRUE}: successful; \code{FALSE}: fail.
#' @example inst/examples/showPushRefs.r
#' @seealso \code{
#'    \link[reactr]{showPushRefs},
#'     \link[reactr]{getRegistry}
#' }
#' @template author
#' @template references
#' @export
#' @aliases showPushRefs-environment-method
setMethod(
  f = "showPushRefs", 
  signature = signature(
    id = "character",
    where = "environment"
  ), 
  definition = function(
    id,
    where,
    ...
  ) {
  
  ls(getFromRegistry(id = id, where = where)$.refs_push)
    
  }
)


