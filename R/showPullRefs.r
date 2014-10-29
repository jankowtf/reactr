#' @title
#' Show Pull References (generic)
#'
#' @description 
#' Convenience function to show the UIDs of registered Pull references.
#' 
#' @section Main S4 method:
#' 
#' \code{\link[reactr]{showPullRefs-character-environment-method}}
#'   	
#' @param id \strong{Signature argument}.
#'    Object containing name/ID information.
#' @param where \strong{Signature argument}.
#'    Object containing location information.
#' @template threedots
#' @example inst/examples/showPullRefs.r
#' @seealso \code{
#'   	\link[reactr]{showPullRefs-environment-method}
#' }
#' @template author
#' @template references
#' @export
setGeneric(
  name = "showPullRefs",
  signature = c(
    "id",
    "where"
  ),
  def = function(
    id,
    where = parent.frame(),
    ...
  ) {
    standardGeneric("showPullRefs")       
  }
)

#' @title
#' Show Pull References (missing)
#'
#' @description 
#' See generic: \code{\link[reactr]{showPullRefs}}
#'      
#' @inheritParams showPullRefs
#' @param id \code{\link{character}}.
#' @param where \code{\link{missing}}.
#' @return See method
#'    \code{\link[reactr]{showPullRefs-character-environment-method}}.
#' @example inst/examples/showPullRefs.r
#' @seealso \code{
#'    \link[reactr]{showPullRefs-environment-method}
#' }
#' @template author
#' @template references
#' @aliases showPullRefs-missing-method
#' @export
setMethod(
  f = "showPullRefs", 
  signature = signature(
    id = "character",
    where = "missing"
  ), 
  definition = function(
    id,
    where,
    ...
  ) {
  
  showPullRefs(
    id = id,
    where = where,
    ...
  )
  
  }
)

#' @title
#' Show Pull References (environment)
#'
#' @description 
#' See generic: \code{\link[reactr]{showPullRefs}}
#'      
#' @inheritParams showPullRefs
#' @param id \code{\link{character}}.
#' @param where \code{\link{environment}}.
#' @return \code{\link{logical}}. \code{TRUE}: successful; \code{FALSE}: fail.
#' @example inst/examples/showPullRefs.r
#' @seealso \code{
#'    \link[reactr]{showPullRefs},
#'     \link[reactr]{getRegistry}
#' }
#' @template author
#' @template references
#' @export
#' @aliases showPullRefs-environment-method
setMethod(
  f = "showPullRefs", 
  signature = signature(
    id = "character",
    where = "environment"
  ), 
  definition = function(
    id,
    where,
    ...
  ) {
  
  ls(getFromRegistry(id = id, where = where)$.refs_pull)
    
  }
)


