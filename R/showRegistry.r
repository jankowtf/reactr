#' @title
#' Show Registry Content (generic)
#'
#' @description 
#' Convenience function to show the content of the registry in
#' \code{getOption("reactr")$.registry}.
#'   	
#' @param where \strong{Signature argument}.
#'    Object containing registry location information.
#' @template threedots
#' @example inst/examples/showRegistry.r
#' @seealso \code{
#'   	\link[reactr]{showRegistry-environment-method},
#'     \link[reactr]{getRegistry}
#' }
#' @template author
#' @template references
setGeneric(
  name = "showRegistry",
  signature = c(
    "where"
  ),
  def = function(
    where = NULL,
    ...
  ) {
    standardGeneric("showRegistry")       
  }
)

#' @title
#' Show Registry Content (missing)
#'
#' @description 
#' See generic: \code{\link[reactr]{showRegistry}}
#'      
#' @inheritParams showRegistry
#' @param where \code{\link{missing}}.
#' @return See method
#'    \code{\link[reactr]{showRegistry-environment-method}}.
#' @example inst/examples/showRegistry.r
#' @seealso \code{
#'    \link[reactr]{showRegistry-environment-method},
#'     \link[reactr]{getRegistry}
#' }
#' @template author
#' @template references
#' @export
#' @aliases showRegistry-missing-method
setMethod(
  f = "showRegistry", 
  signature = signature(
    where= "missing"
  ), 
  definition = function(
    where,
    ...
  ) {
  
  return(showRegistry(
    where = getRegistry(),
    ...
  ))    
  
  }
)

#' @title
#' Get Hash Registry (environment)
#'
#' @description 
#' See generic: \code{\link[reactr]{showRegistry}}
#'      
#' @inheritParams showRegistry
#' @param where \code{\link{environment}}.
#' @return \code{\link{logical}}. \code{TRUE}: successful; \code{FALSE}: fail.
#' @example inst/examples/showRegistry.r
#' @seealso \code{
#'    \link[reactr]{showRegistry},
#'     \link[reactr]{getRegistry}
#' }
#' @template author
#' @template references
#' @export
#' @aliases showRegistry-environment-method
setMethod(
  f = "showRegistry", 
  signature = signature(
    where= "environment"
  ), 
  definition = function(
    where,
    ...
  ) {
  
  return(ls(where))
    
  }
)


