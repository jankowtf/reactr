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
#' @example inst/examples/showRegistryContent.r
#' @seealso \code{
#'   	\link[reactr]{showRegistryContent-environment-method},
#'     \link[reactr]{getRegistry}
#' }
#' @template author
#' @template references
setGeneric(
  name = "showRegistryContent",
  signature = c(
    "where"
  ),
  def = function(
    where = NULL,
    ...
  ) {
    standardGeneric("showRegistryContent")       
  }
)

#' @title
#' Show Registry Content (missing)
#'
#' @description 
#' See generic: \code{\link[reactr]{showRegistryContent}}
#'      
#' @inheritParams showRegistryContent
#' @param where \code{\link{missing}}.
#' @return See method
#'    \code{\link[reactr]{showRegistryContent-environment-method}}.
#' @example inst/examples/showRegistryContent.r
#' @seealso \code{
#'    \link[reactr]{showRegistryContent-environment-method},
#'     \link[reactr]{getRegistry}
#' }
#' @template author
#' @template references
#' @export
#' @aliases showRegistryContent-missing-method
setMethod(
  f = "showRegistryContent", 
  signature = signature(
    where= "missing"
  ), 
  definition = function(
    where,
    ...
  ) {
  
  return(showRegistryContent(
    where = getRegistry(),
    ...
  ))    
  
  }
)

#' @title
#' Get Hash Registry (environment)
#'
#' @description 
#' See generic: \code{\link[reactr]{showRegistryContent}}
#'      
#' @inheritParams showRegistryContent
#' @param where \code{\link{environment}}.
#' @return \code{\link{logical}}. \code{TRUE}: successful; \code{FALSE}: fail.
#' @example inst/examples/showRegistryContent.r
#' @seealso \code{
#'    \link[reactr]{showRegistryContent},
#'     \link[reactr]{getRegistry}
#' }
#' @template author
#' @template references
#' @export
#' @aliases showRegistryContent-environment-method
setMethod(
  f = "showRegistryContent", 
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


