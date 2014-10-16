#' @title
#' Reset Registry (generic)
#'
#' @description 
#' Resets the registry located in \code{getOption("reactr")$.registry}.
#'   	
#' @param where \strong{Signature argument}.
#'    Object containing registry location information.
#' @template threedots
#' @example inst/examples/resetRegistry.r
#' @seealso \code{
#'   	\link[reactr]{resetRegistry-environment-method},
#'     \link[reactr]{removeFromRegistryByUid}
#' }
#' @template author
#' @template references
setGeneric(
  name = "resetRegistry",
  signature = c(
    "where"
  ),
  def = function(
    where = getRegistry(),
    ...
  ) {
    standardGeneric("resetRegistry")       
  }
)

#' @title
#' Reset Registry (missing)
#'
#' @description 
#' See generic: \code{\link[reactr]{resetRegistry}}
#'      
#' @inheritParams resetRegistry
#' @param where \code{\link{missing}}.
#' @return See method
#'    \code{\link[reactr]{resetRegistry-environment-method}}.
#' @example inst/examples/resetRegistry.r
#' @seealso \code{
#'    \link[reactr]{resetRegistry-environment-method},
#'     \link[reactr]{removeFromRegistryByUid}
#' }
#' @template author
#' @template references
#' @export
#' @aliases resetRegistry-missing-method
setMethod(
  f = "resetRegistry", 
  signature = signature(
    where= "missing"
  ), 
  definition = function(
    where,
    ...
  ) {
    
  return(resetRegistry(
    where = where,
    ...
  ))    
  
  }
)

#' @title
#' Reset Registry (environment)
#'
#' @description 
#' See generic: \code{\link[reactr]{resetRegistry}}
#'      
#' @inheritParams resetRegistry
#' @param where \code{\link{environment}}.
#' @return \code{\link{logical}}. \code{TRUE}: successful; \code{FALSE}: fail.
#' @example inst/examples/resetRegistry.r
#' @seealso \code{
#'    \link[reactr]{resetRegistry},
#'     \link[reactr]{removeFromRegistryByUid}
#' }
#' @template author
#' @template references
#' @export
#' @aliases resetRegistry-environment-method
setMethod(
  f = "resetRegistry", 
  signature = signature(
    where= "environment"
  ), 
  definition = function(
    where,
    ...
  ) {
    
  tmp <- rm(list = ls(where, all.names = TRUE), envir = where)
  return(!length(tmp))
    
  }
)


