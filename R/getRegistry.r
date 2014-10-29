#' @title
#' Get Registry (generic)
#'
#' @description 
#' Convenience function to retrieve the registry located in 
#' \code{getOption("reactr")$.registry}.
#'   	
#' @param where \strong{Signature argument}.
#'    Object containing registry location information.
#' @template threedots
#' @example inst/examples/getRegistry.r
#' @seealso \code{
#'   	\link[reactr]{getRegistry-environment-method},
#'     \link[reactr]{rmFromRegistryByUid}
#' }
#' @template author
#' @template references
setGeneric(
  name = "getRegistry",
  signature = c(
    "where"
  ),
  def = function(
    where = NULL,
    ...
  ) {
    standardGeneric("getRegistry")       
  }
)

#' @title
#' Get Registry (missing)
#'
#' @description 
#' See generic: \code{\link[reactr]{getRegistry}}
#'      
#' @inheritParams getRegistry
#' @param where \code{\link{missing}}.
#' @return See method
#'    \code{\link[reactr]{getRegistry-environment-method}}.
#' @example inst/examples/getRegistry.r
#' @seealso \code{
#'    \link[reactr]{getRegistry-environment-method},
#'     \link[reactr]{rmFromRegistryByUid}
#' }
#' @template author
#' @template references
#' @export
#' @aliases getRegistry-missing-method
setMethod(
  f = "getRegistry", 
  signature = signature(
    where = "missing"
  ), 
  definition = function(
    where,
    ...
  ) {
    
  id <- "reactr"    
  if (is.null(getOption(id))) {
    initializeOptionContainer(id = id)
  }
  where <- getOption(id)$.registry
    
  return(getRegistry(
    where = where,
    ...
  ))    
  
  }
)

#' @title
#' Get Registry (environment)
#'
#' @description 
#' See generic: \code{\link[reactr]{getRegistry}}
#'      
#' @inheritParams getRegistry
#' @param where \code{\link{environment}}.
#' @return \code{\link{environment}}. Registry object.
#' @example inst/examples/getRegistry.r
#' @seealso \code{
#'    \link[reactr]{getRegistry},
#'     \link[reactr]{rmFromRegistryByUid}
#' }
#' @template author
#' @template references
#' @export
#' @aliases getRegistry-environment-method
setMethod(
  f = "getRegistry", 
  signature = signature(
    where= "environment"
  ), 
  definition = function(
    where,
    ...
  ) {
  
  return(where)
    
  }
)


