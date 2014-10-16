#' @title
#' Initialize Registry (generic)
#'
#' @description 
#' Convenience function to initialize a registry environment below an 
#' option container.
#'   	
#' @param id \strong{Signature argument}.
#'    Object containing ID information.
#' @param where \strong{Signature argument}.
#'    Object containing location information.
#' @template threedots
#' @example inst/examples/initializeRegistry.r
#' @seealso \code{
#'   	\link[reactr]{initializeRegistry-NULL-method},
#'     \link[reactr]{getRegistry}
#' }
#' @template author
#' @template references
setGeneric(
  name = "initializeRegistry",
  signature = c(
    "id",
    "where"
  ),
  def = function(
    id = ".registry",
    where,
    ...
  ) {
    standardGeneric("initializeRegistry")       
  }
)

#' @title
#' Initialize Registry (missing-missing)
#'
#' @description 
#' See generic: \code{\link[reactr]{initializeRegistry}}
#'      
#' @inheritParams initializeRegistry
#' @param id \code{\link{missing}}.
#' @param where \code{\link{missing}}.
#' @return See method
#'    \code{\link[reactr]{initializeRegistry-NULL-method}}.
#' @example inst/examples/initializeRegistry.r
#' @seealso \code{
#'    \link[reactr]{initializeRegistry-NULL-method},
#'     \link[reactr]{getRegistry}
#' }
#' @template author
#' @template references
#' @export
#' @aliases initializeRegistry-missing-missing-method
setMethod(
  f = "initializeRegistry", 
  signature = signature(
    id = "missing",
    where= "missing"
  ), 
  definition = function(
    id,
    where,
    ...
  ) {
    
  return(initializeRegistry(
    id = id,
    where = where,
    ...
  ))    
  
  }
)

#' @title
#' Initialize Registry (missing-environment)
#'
#' @description 
#' See generic: \code{\link[reactr]{initializeRegistry}}
#'      
#' @inheritParams initializeRegistry
#' @param id \code{\link{missing}}.
#' @param where \code{\link{environment}}.
#' @return See method
#'    \code{\link[reactr]{initializeRegistry-NULL-method}}.
#' @example inst/examples/initializeRegistry.r
#' @seealso \code{
#'    \link[reactr]{initializeRegistry-NULL-method},
#'     \link[reactr]{getRegistry}
#' }
#' @template author
#' @template references
#' @export
#' @aliases initializeRegistry-missing-environment-method
setMethod(
  f = "initializeRegistry", 
  signature = signature(
    id = "missing",
    where= "environment"
  ), 
  definition = function(
    id,
    where,
    ...
  ) {
    
  return(initializeRegistry(
    id = id,
    where = where,
    ...
  ))    
  
  }
)

#' @title
#' Initialize Registry (character-environment)
#'
#' @description 
#' See generic: \code{\link[reactr]{initializeRegistry}}
#'      
#' @inheritParams initializeRegistry
#' @param id \code{\link{character}}.
#' @param where \code{\link{environment}}.
#' @return \code{\link{environment}}. Initialized registry.
#' @example inst/examples/initializeRegistry.r
#' @seealso \code{
#'    \link[reactr]{initializeRegistry},
#'     \link[reactr]{getRegistry}
#' }
#' @template author
#' @template references
#' @export
#' @aliases initializeRegistry-character-environment-method
setMethod(
  f = "initializeRegistry", 
  signature = signature(
    id = "character",
    where= "environment"
  ), 
  definition = function(
    id,
    where,
    ...
  ) {
  
  assign(id, new.env(parent = emptyenv()), envir = where)
    
  }
)

