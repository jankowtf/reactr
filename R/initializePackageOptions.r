#' @title
#' Initialize Package Options (generic)
#'
#' @description 
#' Convenience function to initialize a package options environment below an 
#' option container.
#'   	
#' @param id \strong{Signature argument}.
#'    Object containing ID information.
#' @param where \strong{Signature argument}.
#'    Object containing location information.
#' @template threedots
#' @example inst/examples/initializePackageOptions.r
#' @seealso \code{
#'   	\link[reactr]{initializePackageOptions-NULL-method},
#'     \link[reactr]{getRegistry}
#' }
#' @template author
#' @template references
setGeneric(
  name = "initializePackageOptions",
  signature = c(
    "id",
    "where"
  ),
  def = function(
    id = "options",
    where,
    ...
  ) {
    standardGeneric("initializePackageOptions")       
  }
)

#' @title
#' Initialize Package Options (missing-missing)
#'
#' @description 
#' See generic: \code{\link[reactr]{initializePackageOptions}}
#'      
#' @inheritParams initializePackageOptions
#' @param id \code{\link{missing}}.
#' @param where \code{\link{missing}}.
#' @return See method
#'    \code{\link[reactr]{initializePackageOptions-NULL-method}}.
#' @example inst/examples/initializePackageOptions.r
#' @seealso \code{
#'    \link[reactr]{initializePackageOptions-NULL-method},
#'     \link[reactr]{getRegistry}
#' }
#' @template author
#' @template references
#' @export
#' @aliases initializePackageOptions-missing-missing-method
setMethod(
  f = "initializePackageOptions", 
  signature = signature(
    id = "missing",
    where= "missing"
  ), 
  definition = function(
    id,
    where,
    ...
  ) {
    
  return(initializePackageOptions(
    id = id,
    where = where,
    ...
  ))    
  
  }
)

#' @title
#' Initialize Package Options (missing-environment)
#'
#' @description 
#' See generic: \code{\link[reactr]{initializePackageOptions}}
#'      
#' @inheritParams initializePackageOptions
#' @param id \code{\link{missing}}.
#' @param where \code{\link{environment}}.
#' @return See method
#'    \code{\link[reactr]{initializePackageOptions-NULL-method}}.
#' @example inst/examples/initializePackageOptions.r
#' @seealso \code{
#'    \link[reactr]{initializePackageOptions-NULL-method},
#'     \link[reactr]{getRegistry}
#' }
#' @template author
#' @template references
#' @export
#' @aliases initializePackageOptions-missing-environment-method
setMethod(
  f = "initializePackageOptions", 
  signature = signature(
    id = "missing",
    where= "environment"
  ), 
  definition = function(
    id,
    where,
    ...
  ) {
    
  return(initializePackageOptions(
    id = id,
    where = where,
    ...
  ))    
  
  }
)

#' @title
#' Initialize Package Options (character-environment)
#'
#' @description 
#' See generic: \code{\link[reactr]{initializePackageOptions}}
#'      
#' @inheritParams initializePackageOptions
#' @param id \code{\link{character}}.
#' @param where \code{\link{environment}}.
#' @return \code{\link{environment}}. Environment containing the options.
#' @example inst/examples/initializePackageOptions.r
#' @seealso \code{
#'    \link[reactr]{initializePackageOptions},
#'     \link[reactr]{getRegistry}
#' }
#' @template author
#' @template references
#' @export
#' @aliases initializePackageOptions-character-environment-method
setMethod(
  f = "initializePackageOptions", 
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

