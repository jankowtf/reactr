#' @title
#' Get Hash Registry
#'
#' @description 
#' Convenience function to retrieve the hash registry located in 
#' \code{getOption("reactr")$.hash}.
#'   	
#' @param where \strong{Signature argument}.
#'    Object containing hash registry location information.
#' @template threedot
#' @example inst/examples/getHashRegistry.r
#' @seealso \code{
#'   	\link[reactr]{getHashRegistry-environment-method},
#'     \link[reactr]{removeFromHashRegistry}
#' }
#' @template author
#' @template references
setGeneric(
  name = "getHashRegistry",
  signature = c(
    "where"
  ),
  def = function(
    where = getOption("reactr")$.hash,
    ...
  ) {
    standardGeneric("getHashRegistry")       
  }
)

#' @title
#' Get Hash Registry (missing-method)
#'
#' @description 
#' See generic: \code{\link[reactr]{getHashRegistry}}
#'      
#' @inheritParams getHashRegistry
#' @param where \code{\link{missing}}.
#' @return See method
#'    \code{\link[reactr]{getHashRegistry-environment-method}}.
#' @example inst/examples/getHashRegistry.r
#' @seealso \code{
#'    \link[reactr]{getHashRegistry-environment-method},
#'     \link[reactr]{removeFromHashRegistry}
#' }
#' @template author
#' @template references
#' @export
setMethod(
  f = "getHashRegistry", 
  signature = signature(
    where= "missing"
  ), 
  definition = function(
    where,
    ...
  ) {
    
  return(getHashRegistry(
    where = where,
    ...
  ))    
  
  }
)

#' @title
#' Get Hash Registry (environment-method)
#'
#' @description 
#' See generic: \code{\link[reactr]{getHashRegistry}}
#'      
#' @inheritParams getHashRegistry
#' @param where \code{\link{environment}}.
#' @return \code{\link{logical}}. \code{TRUE}: successful; \code{FALSE}: fail.
#' @example inst/examples/getHashRegistry.r
#' @seealso \code{
#'    \link[reactr]{getHashRegistry},
#'     \link[reactr]{removeFromHashRegistry}
#' }
#' @template author
#' @template references
#' @export
setMethod(
  f = "getHashRegistry", 
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


