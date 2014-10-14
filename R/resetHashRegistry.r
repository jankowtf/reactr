#' @title
#' Reset Hash Registry
#'
#' @description 
#' Resets the hash registry located in \code{getOption("reactr")$.hash}.
#'   	
#' @param where \strong{Signature argument}.
#'    Object containing hash registry location information.
#' @template threedot
#' @example inst/examples/resetHashRegistry.r
#' @seealso \code{
#'   	\link[reactr]{resetHashRegistry-environment-method},
#'     \link[reactr]{removeFromHashRegistryByUid}
#' }
#' @template author
#' @template references
setGeneric(
  name = "resetHashRegistry",
  signature = c(
    "where"
  ),
  def = function(
    where = getHashRegistry(),
    ...
  ) {
    standardGeneric("resetHashRegistry")       
  }
)

#' @title
#' Reset Hash Registry
#'
#' @description 
#' See generic: \code{\link[reactr]{resetHashRegistry}}
#'      
#' @inheritParams resetHashRegistry
#' @param where \code{\link{missing}}.
#' @return See method
#'    \code{\link[reactr]{resetHashRegistry-environment-method}}.
#' @example inst/examples/resetHashRegistry.r
#' @seealso \code{
#'    \link[reactr]{resetHashRegistry-environment-method},
#'     \link[reactr]{removeFromHashRegistryByUid}
#' }
#' @template author
#' @template references
#' @export
setMethod(
  f = "resetHashRegistry", 
  signature = signature(
    where= "missing"
  ), 
  definition = function(
    where,
    ...
  ) {
    
  return(resetHashRegistry(
    where = where,
    ...
  ))    
  
  }
)

#' @title
#' Reset Hash Registry
#'
#' @description 
#' See generic: \code{\link[reactr]{resetHashRegistry}}
#'      
#' @inheritParams resetHashRegistry
#' @param where \code{\link{environment}}.
#' @return \code{\link{logical}}. \code{TRUE}: successful; \code{FALSE}: fail.
#' @example inst/examples/resetHashRegistry.r
#' @seealso \code{
#'    \link[reactr]{resetHashRegistry},
#'     \link[reactr]{removeFromHashRegistryByUid}
#' }
#' @template author
#' @template references
#' @export
setMethod(
  f = "resetHashRegistry", 
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


