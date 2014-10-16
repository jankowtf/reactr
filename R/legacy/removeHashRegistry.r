#' @title
#' Remove Hash Registry
#'
#' @description 
#' Convenience function to remove the hash registry located in 
#' \code{getOption("reactr")$.registry}.
#'   	
#' @param where \strong{Signature argument}.
#'    Object containing hash registry location information.
#' @template threedots
#' @example inst/examples/removeHashRegistry.r
#' @seealso \code{
#'   	\link[reactr]{removeHashRegistry-missing-method},
#'     \link[reactr]{removeFromRegistryByUid}
#' }
#' @template author
#' @template references
setGeneric(
  name = "removeHashRegistry",
  signature = c(
    "where"
  ),
  def = function(
    where = NULL,
    ...
  ) {
    standardGeneric("removeHashRegistry")       
  }
)

#' @title
#' Remove Hash Registry (missing-method)
#'
#' @description 
#' See generic: \code{\link[reactr]{removeHashRegistry}}
#'      
#' @inheritParams removeHashRegistry
#' @param where \code{\link{missing}}.
#' @return \code{\link{logical}}. \code{TRUE}.
#' @example inst/examples/removeHashRegistry.r
#' @seealso \code{
#'    \link[reactr]{removeHashRegistry},
#'     \link[reactr]{getRegistry}
#' }
#' @template author
#' @template references
#' @export
setMethod(
  f = "removeHashRegistry", 
  signature = signature(
    where= "missing"
  ), 
  definition = function(
    where,
    ...
  ) {
    
  pkg_opts <- getOption("reactr")
  
  if (!is.null(pkg_opts$.registry)) {
    rm(".hash", envir = getOption("reactr"))
  }
  
  return(TRUE)
  
  }
)
