#' @title
#' Inititalize Hash Registry
#'
#' @description 
#' Convenience function to initialize the hash registry located in 
#' \code{getOption("reactr")$.hash} in case it has not been properly 
#' initialized at package startup.
#'   	
#' @param where \strong{Signature argument}.
#'    Object containing hash registry location information.
#' @template threedot
#' @example inst/examples/initializeHashRegistry.r
#' @seealso \code{
#'   	\link[reactr]{initializeHashRegistry-NULL-method},
#'     \link[reactr]{getHashRegistry}
#' }
#' @template author
#' @template references
setGeneric(
  name = "initializeHashRegistry",
  signature = c(
    "where"
  ),
  def = function(
    where = NULL,
    ...
  ) {
    standardGeneric("initializeHashRegistry")       
  }
)

#' @title
#' Inititalize Hash Registry (missing-method)
#'
#' @description 
#' See generic: \code{\link[reactr]{initializeHashRegistry}}
#'      
#' @inheritParams initializeHashRegistry
#' @param where \code{\link{missing}}.
#' @return See method
#'    \code{\link[reactr]{initializeHashRegistry-NULL-method}}.
#' @example inst/examples/initializeHashRegistry.r
#' @seealso \code{
#'    \link[reactr]{initializeHashRegistry-NULL-method},
#'     \link[reactr]{getHashRegistry}
#' }
#' @template author
#' @template references
#' @export
setMethod(
  f = "initializeHashRegistry", 
  signature = signature(
    where= "missing"
  ), 
  definition = function(
    where,
    ...
  ) {
    
  return(initializeHashRegistry(
    where = where,
    ...
  ))    
  
  }
)

#' @title
#' Inititalize Hash Registry (NULL-method)
#'
#' @description 
#' See generic: \code{\link[reactr]{initializeHashRegistry}}
#'      
#' @inheritParams initializeHashRegistry
#' @param where \code{\link{NULL}}.
#' @return \code{\link{logical}}. \code{TRUE}.
#' @example inst/examples/initializeHashRegistry.r
#' @seealso \code{
#'    \link[reactr]{initializeHashRegistry},
#'     \link[reactr]{getHashRegistry}
#' }
#' @template author
#' @template references
#' @export
setMethod(
  f = "initializeHashRegistry", 
  signature = signature(
    where= "NULL"
  ), 
  definition = function(
    where,
    ...
  ) {
  
  envir <- new.env()
  envir$.hash <- new.env()
  options("reactr" = envir)    
  
  return(TRUE)
    
  }
)

