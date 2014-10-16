#' @title
#' Initialize Option Container (generic)
#'
#' @description 
#' Convenience function to initialize the option container that is then stored 
#' in option \code{"reactr"} and can be retrieved by 
#' \code{getOption("reactr")}.
#'   	
#' @param id \strong{Signature argument}.
#'    Object containing R option ID information. Typically, this corresponds
#'    to the name of the package/package project.
#' @param overwrite \code{\link{logical}}.
#'    \code{TRUE}: overwrite existing container;
#'    \code{FALSE}: create only if no container exists yet.
#' @template threedots
#' @example inst/examples/initializeOptionContainer.r
#' @seealso \code{
#'   	\link[reactr]{initializeOptionContainer-NULL-method},
#'     \link[reactr]{getRegistry}
#' }
#' @template author
#' @template references
setGeneric(
  name = "initializeOptionContainer",
  signature = c(
    "id"
  ),
  def = function(
    id,
    overwrite = FALSE,
    ...
  ) {
    standardGeneric("initializeOptionContainer")       
  }
)

#' @title
#' Initialize Option Container (missing)
#'
#' @description 
#' See generic: \code{\link[reactr]{initializeOptionContainer}}
#'      
#' @inheritParams initializeOptionContainer
#' @param id \code{\link{missing}}.
#' @return See method
#'    \code{\link[reactr]{initializeOptionContainer-NULL-method}}.
#' @example inst/examples/initializeOptionContainer.r
#' @seealso \code{
#'    \link[reactr]{initializeOptionContainer-NULL-method},
#'     \link[reactr]{getRegistry}
#' }
#' @template author
#' @template references
#' @export
#' @aliases initializeOptionContainer-missing-method
setMethod(
  f = "initializeOptionContainer", 
  signature = signature(
    id= "missing"
  ), 
  definition = function(
    id,
    overwrite,
    ...
  ) {
    
  return(initializeOptionContainer(
    id = id,
    overwrite = overwrite,
    ...
  ))    
  
  }
)

#' @title
#' Initialize Option Container (character)
#'
#' @description 
#' See generic: \code{\link[reactr]{initializeOptionContainer}}
#'      
#' @inheritParams initializeOptionContainer
#' @param id \code{\link{NULL}}.
#' @return \code{\link{environment}}. The option container.
#' @example inst/examples/initializeOptionContainer.r
#' @seealso \code{
#'    \link[reactr]{initializeOptionContainer},
#'     \link[reactr]{getRegistry}
#' }
#' @template author
#' @template references
#' @export
#' @aliases initializeOptionContainer-missing-method
setMethod(
  f = "initializeOptionContainer", 
  signature = signature(
    id= "character"
  ), 
  definition = function(
    id,
    overwrite,
    ...
  ) {
  
  out <- if (is.null(getOption(id)) || overwrite) {
    envir <- new.env()
    initializePackageOptions(where = envir)
    initializeRegistry(where = envir)
    eval(parse(text = sprintf("options(%s = envir)", id)))
    envir
  } else {
    getOption(id)    
  }
  out
    
  }
)

