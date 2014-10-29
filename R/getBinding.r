#' @title
#' Get Binding Function (generic)
#'
#' @description 
#' Retrieves the binding function by which a reactive object is bound to 
#' other referenced reactive objects.
#' 
#' @details
#' This is a convenience function to retrieve the value of field \code{.func}
#' from the invisible objects stored in the registry.
#'   	
#' @param id \strong{Signature argument}.
#'    Object containing name/ID information.
#' @param where \strong{Signature argument}.
#'    Object containing location information.
#' @template threedots
#' @example inst/examples/getBinding.r
#' @seealso \code{
#'   	\link[reactr]{getBinding-character-environment-method}
#' }
#' @template author
#' @template references
setGeneric(
  name = "getBinding",
  signature = c(
    "id",
    "where"
  ),
  def = function(
    id,
    where = parent.frame(),
    ...
  ) {
    standardGeneric("getBinding")       
  }
)

#' @title
#' Get Binding Function (character-missing)
#'
#' @description 
#' See generic: \code{\link[reactr]{getBinding}}
#'      
#' @inheritParams getBinding
#' @param id \code{\link{character}}.
#' @param where \code{\link{missing}}.
#' @return See method
#'    \code{\link[reactr]{getBinding-character-environment-method}}
#' @example inst/examples/getBinding.r
#' @seealso \code{
#'    \link[reactr]{getBinding},
#'    \link[reactr]{getBinding-character-environment-method}
#' }
#' @template author
#' @template references
#' @export
#' @aliases getBinding-character-missing-method
setMethod(
  f = "getBinding", 
  signature = signature(
    id = "character",
    where = "missing"
  ), 
  definition = function(
    id,
    where,
    ...
  ) {

  getBinding(
    id = id,
    where = where,
    ...
  )
    
  }
)

#' @title
#' Get Binding Function (character-environment)
#'
#' @description 
#' See generic: \code{\link[reactr]{getBinding}}
#'   	 
#' @inheritParams getBinding
#' @param id \code{\link{character}}.
#' @param where \code{\link{environment}}.
#' @return \code{\link{function}}. Binding function.
#' @example inst/examples/getBinding.r
#' @seealso \code{
#'    \link[reactr]{getBinding}
#' }
#' @template author
#' @template references
#' @aliases getBinding-character-environment-method
#' @export
setMethod(
  f = "getBinding", 
  signature = signature(
    id = "character",
    where = "environment"
  ), 
  definition = function(
    id,
    where,
    ...
  ) {

  getFromRegistry(id = id, where = where)$.func
    
  }
)
