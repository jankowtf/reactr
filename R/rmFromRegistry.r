#' @title
#' Remove From Registry (generic)
#'
#' @description 
#' Removes entry of associated reactive object that has been unset via 
#' \code{\link[reactr]{unsetReactive}} from the registry.
#'   	
#' @param id \strong{Signature argument}.
#'    Object containing name/ID information.
#' @param where \strong{Signature argument}.
#'    Object containing location information.
#' @template threedots
#' @example inst/examples/rmFromRegistry.r
#' @seealso \code{
#'   	\link[reactr]{rmFromRegistry-character-environment-method}
#' }
#' @template author
#' @template references
setGeneric(
  name = "rmFromRegistry",
  signature = c(
    "id",
    "where"
  ),
  def = function(
    id,
    where = parent.frame(),
    ...
  ) {
    standardGeneric("rmFromRegistry")       
  }
)

#' @title
#' Remove From Registry (character-missing)
#'
#' @description 
#' See generic: \code{\link[reactr]{rmFromRegistry}}
#'      
#' @inheritParams rmFromRegistry
#' @param id \code{\link{character}}.
#' @param where \code{\link{missing}}.
#' @return See method
#'    \code{\link[reactr]{rmFromRegistry-character-environment-method}}.
#' @example inst/examples/rmFromRegistry.r
#' @seealso \code{
#'    \link[reactr]{rmFromRegistry},
#'    \link[reactr]{rmFromRegistry-character-environment-method}
#' }
#' @template author
#' @template references
#' @export
#' @aliases rmFromRegistry-character-missing-method
setMethod(
  f = "rmFromRegistry", 
  signature = signature(
    id = "character",
    where = "missing"
  ), 
  definition = function(
    id,
    where,
    ...
  ) {

  rmFromRegistry(
    id = id,
    where = where,
    ...
  )
    
  }
)

#' @title
#' Remove From Registry (character-environment)
#'
#' @description 
#' See generic: \code{\link[reactr]{rmFromRegistry}}
#'   	 
#' @inheritParams rmFromRegistry
#' @param id \code{\link{character}}.
#' @param where \code{\link{environment}}.
#' @return \code{\link{logical}}. \code{TRUE}: successfully removed; 
#'    \code{FALSE}: not removed because there was nothing to remove.
#' @example inst/examples/rmFromRegistry.r
#' @seealso \code{
#'    \link[reactr]{rmFromRegistry}
#' }
#' @template author
#' @template references
#' @export
#' @aliases rmFromRegistry-character-environment-method
setMethod(
  f = "rmFromRegistry", 
  signature = signature(
    id = "character",
    where = "environment"
  ), 
  definition = function(
    id,
    where,
    ...
  ) {

  out <- FALSE
  if (length(id)) {
    uid <- computeObjectUid(id = id, where = where)
    out <- rmFromRegistryByUid(uid = uid)
  }
  return(out)
    
  }
)
