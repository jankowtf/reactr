#' @title
#' Get From Registry (generic)
#'
#' @description 
#' Retrieves reactive object from registry 
#' (see \code{\link[reactr]{getRegistry}}).
#'   	
#' @param id \strong{Signature argument}.
#'    Object containing name/ID information.
#' @param where \strong{Signature argument}.
#'    Object containing location information.
#' @template threedots
#' @example inst/examples/getFromRegistry.r
#' @seealso \code{
#'   	\link[reactr]{getFromRegistry-character-environment-method}
#' }
#' @template author
#' @template references
setGeneric(
  name = "getFromRegistry",
  signature = c(
    "id",
    "where"
  ),
  def = function(
    id,
    where = parent.frame(),
    ...
  ) {
    standardGeneric("getFromRegistry")       
  }
)

#' @title
#' Get From Registry (character-missing)
#'
#' @description 
#' See generic: \code{\link[reactr]{getFromRegistry}}
#'      
#' @inheritParams getFromRegistry
#' @param id \code{\link{character}}.
#' @param where \code{\link{missing}}.
#' @return See method
#'    \code{\link[reactr]{getFromRegistry-character-environment-method}}
#' @example inst/examples/getFromRegistry.r
#' @seealso \code{
#'    \link[reactr]{getFromRegistry},
#'    \link[reactr]{getFromRegistry-character-environment-method}
#' }
#' @template author
#' @template references
#' @export
#' @aliases getFromRegistry-character-missing-method
setMethod(
  f = "getFromRegistry", 
  signature = signature(
    id = "character",
    where = "missing"
  ), 
  definition = function(
    id,
    where,
    ...
  ) {

  getFromRegistry(
    id = id,
    where = where,
    ...
  )
    
  }
)

#' @title
#' Get From Registry (character-environment)
#'
#' @description 
#' See generic: \code{\link[reactr]{getFromRegistry}}
#'   	 
#' @inheritParams getFromRegistry
#' @param id \code{\link{character}}.
#' @param where \code{\link{environment}}.
#' @return \code{\link{ReactiveObject.S3}}. Stored invisible instance of reactive
#'    object.
#' @example inst/examples/getFromRegistry.r
#' @seealso \code{
#'    \link[reactr]{getFromRegistry}
#' }
#' @template author
#' @template references
#' @export
#' @aliases getFromRegistry-character-environment-method
setMethod(
  f = "getFromRegistry", 
  signature = signature(
    id = "character",
    where = "environment"
  ), 
  definition = function(
    id,
    where,
    ...
  ) {

  out <- NULL
  if (length(id)) {
    uid <- computeObjectUid(id = id, where = where)
    out <- getFromRegistryByUid(uid = uid)
  }
  return(out)
    
  }
)
