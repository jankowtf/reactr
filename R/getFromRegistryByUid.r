#' @title
#' Get From Registry By UID (generic)
#'
#' @description 
#' Retrieves reactive object from the registry
#' (see \code{\link[reactr]{getRegistry}}).
#'   	
#' @param uid \strong{Signature argument}.
#'    Object containing UID information.
#' @template threedots
#' @example inst/examples/getFromRegistryByUid.r
#' @seealso \code{
#'   	\link[reactr]{getFromRegistryByUid-character-environment-method}
#' }
#' @template author
#' @template references
setGeneric(
  name = "getFromRegistryByUid",
  signature = c(
    "uid"
  ),
  def = function(
    uid,
    ...
  ) {
    standardGeneric("getFromRegistryByUid")       
  }
)

#' @title
#' Get From Registry By UID (character)
#'
#' @description 
#' See generic: \code{\link[reactr]{getFromRegistryByUid}}
#'      
#' @inheritParams getFromRegistryByUid
#' @param uid \code{\link{character}}.
#' @param where \code{\link{environment}}.
#' @return \code{\link{ReactiveObject.S3}}. Stored hidden instance of reactive
#'    object.
#' @example inst/examples/getFromRegistryByUid.r
#' @seealso \code{
#'    \link[reactr]{getFromRegistryByUid}
#' }
#' @template author
#' @template references
#' @export
#' @aliases getFromRegistry-character-method
setMethod(
  f = "getFromRegistryByUid", 
  signature = signature(
    uid = "character"
  ), 
  definition = function(
    uid,
    ...
  ) {
    
  getRegistry()[[uid]]
    
  }
)


