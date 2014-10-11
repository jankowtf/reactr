#' @title
#' Remove From Hash Registry (generic)
#'
#' @description 
#' Removes entry of associated reactive object that has been unset via 
#' \code{\link[reactr]{unsetReactive}} from the hash registry.
#'   	
#' @param id \strong{Signature argument}.
#'    Object containing name/ID information.
#' @param where \strong{Signature argument}.
#'    Object containing location information.
#' @template threedot
#' @example inst/examples/removeFromHashRegistry.r
#' @seealso \code{
#'   	\link[reactr]{removeFromHashRegistry-character-environment-method}
#' }
#' @template author
#' @template references
setGeneric(
  name = "removeFromHashRegistry",
  signature = c(
    "id",
    "where"
  ),
  def = function(
    id,
    where,
    ...
  ) {
    standardGeneric("removeFromHashRegistry")       
  }
)

#' @title
#' Remove From Hash Registry (char-env-method)
#'
#' @description 
#' See generic: \code{\link[reactr]{removeFromHashRegistry}}
#'   	 
#' @inheritParams removeFromHashRegistry
#' @param id \code{\link{character}}.
#' @param where \code{\link{environment}}.
#' @return \code{\link{logical}}. \code{TRUE}: successfully removed; 
#'    \code{FALSE}: not removed because there was nothing to remove.
#' @example inst/examples/removeFromHashRegistry.r
#' @seealso \code{
#'    \link[reactr]{removeFromHashRegistry}
#' }
#' @template author
#' @template references
#' @export
setMethod(
  f = "removeFromHashRegistry", 
  signature = signature(
    id = "character",
    where = "environment"
  ), 
  definition = function(
    id,
    where,
    ...
  ) {

  hash_env <- getHashRegistry()
  out <- FALSE
  if (length(id)) {
    uid <- getReactiveUid(id = id, where = where)
    out <- removeFromHashRegistryByUid(uid = uid)
  }
  return(out)
    
  }
)
