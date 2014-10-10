#' @title
#' Remove From Hash Registry
#'
#' @description 
#' Removes entry of associated reactive object that has been unset via 
#' \code{\link[reactr]{unsetReactive}} from hash registry \code{<where>[[.hash_id]]}.
#'   	
#' @param id \strong{Signature argument}.
#'    Object containing id information.
#' @param where \strong{Signature argument}.
#'    Object containing location information.
#' @param .hash_id \code{\link{character}}.
#'    Name of the auxiliary environment for caching hash values. 
#'    Default: \code{"._HASH"}. Keep it unless this name is already taken in 
#'    either \code{where} or \code{where_watch}.
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
    watch = character(),
    where,
    .hash_id = "._HASH",
    ...
  ) {
    standardGeneric("removeFromHashRegistry")       
  }
)

#' @title
#' Remove From Hash Registry
#'
#' @description 
#' See generic: \code{\link[reactr]{removeFromHashRegistry}}
#'      
#' @inheritParams removeFromHashRegistry
#' @param id \code{\link{character}}.
#' @param where \code{\link{environment}}.
#' @return See method
#'    \code{\link[reactr]{removeFromHashRegistry-character-character-environment-method}}
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
    watch,
    where,
    .hash_id,
    ...
  ) {
    
  return(removeFromHashRegistry(
    id = id,
    where = where,
    .hash_id = .hash_id,
    ...
  ))
    
  }
)

#' @title
#' Remove From Hash Registry
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
    .hash_id,
    ...
  ) {

  out <- FALSE
  if (  exists(.hash_id, envir = where, inherits = FALSE) &&
        length(id)
  ) {
    if (exists(id, envir = where[[.hash_id]], inherits = FALSE)) {
      rm(list = id, envir = where[[.hash_id]], inherits = FALSE)
      out <- TRUE
    }  
  }
  return(out)
    
  }
)
