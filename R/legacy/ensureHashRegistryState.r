#' @title
#' Ensure Hash Registry State
#'
#' @description 
#' Ensures the required state of the hash registry \code{<where>[[.hash_id]]}.
#'     
#' @param id \strong{Signature argument}.
#'    Object containing id information.
#' @param watch \strong{Signature argument}.
#'    Object containing monitored variable information.
#' @param where \strong{Signature argument}.
#'    Object containing location information.
#' @param .hash_id \code{\link{character}}.
#'    Name of the auxiliary environment for caching hash values. 
#'    Default: \code{"._HASH"}. Keep it unless this name is already taken in 
#'    either \code{where} or \code{where_watch}.
#' @template threedots
#' @example inst/examples/ensureHashRegistryState.r
#' @seealso \code{
#'   	\link[reactr]{ensureHashRegistryState-character-character-environment-method}
#' }
#' @template author
#' @template references
setGeneric(
  name = "ensureHashRegistryState",
  signature = c(
    "id",
    "watch",
    "where"
  ),
  def = function(
    id,
    watch = character(),
    where = .GlobalEnv,
    .hash_id = "._HASH",
    ...
  ) {
    standardGeneric("ensureHashRegistryState")       
  }
)

#' @title
#' Ensure Hash Registry State
#'
#' @description 
#' See generic: \code{\link[reactr]{ensureHashRegistryState}}
#'      
#' @inheritParams ensureHashRegistryState
#' @param id \code{\link{character}}.
#' @param watch \code{\link{missing}}.
#' @param where \code{\link{environment}}.
#' @return See method
#'    \code{\link[reactr]{ensureHashRegistryState-character-character-environment-method}}
#' @example inst/examples/ensureHashRegistryState.r
#' @seealso \code{
#'    \link[reactr]{ensureHashRegistryState}
#' }
#' @template author
#' @template references
#' @export
setMethod(
  f = "ensureHashRegistryState", 
  signature = signature(
    id = "character",
    watch = "missing",
    where = "environment"
  ), 
  definition = function(
    id,
    watch,
    where,
    .hash_id,
    ...
  ) {
    
  return(ensureHashRegistryState(
    id = id,
    watch = watch,
    where = where,
    .hash_id = .hash_id,
    ...
  ))
    
  }
)

#' @title
#' Ensure Hash Registry State
#'
#' @description 
#' See generic: \code{\link[reactr]{ensureHashRegistryState}}
#'   	 
#' @inheritParams ensureHashRegistryState
#' @param id \code{\link{character}}.
#' @param watch \code{\link{character}}.
#' @param where \code{\link{environment}}.
#' @return \code{\link{logical}}. \code{TRUE}.
#' @example inst/examples/ensureHashRegistryState.r
#' @seealso \code{
#'    \link[reactr]{ensureHashRegistryState}
#' }
#' @template author
#' @template references
#' @export
setMethod(
  f = "ensureHashRegistryState", 
  signature = signature(
    id = "character",
    watch = "character",
    where = "environment"
  ), 
  definition = function(
    id,
    watch,
    where,
    .hash_id,
    ...
  ) {

  if (!exists(.hash_id, envir = where, inherits = FALSE)) {
    assign(.hash_id, new.env(), envir = where)
  }     
    
  ## Turn things around when watching another variable //
  ## This means that the 'id' part is actually the 'watch' part
  ## and the 'watch' part takes care of assigning the hash value of 'id'
  ## in the hash environment of 'watch'
  if (length(id) && length(watch)) {
    tmp <- watch
    watch <- id
    id <- tmp
  }    
  if (length(id)) {
    if (!exists(id, envir = where[[.hash_id]], inherits = FALSE)) {
      assign(id, new.env(), envir = where[[.hash_id]])
    }  
    if (!exists(id, envir = where[[.hash_id]][[id]], inherits = FALSE)) {
      assign(id, digest::digest(NULL), envir = where[[.hash_id]][[id]])
    }
  }

  if (length(watch)) {
    if (!exists(watch, envir = where[[.hash_id]][[id]], inherits = FALSE)) {
      assign(watch, digest::digest(NULL), envir = where[[.hash_id]][[id]])
    }
  }
  
  return(TRUE)
    
  }
)
