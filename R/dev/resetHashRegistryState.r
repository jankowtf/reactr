#' @title
#' Reset Hash Registry State
#'
#' @description 
#' Resetss the required state of the hash registry \code{<where>[[.hash_id]]}.
#'   	
#' @param id \strong{Signature argument}.
#'    Object containing id information.
#' @param watch \strong{Signature argument}.
#'    Object containing monitored variable information.
#' @param where \strong{Signature argument}.
#'    Object containing location information.
#' @template threedots
#' @example inst/examples/resetRegistryState.r
#' @seealso \code{
#'   	\link[reactr]{resetRegistryState-missing-method}
#' }
#' @template author
#' @template references
setGeneric(
  name = "resetRegistryState",
  signature = c(
    "id",
    "watch",
    "where"
  ),
  def = function(
    id,
    watch = character(),
    where,
    .hash_id = "._HASH",
    ...
  ) {
    standardGeneric("resetRegistryState")       
  }
)

#' @title
#' Reset Hash Registry State
#'
#' @description 
#' See generic: \code{\link[reactr]{resetRegistryState}}
#'      
#' @inheritParams resetRegistryState
#' @param id \code{\link{character}}.
#' @param watch \code{\link{missing}}.
#' @param where \code{\link{environment}}.
#' @return See method
#'    \code{\link[reactr]{resetRegistryState-character-character-environment-method}}
#' @example inst/examples/resetRegistryState.r
#' @seealso \code{
#'    \link[reactr]{resetRegistryState}
#' }
#' @template author
#' @template references
#' @export
setMethod(
  f = "resetRegistryState", 
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
    
  return(resetRegistryState(
    id = id,
    watch = watch,
    where = where,
    .hash_id = .hash_id,
    ...
  ))
    
  }
)

#' @title
#' Reset Hash Registry State
#'
#' @description 
#' See generic: \code{\link[reactr]{resetRegistryState}}
#'   	 
#' @inheritParams resetRegistryState
#' @param id \code{\link{character}}.
#' @param watch \code{\link{character}}.
#' @param where \code{\link{environment}}.
#' @return \code{\link{logical}}. \code{TRUE}.
#' @example inst/examples/resetRegistryState.r
#' @seealso \code{
#'    \link[reactr]{resetRegistryState}
#' }
#' @template author
#' @template references
#' @export
setMethod(
  f = "resetRegistryState", 
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
