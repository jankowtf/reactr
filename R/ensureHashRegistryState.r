#' @title
#' Ensure Hash Registry State
#'
#' @description 
#' Ensures the required state of the hash registry \code{<where>$.hash}.
#'   	
#' @param id \strong{Signature argument}.
#'    Object containing id information.
#' @param watch \strong{Signature argument}.
#'    Object containing monitored variable information.
#' @param where \strong{Signature argument}.
#'    Object containing location information.
#' @template threedot
#' @example inst/examples/ensureHashRegistryState.r
#' @seealso \code{
#'   	\link[reactr]{ensureHashRegistryState-missing-method}
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
    where,
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
#' @import rapp.core.condition
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
    ...
  ) {
    
  return(ensureHashRegistryState(
    id = id,
    watch = watch,
    where = where,
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
#' @import rapp.core.condition
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
    ...
  ) {

  if (length(id) && length(watch)) {
    tmp <- watch
    watch <- id
    id <- tmp
  }    
  if (length(id)) {
    if (!exists(id, envir = where$.hash, inherits = FALSE)) {
      assign(id, new.env(), envir = where$.hash)
    }  
    if (!exists(id, envir = where$.hash[[id]], inherits = FALSE)) {
      assign(id, character(), envir = where$.hash[[id]])
    }
  }

  if (length(watch)) {
    if (!exists(id, envir = where$.hash[[watch]], inherits = FALSE)) {
      assign(id, character(), envir = where$.hash[[watch]])
    }
  }
  
  return(TRUE)
    
  }
)
