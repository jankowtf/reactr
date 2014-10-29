#' @title
#' Get Checksum (generic)
#'
#' @description 
#' Retrieves the checksum of a reactive object's visible value.
#' 
#' @section Main S4 method:
#' 
#' \code{\link[reactr]{getChecksum-character-environment-method}}
#'   	
#' @param id \strong{Signature argument}.
#'    Object containing name/ID information.
#' @param where \strong{Signature argument}.
#'    Object containing location information.
#' @template threedots
#' @example inst/examples/getChecksum.r
#' @seealso \code{
#'   	\link[reactr]{getChecksum-character-environment-method}
#' }
#' @template author
#' @template references
setGeneric(
  name = "getChecksum",
  signature = c(
    "id",
    "where"
  ),
  def = function(
    id,
    where = parent.frame(),
    ...
  ) {
    standardGeneric("getChecksum")       
  }
)

#' @title
#' Get Checksum (character-missing)
#'
#' @description 
#' See generic: \code{\link[reactr]{getChecksum}}
#'      
#' @inheritParams getChecksum
#' @param id \code{\link{character}}.
#' @param where \code{\link{missing}}.
#' @return See method
#'    \code{\link[reactr]{getChecksum-character-environment-method}}
#' @example inst/examples/getChecksum.r
#' @seealso \code{
#'    \link[reactr]{getChecksum},
#'    \link[reactr]{getChecksum-character-environment-method}
#' }
#' @template author
#' @template references
#' @export
#' @aliases getChecksum-character-missing-method
setMethod(
  f = "getChecksum", 
  signature = signature(
    id = "character",
    where = "missing"
  ), 
  definition = function(
    id,
    where,
    ...
  ) {

  getChecksum(
    id = id,
    where = where,
    ...
  )
    
  }
)

#' @title
#' Get Checksum (character-environment)
#'
#' @description 
#' See generic: \code{\link[reactr]{getChecksum}}
#'   	 
#' @inheritParams getChecksum
#' @param id \code{\link{character}}.
#' @param where \code{\link{environment}}.
#' @return \code{\link{function}}. Binding function.
#' @example inst/examples/getChecksum.r
#' @seealso \code{
#'    \link[reactr]{getChecksum}
#' }
#' @template author
#' @template references
#' @aliases getChecksum-character-environment-method
#' @export
setMethod(
  f = "getChecksum", 
  signature = signature(
    id = "character",
    where = "environment"
  ), 
  definition = function(
    id,
    where,
    ...
  ) {

  getFromRegistry(id = id, where = where)$.checksum
    
  }
)
