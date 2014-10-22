#' @title
#' Copy Reactive Object (generic)
#'
#' @description 
#' Copies essential information of a reactive object to a new object.
#' 
#' @details
#' Takes the essential information of a reactive object (fields \code{value}, 
#' \code{refs_pull}, \code{refs_push} and \code{funct} of the hidden instance 
#' of class \code{\link[reactr]{ReactiveObject.S3}} and creates a new reactive 
#' object of name \code{id} in \code{where} with that information. 
#' The important thing to note is that the two objects (and their hidden objects)
#' are not connected via any environments (pass-by-reference) as would be the 
#' case if the copy was carried out by a mere call to \code{\link[base]{<-}}.
#'   	
#' @param id_from \strong{Signature argument}.
#'    Object containing name/ID information of object to copy.
#' @param where_from \strong{Signature argument}.
#'    Object containing location information of object \code{id_from}.
#' @param id_to \strong{Signature argument}.
#'    Object containing name/ID information of object to assign the copy to.
#' @param where_to \strong{Signature argument}.
#'    Object containing location information of object that the copy is 
#'    assigned to.
#' @template threedots
#' @example inst/examples/copyReactive.r
#' @seealso \code{
#'   	\link[reactr]{copyReactive-character-character-environment-environment-method}
#' }
#' @template author
#' @template references
#' @export 
setGeneric(
  name = "copyReactive",
  signature = c(
    "id_from",
    "where_from",
    "id_to",
    "where_to"
  ),
  def = function(
    id_from,
    where_from = parent.frame(),
    id_to,
    where_to = parent.frame(),
    ...
  ) {
    standardGeneric("copyReactive")       
  }
)

#' @title
#' Copy Reactive Object (character-missing-character-missing)
#'
#' @description 
#' See generic: \code{\link[reactr]{copyReactive}}
#'      
#' @inheritParams copyReactive
#' @param id_from \code{\link{character}}.
#' @param where_from \code{\link{missing}}.
#' @param id_to \code{\link{character}}.
#' @param where_from \code{\link{missing}}.
#' @return See method
#'    \code{\link[reactr]{copyReactive-character-environment-character-environment-method}}
#' @example inst/examples/copyReactive.r
#' @seealso \code{
#'    \link[reactr]{copyReactive},
#'    \link[reactr]{copyReactive-character-environment-character-environment-method}
#' }
#' @template author
#' @template references
#' @export
#' @aliases copyReactive-character-missing-character-missing-method
setMethod(
  f = "copyReactive", 
  signature = signature(
    id_from = "character",
    where_from = "missing",
    id_to = "character",
    where_to = "missing"
  ), 
  definition = function(
    id_from,
    where_from,
    id_to,
    where_to,
    ...
  ) {

  copyReactive(
    id_from = id_from, 
    where_from = where_from,
    id_to = id_to, 
    where_to = where_to,
    ...
  )
    
  }
)

#' @title
#' Copy Reactive Object (character-environment-character-environment)
#'
#' @description 
#' See generic: \code{\link[reactr]{copyReactive}}
#'   	 
#' @inheritParams copyReactive
#' @param id_from \code{\link{character}}.
#' @param where_from \code{\link{environment}}.
#' @param id_to \code{\link{character}}.
#' @param where_from \code{\link{environment}}.
#' @return \code{\link{ANY}}. Object/object value.
#' @example inst/examples/copyReactive.r
#' @seealso \code{
#'    \link[reactr]{copyReactive}
#' }
#' @template author
#' @template references
#' @export
#' @aliases copyReactive-character-environment-character-environment-method
setMethod(
  f = "copyReactive", 
  signature = signature(
    id_from = "character",
    where_from = "environment",
    id_to = "character",
    where_to = "environment"
  ), 
  definition = function(
    id_from,
    where_from,
    id_to,
    where_to,
    ...
  ) {

  from <- getFromRegistry(id = id_from, where = where_from)
  from$copy(id = id_to, where = where_to)
    
  }
)
