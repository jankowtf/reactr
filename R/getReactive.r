#' @title
#' Get Reactive Object (generic)
#'
#' @description 
#' Retrieves an reactive object, or, to be more precise, the \strong{visible}
#' part of the object which corresponds to the \code{value} field of the 
#' hidden instance of class \code{\link[reactr]{ReactiveObject.S3}}.
#'   	
#' @param id \strong{Signature argument}.
#'    Object containing name/ID information.
#' @param where \strong{Signature argument}.
#'    Object containing location information.
#' @param hidden \code{\link{logical}}.
#'    \code{TRUE}: retrieve entire \strong{hidden} object/instance from registry;
#'    \code{FALSE}: retrieve the visible part of the object/instance.
#' @template threedots
#' @example inst/examples/getReactive.r
#' @seealso \code{
#'   	\link[reactr]{getReactive-character-environment-method}
#' }
#' @template author
#' @template references
#' @export 
setGeneric(
  name = "getReactive",
  signature = c(
    "id",
    "where"
  ),
  def = function(
    id,
    where = parent.frame(),
    hidden = FALSE,
    ...
  ) {
    standardGeneric("getReactive")       
  }
)

#' @title
#' Get Reactive Object (character-missing)
#'
#' @description 
#' See generic: \code{\link[reactr]{getReactive}}
#'      
#' @inheritParams getReactive
#' @param id \code{\link{character}}.
#' @param where \code{\link{missing}}.
#' @return See method
#'    \code{\link[reactr]{getReactive-character-environment-method}}
#' @example inst/examples/getReactive.r
#' @seealso \code{
#'    \link[reactr]{getReactive}
#' }
#' @template author
#' @template references
#' @export
setMethod(
  f = "getReactive", 
  signature = signature(
    id = "character",
    where = "missing"
  ), 
  definition = function(
    id,
    where,
    hidden,
    ...
  ) {

  getReactive(
    id = id, 
    where = where, 
    hidden = hidden,
    ...
  )
    
  }
)

#' @title
#' Get Reactive Object (character-environment)
#'
#' @description 
#' See generic: \code{\link[reactr]{getReactive}}
#'   	 
#' @inheritParams getReactive
#' @param id \code{\link{character}}.
#' @param where \code{\link{environment}}.
#' @return \code{\link{ANY}}. Object/object value.
#' @example inst/examples/getReactive.r
#' @seealso \code{
#'    \link[reactr]{getReactive}
#' }
#' @template author
#' @template references
#' @export
setMethod(
  f = "getReactive", 
  signature = signature(
    id = "character",
    where = "environment"
  ), 
  definition = function(
    id,
    where,
    hidden,
    ...
  ) {

  if (!hidden) {
    out <- get(x = id, envir = where, inherits = FALSE)
  } else {
    out <- getFromRegistry(id = id, where = where)
  }
      
  return(out)
    
  }
)
