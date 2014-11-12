#' @title
#' Checks if Object is Reactive (generic)
#'
#' @description 
#' Checks if object is reactive by investigating if it
#' 
#' \itemize{
#'    \item{has an active binding via \code{\link[base]{bindingIsActive}}}
#'    \item{has reference of class \code{\link[reactr]{ReactiveObject.S3}} 
#'      in registry (\code{\link[reactr]{getRegistry}})}
#' }
#'   	
#' @param id \strong{Signature argument}.
#'    Object containing name/ID information.
#' @param where \strong{Signature argument}.
#'    Object containing location information.
#' @template threedots
#' @example inst/examples/isReactive.r
#' @seealso \code{
#'   	\link[reactr]{isReactive-character-environment-method}
#' }
#' @template author
#' @template references
setGeneric(
  name = "isReactive",
  signature = c(
    "id",
    "where"
  ),
  def = function(
    id,
    where = parent.frame(),
    ...
  ) {
    standardGeneric("isReactive")       
  }
)

#' @title
#' Get From Registry (character-missing)
#'
#' @description 
#' See generic: \code{\link[reactr]{isReactive}}
#'      
#' @inheritParams isReactive
#' @param id \code{\link{character}}.
#' @param where \code{\link{missing}}.
#' @return See method
#'    \code{\link[reactr]{isReactive}}
#' @example inst/examples/isReactive.r
#' @seealso \code{
#'    \link[reactr]{isReactive}
#' }
#' @template author
#' @template references
#' @export
#' @aliases isReactive-character-missing-method
setMethod(
  f = "isReactive", 
  signature = signature(
    id = "character",
    where = "missing"
  ), 
  definition = function(
    id,
    where,
    ...
  ) {

  isReactive(
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
#' See generic: \code{\link[reactr]{isReactive}}
#'   	 
#' @inheritParams isReactive
#' @param id \code{\link{character}}.
#' @param where \code{\link{environment}}.
#' @return \code{\link{logical}}. 
#'    \code{TRUE}: reactive object; 
#'    \code{FALSE}: regular/non-reactive object.
#' @example inst/examples/isReactive.r
#' @seealso \code{
#'    \link[reactr]{isReactive}
#' }
#' @template author
#' @template references
#' @export
#' @aliases isReactive-character-environment-method
setMethod(
  f = "isReactive", 
  signature = signature(
    id = "character",
    where = "environment"
  ), 
  definition = function(
    id,
    where,
    ...
  ) {

  if (length(id)) {
    has_binding <- try(bindingIsActive(id, where), silent = TRUE)
    if (inherits(has_binding, "try-error")){
      has_binding <- FALSE
    } 
#     in_registry <- exists(computeObjectUid(id = id, where = where), 
#       getRegistry(), inherits = FALSE)
#     has_binding && in_registry
    has_binding
  } else {
    FALSE
  }
    
  }
)

