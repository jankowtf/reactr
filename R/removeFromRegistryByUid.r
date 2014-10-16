#' @title
#' Remove From Registry
#'
#' @description 
#' Removes entry of associated reactive object that has been unset via 
#' \code{\link[reactr]{unsetReactiveByUid}} from the registry 
#' \code{getOption("reactr")$.registry}.
#'   	
#' @param uid \strong{Signature argument}.
#'    Object containing UID information.
#' @template threedots
#' @example inst/examples/removeFromRegistryByUid.r
#' @seealso \code{
#'   	\link[reactr]{removeFromRegistryByUid-character-environment-method}
#' }
#' @template author
#' @template references
setGeneric(
  name = "removeFromRegistryByUid",
  signature = c(
    "uid"
  ),
  def = function(
    uid,
    ...
  ) {
    standardGeneric("removeFromRegistryByUid")       
  }
)

#' @title
#' Remove From Registry (character)
#'
#' @description 
#' See generic: \code{\link[reactr]{removeFromRegistryByUid}}
#'      
#' @inheritParams removeFromRegistryByUid
#' @param uid \code{\link{character}}.
#' @param where \code{\link{environment}}.
#' @return See method
#'    \code{\link[reactr]{removeFromRegistryByUid-character-character-environment-method}}
#' @example inst/examples/removeFromRegistryByUid.r
#' @seealso \code{
#'    \link[reactr]{removeFromRegistryByUid}
#' }
#' @template author
#' @template references
#' @export
#' @aliases removeFromRegistryByUid-character-method
setMethod(
  f = "removeFromRegistryByUid", 
  signature = signature(
    uid = "character"
  ), 
  definition = function(
    uid,
    ...
  ) {
    
  out <- FALSE
  registry <- getRegistry()
  if (is.null(registry)) {
    stop("No registry environment available ('getOption(\"reactr\")$.registry')")
  }
  if (length(uid)) {
    if (exists(uid, envir = registry, inherits = FALSE)) {
      rm(list = uid, envir = registry, inherits = FALSE)
      out <- TRUE
    }
  }
  return(out)  
    
  }
)


