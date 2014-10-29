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
#' @example inst/examples/rmFromRegistryByUid.r
#' @seealso \code{
#'   	\link[reactr]{rmFromRegistryByUid-character-environment-method}
#' }
#' @template author
#' @template references
setGeneric(
  name = "rmFromRegistryByUid",
  signature = c(
    "uid"
  ),
  def = function(
    uid,
    ...
  ) {
    standardGeneric("rmFromRegistryByUid")       
  }
)

#' @title
#' Remove From Registry (character)
#'
#' @description 
#' See generic: \code{\link[reactr]{rmFromRegistryByUid}}
#'      
#' @inheritParams rmFromRegistryByUid
#' @param uid \code{\link{character}}.
#' @param where \code{\link{environment}}.
#' @return See method
#'    \code{\link[reactr]{rmFromRegistryByUid-character-character-environment-method}}
#' @example inst/examples/rmFromRegistryByUid.r
#' @seealso \code{
#'    \link[reactr]{rmFromRegistryByUid}
#' }
#' @template author
#' @template references
#' @export
#' @aliases rmFromRegistryByUid-character-method
setMethod(
  f = "rmFromRegistryByUid", 
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


