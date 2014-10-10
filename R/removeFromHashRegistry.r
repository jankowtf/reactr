#' @title
#' Remove From Hash Registry
#'
#' @description 
#' Removes entry of associated reactive object that has been unset via 
#' \code{\link[reactr]{unsetReactiveByUid}} from hash registry \code{<where>[[.hash_id]]}.
#'   	
#' @param uid \strong{Signature argument}.
#'    Object containing UID information.
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
    "uid"
  ),
  def = function(
    uid,
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
#' @param uid \code{\link{character}}.
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
    uid = "character"
  ), 
  definition = function(
    uid,
    ...
  ) {
    
  out <- FALSE
  hash_env <- getHashRegistry()
  if (is.null(hash_env)) {
    stop("No hash environment available ('getOption(\"reactr\")$.hash')")
  }
  if (length(uid)) {
    if (exists(uid, envir = hash_env, inherits = FALSE)) {
      rm(list = uid, envir = hash_env, inherits = FALSE)
      out <- TRUE
    }
  }
  return(out)  
    
  }
)


