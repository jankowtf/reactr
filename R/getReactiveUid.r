#' @title
#' Get UID of Reactive Object
#'
#' @description 
#' Computes the UID of an reactive object.
#' 
#' @param id \code{\link{character}}.
#'    Name of the reactive object.
#' @param where \code{\link{environment}}.
#'    Environment to create the object in.
#' @template threedot
#' @example inst/examples/getReactiveUid.r
#' @seealso \code{
#'   	\link[reactr]{setReactive-character-ANY-environment-character-call-method}
#' }
#' @template author
#' @template references
#' @export 
#' @import shiny
getReactiveUid <- function(
    id,
    where = .GlobalEnv,
    ...
  ) {

  return(eval(substitute(digest::digest(list(id = ID, where = WHERE)), 
    list(ID = id, WHERE = capture.output(eval(where))))))
    
}
