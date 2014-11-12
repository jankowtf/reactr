#' @title
#' Compute Object UID (generic)
#'
#' @description 
#' Computes the UID of an object based on its name/ID (\code{id}) and the 
#' environment where it has been assigned to (\code{where}).
#' 
#' @param id \code{\link{character}}.
#'    Name of the reactive object.
#' @param where \code{\link{environment}}.
#'    Environment to create the object in.
#' @template threedots
#' @return \code{\link{character}}. Object UID (checksum).
#' @example inst/examples/computeObjectUid.r
#' @seealso \code{
#'   	\link[reactr]{setReactive}
#' }
#' @template author
#' @template references
#' @export 
#' @import shiny
#' @import digest
computeObjectUid <- function(
    id,
    where = parent.frame(),
    ...
  ) {

#   ls(where)
  where <- where
  digest::digest(list(id = id, where = capture.output(where)))
#   expr <- substitute(digest::digest(list(id = ID, where = WHERE)), 
#     list(ID = id, WHERE = capture.output(eval(where))))
#   print(expr)
#   eval(expr)
    
}
