#' @title
#' Class: Reactive.S3 
#'
#' @description
#' Class representing the system state (S3) and its constructor function.
#' 
#' @template intended-use
#'
#' @param .x \code{\link{ANY}}. An object of an arbitrary class whose class
#'    attribute should be updated so that it becomes an instance of class
#'    \code{Reactive.S3}. Mainly intended for rapid prototyping 
#'    purposes
#'    
#' @field id \code{\link{character}}.
#'    Object ID.
#'    Initial: \code{character()}.
#' @field uid \code{\link{character}}.
#'    Object ID.
#'    Initial: \code{character()}. Automatically computed once \code{id} is 
#'    specified: \code{
#'      eval(substitute(
#'        digest::digest(list(id = ID, where = WHERE)), 
#'        list(ID = id, WHERE = where))
#'      )
#'    }.
#' @field value \code{\link{ANY}}.
#'    Actual value.
#'    Initial: \code{NULL}.
#' @field where \code{\link{environment}}.
#'    Environment of reactive object.
#'    Initial: \code{.GlobalEnv}.
#' @field hash \code{\link{environment}}.
#'    Environment for hash value storage.
#'    Initial: \code{getHashRegistry()}.
#' @field dependees \code{\link{environment}}.
#'    Environment storing information of objects that depend on this object.
#'    Initial: \code{new.env(parent = emptyenv())}.
#'    \strong{Not used currently, for potential future use only}.
#' @field dependencies \code{\link{environment}}.
#'    Environment storing information of objects that this object depends on.
#'    Initial: \code{new.env(parent = emptyenv())}.
#'    \strong{Not used currently, for potential future use only}.
#' @return Instance of class \code{Reactive.S3}.
#' @example inst/examples/Reactive.S3.r
#' @seealso \code{
#'   	\link[reactr]{setReactiveS3}
#' }
#' @template author
#' @template references
#' @export
Reactive.S3 <- function(
  .x,
  id = character(),
  uid = character(),
  value = character(),
  where = .GlobalEnv,
  hash = getHashRegistry(),
  dependees = new.env(parent = emptyenv()),
  dependencies = new.env(parent = emptyenv())
) {
  if (!missing(.x)) {
    class(.x) <- c("Reactive.S3", class(.x))
    out <- .x
  } else {
    out <- new.env()
    out$id <- id
    out$value <- value
    out$hash <- hash
    out$dependees <- dependees
    out$dependencies <- dependencies
    if (length(id)) {
      out$uid <- eval(substitute(digest::digest(list(id = ID, where = WHERE)), 
        list(ID = id, WHERE = where)))
    }
    class(out) <- c("Reactive.S3", class(out))
  }
  return(out)
}
