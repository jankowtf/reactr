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
#'    Initial: \code{parent.frame()}.
#' @field hash \code{\link{environment}}.
#'    Environment for hash value storage.
#'    Initial: \code{getHashRegistry()}.
#' @field references \code{\link{environment}}.
#'    Environment storing information of referenced objects.
#'    Initial: \code{new.env(parent = emptyenv())}.
#' @field condition \code{\link{condition}} (at least by inheritance).
#'    If a condition has been signaled, this field is assigned a respectiv 
#'    condition object that is triggered when \code{.self$value} is requested.
#'    See \code{\link[base]{signalCondition}} and 
#'    \code{\link[conditionr]{signalCondition}}
#'    Initial: \code{NULL}.
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
  where = parent.frame(),
  hash = getHashRegistry(),
  references = new.env(parent = emptyenv()),
  condition = NULL
) {
  if (!missing(.x)) {
    class(.x) <- c("Reactive.S3", class(.x))
    out <- .x
  } else {
    out <- new.env()
    out$id <- id
    out$value <- value
    out$where <- where
    out$hash <- hash
    out$references <- references
    if (length(id)) {
      out$uid <- eval(substitute(digest::digest(list(id = ID, where = WHERE)), 
        list(ID = id, WHERE = where)))
    }
    out$condition <- condition
    
    ## Methods //
    out$hasReferences <- function(self = out) {
      length(ls(self$references, all.names = TRUE)) > 0
    }
    
    class(out) <- c("Reactive.S3", class(out))
  }
  return(out)
}
