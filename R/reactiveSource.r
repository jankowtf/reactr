#' @title
#' Create Reactive Source Object
#'
#' @description 
#' Creates a reactive source object that can be used by 
#' observable reactive objects created via \code{\link[reactr]{setShinyReactive}}
#' or \code{\link[shiny]{reactive}}.
#' 
#' @details 
#' This is a slightly modified version of \link[shiny]{makeReactiveBinding}.
#'  
#' @param id \code{\link{character}}.
#'    Name/ID of the reactive source object.
#' @param value \code{\link{ANY}}.
#'    Value of reactive source object.
#' @param where \code{\link{environment}}.
#'    Environment in which to create the object.
#' @param overwrite \code{\link{logical}}.
#'    Only relevant if object already exists.
#'    \code{TRUE}: overwrite existing value;
#'    \code{FALSE}: keep currrent value. 
#' @param typed \code{\link{logical}}.
#'    \code{TRUE}: checks class validity of assignment value specified via
#'    \code{value} and throws an error if classes do not match or if the class 
#'    of the assignment value does not inherit from the class of field value 
#'    \code{.value} at initialization;
#'    \code{FALSE}: no class check is performed.
#'    Note that initial values of \code{NULL} are disregarded for the class 
#'    check, i.e. any value overwriting an initial \code{NULL} value 
#'    is valid.
#' @param strict_set \code{\link{numeric}}.
#'    Relevant if \code{typed = TRUE} and class mismatch.
#'    \itemize{
#'      \item{\code{0}: } {ignore without warning}
#'      \item{\code{1}: } {ignore with Warning}
#'      \item{\code{2}: } {stop with error}
#'    }
#' @param Further arguments to be passed to subsequent functions.
#'    In particular: 
#'    \code{\link[typr]{setTyped}} and \code{\link[typr]{validateType}}.
#' @return \code{ANY}. The value of \code{value}.
#' @example inst/examples/reactiveSource.r
#' @seealso \code{
#'    \link[reactr]{setShinyReactive},
#'    \link[shiny]{reactive}
#' }
#' @template author
#' @template references
#' @import conditionr
#' @import shiny
#' @import typr
#' @export 
reactiveSource <- function(
  id, 
  value = NULL,
  where = parent.frame(),
  overwrite = TRUE,
  typed = FALSE,
  strict_set = c(0,1,2),
  ...
) {
  
  strict_set <- as.numeric(match.arg(as.character(strict_set), 
    as.character(c(0, 1, 2))))
  
  if (typed) {
    invis <- typr::setTyped(id = id, value = value, where = where, 
      return_invis = 1, ...)
  }
  
  ## Ensure that shiny let's us do our thing //
  shiny_opt <- getOption("shiny.suppressMissingContextError")
  if (is.null(shiny_opt) || !shiny_opt) {
    options(shiny.suppressMissingContextError = TRUE)  
  }
  
  if (exists(id, where = where, inherits = FALSE) && !is.null(value)) {
    if (!overwrite) {
      value <- get(id, pos = where, inherits = FALSE)
    } 
#     rm(list = id, pos = where, inherits = FALSE)
  }
  values <- shiny:::reactiveValues(value = value)

  ## Add some stuff to the instance of `reactiveValues` the dirty way //
  ## 1) Transfer stuff from `invis` //
  values$.id <- invis$.id
  values$.uid <- invis$.uid
  values$.where <- invis$.where
  values$.class <- invis$.class
  values$.validateType <- typr::validateType
  
  makeActiveBinding(id, env = where, 
#     local({
      fun = function(v) {
        if (missing(v)) {
          values$value
        } else {
          is_valid <- if (typed) {
            values$.validateType(self = values, v = v, strict = strict_set, ...)
          } else {
            TRUE
          }            
          if (is_valid) {
            values$value <- v
          }
        }
      }
#     })
  )

  invisible(values$value)
#   invisible()

}
