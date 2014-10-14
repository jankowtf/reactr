#' @title
#' Class: BindingContractObserving.S3 
#'
#' @description
#' Class representing git versions (S3) and its constructor function.
#' 
#' @template intended-use
#'
#' @param .x \code{\link{ANY}}. An object of an arbitrary class whose class
#'    attribute should be updated so that it becomes an instance of class
#'    \code{BindingContractObserving.S3}. Mainly intended for rapid prototyping 
#'    purposes
#'    
#' @field id \code{\link{character}}. Own ID.
#' @field where \code{\link{environment}}. Own environment.
#' @field what \code{\link{character}}. Object that is observed.
#' @field where_what \code{\link{environment}}. Environment of observed object.
#' @field binding \code{\link{function}}. Binding function.
#' @return Instance of class \code{BindingContractObserving.S3}.
#' @example inst/examples/BindingContractObserving.S3.r
#' @seealso \code{
#'   	\link[reactr]{BindingContractObserved.S3}
#' }
#' @template author
#' @template references
#' @export
BindingContractObserving.S3 <- function(
  .x,
  id = character(),
  where = .GlobalEnv,
  what = character(),
  where_what = where,
  binding = function(){}
) {
  if (!missing(.x)) {
    class(.x) <- c("BindingContractObserving.S3", class(.x))
    out <- .x
  } else {
    out <- structure(
      list(
        id = id,
        where = where,
        what = what,
        where_what = where_what,
        binding = binding
      ),
      class = c("BindingContractObserving.S3", "list")
    )
  }
  return(out)
}
