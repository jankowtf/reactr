#' @title
#' Class: BindingContractObserved.S3 
#'
#' @description
#' Class representing git versions (S3) and its constructor function.
#' 
#' @template intended-use
#'
#' @param .x \code{\link{ANY}}. An object of an arbitrary class whose class
#'    attribute should be updated so that it becomes an instance of class
#'    \code{BindingContractObserved.S3}. Mainly intended for rapid prototyping 
#'    purposes
#'    
#' @field where \code{\link{environment}}. Own environment.
#' @field binding \code{\link{function}}. Binding function.
#' @return Instance of class \code{BindingContractObserved.S3}.
#' @example inst/examples/BindingContractObserved.S3.r
#' @seealso \code{
#'   	\link[reactr]{BindingContractObserving.S3}
#' }
#' @template author
#' @template references
#' @export
BindingContractObserved.S3 <- function(
  .x,
  where = .GlobalEnv,
  binding = function() {}
) {
  if (!missing(.x)) {
    class(.x) <- c("BindingContractObserved.S3", class(.x))
    out <- .x
  } else {
    out <- structure(
      list(where = where, binding = binding),
      class = c("BindingContractObserved.S3", "list")
    )
  }
  return(out)
}
