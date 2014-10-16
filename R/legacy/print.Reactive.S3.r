#' @title
#' Print Method
#'
#' @description 
#' Print method for class \code{ReactiveObject.S3} 
#' (see \code{\link[reactr]{ReactiveObject.S3}}).
#' 
#' @param x \code{\link[reactr]{ReactiveObject.S3}}.
#'    Instance to be printed.
#' @template threedots
#' @example inst/examples/print.ReactiveObject.S3.r
#' @seealso \code{
#'   	\link[reactr]{ReactiveObject.S3}
#' }
#' @template author
#' @template references
#' @export 
print.ReactiveObject.S3 <- function (x, as_is = TRUE, ...) {
    if (as_is) {
      print.default(x)
      invisible(x)
    } else {
      print(x$value, ...)
#       invisible(x)
      invisible(x$value)
    }
}

