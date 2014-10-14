#' @title
#' Print Method
#'
#' @description 
#' Print method for class \code{Reactive.S3} 
#' (see \code{\link[reactr]{Reactive.S3}}).
#' 
#' @param x \code{\link[reactr]{Reactive.S3}}.
#'    Instance to be printed.
#' @template threedot
#' @example inst/examples/print.Reactive.S3.r
#' @seealso \code{
#'   	\link[reactr]{Reactive.S3}
#' }
#' @template author
#' @template references
#' @export 
print.Reactive.S3 <- function (x, as_is = TRUE, ...) {
    if (as_is) {
      print.default(x)
      invisible(x)
    } else {
      print(x$value, ...)
#       invisible(x)
      invisible(x$value)
    }
}

