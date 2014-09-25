#' @title
#' Reactive object bindings with built-in caching
#'
#' @description
#' Facilitates the specification of reactive object bindings based
#' on binding contracts. That way, an object \code{x} can be dynamically
#' observed by \code{n} other objects. Whenever \code{x} changes, the 
#' \code{n} objects observing \code{x} change according to their binding 
#' contracts that can be defined via regular functions (\code{\link{function}}).
#' 
#' @details 
#' The core functions/methods of this package: 
#'  \itemize{
#'    \item{\code{\link[reactr]{setReactive}}: }{
#'      Facilitates the specification of reactive object bindings (S4 method).
#'    }
#'    \item{\code{\link[reactr]{setReactive_bare}}: }{
#'      Bare S3 version of \code{\link[reactr]{setReactive}}. About roughly 
#'      \code{10 - 15} faster than the S4 method.
#'    }
#'    \item{\code{\link[reactr]{getReactive}}: }{
#'      Alternative way of retrieving objects from an environment.
#'    }
#'    \item{\code{\link[reactr]{unsetReactive}}: }{
#'      Removes reactive binding from an object and turns it into a regularly
#'      assigned object again.
#'    }
#'    \item{\code{\link[reactr]{removeReactive}}: }{
#'      Removes an reactive object from its environment completely.
#'      This is equivalent to \code{\link[base]{rm}} with a previous call to
#'      \code{\link[reactr]{unsetReactive}}
#'    }   
#' }
#' 
#' A more internal but very central function:
#' \itemiz{
#'    \item{\code{\link[reactr]{getBoilerplateCode}}: }{
#'      Function that retrieves boilerplate code as required by 
#'      \code{\link[reactr]{setReactive}} and \code{\link[base]{makeActiveBinding}}.
#'    }
#' }
#' 
#' @template author
#' @template references
#' @docType package
#' @name reactr
NULL
