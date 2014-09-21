#' @title
#' Get Value from Environment
#'
#' @description 
#' Retrieves value from an environment or any of its sub-environments based
#' on a path-like ID.
#' 
#' @details
#' Values for \code{id} are expected to be of structure \code{a/b/c/.../z},
#' i.e. being a path-like identifier with a slash used as separator. 
#' The identifier is transformed to \code{a$b$c$...$z} and then in turn to a
#' valid \emph{get} expression (\code{where$a$b$c$...$z}).
#'   	
#' @param id \strong{Signature argument}.
#'    Object containing path-like ID information.
#' @param where \strong{Signature argument}.
#'    Object containing environment information.
#' @template threedot
#' @example inst/examples/getValue.r
#' @seealso \code{
#'   	\link[rapp.core.environment]{getValue-character-environment-method}
#' }
#' @template author
#' @template references
#' @export 
setGeneric(
  name = "getValue",
  signature = c(
    "id",
    "where"
  ),
  def = function(
    id,
    where,
    ...
  ) {
    standardGeneric("getValue")       
  }
)

#' @title
#' Get Value from Environment
#'
#' @description 
#' See generic: \code{\link[rapp.core.environment]{getValue}}
#'   	 
#' @inheritParams getValue
#' @param id \code{\link{character}}.
#' @return \code{\link{ANY}}. Option value or for non-existing option 
#'    (i.e. wrong \code{id}): \code{NULL} if \code{strict = FALSE} and an error
#'    if \code{strict = TRUE}.
#' @example inst/examples/getValue.r
#' @seealso \code{
#'    \link[rapp.core.environment]{getValue}
#' }
#' @template author
#' @template references
#' @export
#' @import rapp.core.condition
setMethod(
  f = "getValue", 
  signature = signature(
    id = "character",
    where = "environment"
  ), 
  definition = function(
    id,
    where,
    ...
  ) {

  watch <- where$.watch[[id]]
  if (!is.null(watch)) {
    idx <- sapply(watch, function(ii) {
      hash_0 <- get(ii, envir = where$.hash[[ii]], inherits = FALSE)
      hash_1 <- get(id, envir = where$.hash[[ii]], inherits = FALSE)
      hash_0 != hash_1
    })
   
    if (any(idx)) {
      ## Update //
      out <- setValue(
        id = id, 
        where = where, 
#         binding = substitute(BINDING), 
#           list(BINDING = get(id, where$.bindings, inherits = FALSE)),
        binding = get(id, envir = where$.bindings, inherits = FALSE),
        watch = watch,
        binding_type = 2
      )
    } else {
      out <- get(id, envir = where, inherits = FALSE)
    }
  } else {
    out <- get(id, envir = where, inherits = FALSE)
  }
      
  return(out)
    
  }
)
