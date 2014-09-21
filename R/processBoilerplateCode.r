#' @title
#' Implement Binding Interface
#'
#' @description 
#' Implements the binding interface as required by 
#' \code{\link[reactr]{setValue}} when \code{binding_type = 1} (i.e. when
#' \code{\link[base]{makeActiveBinding}} is used).
#'   	
#' @param fun \strong{Signature argument}.
#'    Object containing function information.
#' @template threedot
#' @example inst/examples/processBoilerplateCode.r
#' @seealso \code{
#'   	\link[reactr]{processBoilerplateCode-function-method}
#' }
#' @template author
#' @template references
#' @export 
setGeneric(
  name = "processBoilerplateCode",
  signature = c(
    "fun"
  ),
  def = function(
    fun,
    ...
  ) {
    standardGeneric("processBoilerplateCode")       
  }
)

#' @title
#' Implement Binding Interface
#'
#' @description 
#' See generic: \code{\link[reactr]{processBoilerplateCode}}
#'   	 
#' @inheritParams processBoilerplateCode
#' @param fun \code{\link{function}}.
#' @return \code{\link{call}}. Implemented binding interface.
#' @example inst/examples/processBoilerplateCode.r
#' @seealso \code{
#'    \link[reactr]{processBoilerplateCode}
#' }
#' @template author
#' @template references
#' @export
#' @import rapp.core.condition
setMethod(
  f = "processBoilerplateCode", 
  signature = signature(
    fun = "function"
  ), 
  definition = function(
    fun,
    ...
  ) {

  out <- substitute(
    local({
      ## Initial value //
      if (  exists(watch, envir = where, inherits = FALSE) &&
            !is.null(get(watch, envir = where, inherits = FALSE))
      ) {
        VALUE <- BINDING_IFACE
      } else {
        VALUE <- NULL
      }
      
      ## Ensure hash value transfer //
      hash_0 <- as.character(where$.hash[[watch]][[watch]])
      hash_1 <- as.character(where$.hash[[watch]][[id]])
#       if (  exists(watch, envir = where$.hash[[watch]], inherits = FALSE) &&
#             !exists(id, envir = where$.hash[[watch]], inherits = FALSE)
#       ) {
      if (!length(hash_1)) {
        assign(
          id, 
#           get(watch, envir = where$.hash[[watch]]),
          hash_0,
          where$.hash[[watch]]
        )
      }
print("DEBUG")
print(hash_0)
print(hash_1)
      function(v) {
        if (  exists(watch, envir = where, inherits = FALSE) &&
              !is.null(get(watch, envir = where, inherits = FALSE))
        ) {
          if (missing(v)) {
            hash_0 <- where$.hash[[watch]][[watch]]
            hash_1 <- where$.hash[[watch]][[id]]
            message(hash_0)
            message(hash_1)
            if (hash_0 != hash_1) {
#               message("monitored variable has changed:")
#               message("updating")
              VALUE <<- BINDING_IFACE
              where$.hash[[watch]][[id]] <- hash_0
            }
          }
        }
        VALUE
      }
    }),
    list(
      VALUE = as.name("value"), 
      BINDING_IFACE = substitute(.binding(x = where[[watch]]))
    )
  )    
  
  return(out)
    
  }
)


