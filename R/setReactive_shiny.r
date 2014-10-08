#' @title
#' Set Reactive Object (shiny)
#'
#' @description 
#' Creates an reactive object.
#' 
#' @param id \code{\link{character}}.
#'    Name of the object to set.
#' @param value \code{\link{ANY}}.
#'    Variable value or binding.
#' @param where \code{\link{environment}}.
#'    Environment to create the object in.
#' @template threedot
#' @example inst/examples/setReactive_shiny.r
#' @seealso \code{
#'   	\link[reactr]{setReactive-character-ANY-environment-character-call-method}
#' }
#' @template author
#' @template references
#' @export 
#' @import shiny
setReactive_shiny <- function(
    id,
    value = NULL,
    where = .GlobalEnv,
    ...
  ) {

  ## Ensure shiny let's us do this //
  shiny_opt <- getOption("shiny.suppressMissingContextError")
  if (is.null(shiny_opt) || !shiny_opt) {
    options(shiny.suppressMissingContextError = TRUE)  
  }
  
  ## Check if regular value assignment or reactive function //
  if (!inherits(value, "reactive")) {
    is_reactive <- FALSE
    shiny::makeReactiveBinding(symbol = id, env = where)
#       assign(id, value, where)
    value_expr <- substitute(VALUE, list(VALUE = value))
  } else {
    is_reactive <- TRUE
    ## Putting together the "line of lines" //
    ## Approach 1:
#     value_expr <- substitute(value <<- VALUE(), list(VALUE = value))
    ## --> works initially but seems to be static
    ## --> seems like the call to 'local()' needs to contain the *actual*
    ## "literate" expression (i.e. 'reactive(...)'). Evaluation the line above 
    ## results in the reactive object "behind" 'reactive(()' to be assigned
    ## and that seems to make it static.
  
    ## Workarounds based character strings and re-parsing //
    ## Approach 2: via 'capture.output()'
    ## W/o 'where' //
#     reactive_expr <- capture.output(value)
    ## With 'where' //
#     reactive_expr <- gsub(") $", ", env = where)", capture.output(value))

    ## Approach 3: via attributes
    ## --> about 40 times faster than approach 2
    reactive_expr <- attributes(value)$observable$.label
    value_expr <- substitute(value <<- eval(VALUE)(), 
      list(VALUE = parse(text = reactive_expr)))
  }

  ## Call to 'makeActiveBinding' //
  expr <- substitute(
    makeActiveBinding(
      id,
      local({
        value <- VALUE
        function(v) {
          if (!missing(v)) {
              value <<- v
          } else {
              VALUE_EXPR
          }
          value
        }
      }),
      env = where
    ),
    list(
      VALUE = value,
      VALUE_EXPR = value_expr
     )
  )
  eval(expr)

  ## Return value //
  if (is_reactive) {
#     out <- get(id, envir = where, inherits = FALSE)
    ## Try mutual //
    out <- try(get(id, envir = where, inherits = FALSE), silent = TRUE)
    if (inherits(out, "try-error")) {
      out <- NULL
    }
  } else {
    out <- value
  }
  
  return(out)
  
}
