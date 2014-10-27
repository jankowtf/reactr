#' @title
#' Set Reactive Object with Shiny Functionality (S3)
#'
#' @description 
#' Creates an reactive object as the ones created by the 
#' \href{shiny}{http://shiny.rstudio.com/} framework/package.
#' 
#' @details
#' The function makes explicit use of \code{\link[shiny]{makeReactiveBinding}}
#' and \code{\link[shiny]{reactive}}. This implies, that the entire reactive 
#' paradigm underlying the shiny framework is also used. 
#' For the most relevant aspects of this see:
#' 
#' \itemize{
#'    \item{Creating an object that can have reactive bindings: } {
#'        \itemize{
#'          \item{Function \code{\link[shiny]{reactiveValues}}}
#'          \item{Function \code{\link[shiny]{.createReactiveValues}}}
#'          \item{R6 class \code{\link[shiny]{ReactiveValues}}}
#'        }
#'    }
#'    \item{Creating an object that has reactive bindings: } {
#'        \itemize{
#'          \item{Function \code{\link[shiny]{reactive}}}
#'          \item{R6 class \code{\link[shiny]{Observable}}}
#'          \item{R6 class \code{\link[shiny]{Map}}}
#'        }
#'    }   
#' }
#' 
#' Note that the function creates the object with name \code{id} in environment
#' \code{where}. So you don't explicitly need to assign
#' the return value to \code{id}. Of course you can also do so as well.
#' 
#' @section Remarks with respect to mutual reactive bindings:
#' 
#' To the best of my knowledge, the reactive paradigm implemented by the 
#' shiny framework does not offer the possibility to define mutual reactive 
#' bindings. 
#' 
#' Thus, something like \code{x_1} has reactive binding \code{reactive{x_2 * 2}} 
#' and \code{x_2} has reactive binding \code{reactive{x_1 / 2}} where \strong{both} objects can be 
#' modified via \code{\link{<-}} can not be specified. The reason for this is
#' that reactivity is implemented in a direct or immediate manner: whenever 
#' \code{x_1} that has a reactive binding to \code{x_2} is requested, it runs
#' its reactive binding function even though \code{x_2} might not have changed
#' at all. Thus, mutual reactive bindings of the above form result in an 
#' infinite recursion. 
#' 
#' If you would like to define mutual reactive bindings, you currently need to 
#' use \code{\link[reactr]{setReactiveS3}} as it implements a value caching 
#' mechanism that allows reactive functions only to be triggered when actually
#' needed, i.e. when the referenced object has actually changed.
#' 
#' @section Outlook with respect to the integration of shiny functionality:
#' 
#' Currently, at the end of the day the function does little more than 
#' providing a wrapper for \code{\link[base]{makeActiveBinding}} to the 
#' functionality offered by shiny. As shiny itself implements sort of the 
#' reactive version of \code{\link[base]{makeActiveBinding}}, 
#' \code{\link[shiny]{makeReactiveBinding}} already, it is very likely that 
#' these two approaches can and will be merged in future releases.
#' 
#' Also, adding a similar caching mechansims as the one implemented by 
#' \code{\link[reactr]{setReactiveS3}} seems possible.
#' 
#' @param id \code{\link{character}}.
#'    Name/ID of the reactive object to set.
#' @param value \code{\link{ANY}}.
#'    Value or reactive binding.
#' @param where \code{\link{environment}}.
#'    Environment in which to create the object.
#' @template threedots
#' @example inst/examples/setShinyReactive.r
#' @seealso \code{
#'     \link[reactr]{setReactiveS3}
#' }
#' @template author
#' @template references
#' @export 
#' @import shiny
setShinyReactive <- function(
    id,
    value = NULL,
    where = parent.frame(),
    ## from `reactive()` //
    quoted = FALSE, 
    label = NULL,
    domain = shiny:::getDefaultReactiveDomain(), 
    integrity = TRUE,
    push = FALSE,
    typed = FALSE,
    ...
  ) {

  ## Ensure that shiny let's us do this //
  shiny_opt <- getOption("shiny.suppressMissingContextError")
  if (is.null(shiny_opt) || !shiny_opt) {
    options(shiny.suppressMissingContextError = TRUE)  
  }
  
  ## Check if regular value assignment or reactive function //
  if (!is.function(value)) {    
    is_reactive <- FALSE
    references <- character()
    
#     value_initial <- value
#     value_expr <- quote(obj$value <<- value)
    value_expr <- NULL
    func <- NULL
  } else {
    is_reactive <- TRUE

    yaml <- exprToFunction2(x, env, quoted)
    fun <- yaml$src
    # Attach a label and a reference to the original user source for debugging
    if (is.null(label))
      label <- sprintf('reactive(%s)', paste(deparse(body(fun)), collapse='\n'))
    srcref <- attr(substitute(x), "srcref")
    if (length(srcref) >= 2) attr(label, "srcref") <- srcref[[2]]
    attr(label, "srcfile") <- shiny:::srcFileOfRef(srcref[[1]])
    o <- Observable3$new(
      id = id, 
      where = env,
      refs_pull = yaml$parsed,
      func = fun, 
      label = label, 
      domain = domain
    )
# o <<- o
    shiny:::registerDebugHook(".func", o, "Reactive")
  
    ## Some preparations //
    o$.class <- class(o$.value)
  
    ## Push //
    if (push) {
      o$.registerPushReferences()
    }

  }

  ## Call to 'makeActiveBinding' //
  expr <- substitute(
    makeActiveBinding(
      id,
      env = WHERE,
      local({
#         func <- VALUE
#         value <- VALUE
#         VALUE
        value
#         this
        function(v) {
          if (!missing(v)) {
              value <<- v
#               this <<- v
          } else {
              VALUE_EXPR
          }
          value
#           this
        }
      })
    ),
    list(
#       VALUE = if (!is_reactive) quote(value <- value) else quote(value),
#       VALUE = if (!is_reactive) quote(value) else x_2(),
      VALUE_EXPR = value_expr,
      WHERE = where
     )
  )
# print(expr)
# print(ls(where))
  eval(expr)

  ## Return value //
  if (is_reactive) {
    out <- get(id, envir = where, inherits = FALSE)
  } else {
    out <- value
  }
 
  return(out)
  
}

