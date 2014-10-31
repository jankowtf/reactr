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
#' @param lazy \code{\link{logical}}.
#'    \code{TRUE}: lazy execution of reactive expressions/conductors set via 
#'    \code{\link[reactr]{reactiveBinding}} (i.e. `()` is necessary);
#'    \code{FALSE} eagerly execution (i.e. simply calling the object triggers 
#'    the binding function).
#' @param push \code{\link{logical}}.
#'    \code{TRUE}: immediately propagate changes to objects referencing this 
#'    object by implicitly calling/requesting them and thus executing their 
#'    binding functions (corresponds to a \strong{push paradigm});
#'    \code{FALSE}: objects referencing this object will only know of the change
#'    in this object if they are called/requested themselves as this would 
#'    then trigger the execution of their binding functions 
#'    (corresponds to a \strong{pull paradigm}).
#' @param typed \code{\link{logical}}.
#'    \code{TRUE}: checks class validity of assignment value specified via
#'    \code{value} and throws an error if classes do not match or if the class 
#'    of the assignment value does not inherit from the class of field value 
#'    \code{.value} at initialization;
#'    \code{FALSE}: no class check is performed.
#'    Note that initial values of \code{NULL} are disregarded for the class 
#'    check, i.e. any value overwriting an initial \code{NULL} value 
#'    is valid.
## @param strict \code{\link{numeric}}.
##    Relevant when initially setting a reactive object
##    \itemize{
##      \item{\code{0}: } {no checks are performed}
##      \item{\code{1}: } {warning if object is already a non-reactive or 
##      reactive object or if any references does not exist yet}
##      \item{\code{2}: } {error if object is already a non-reactive or 
##      reactive object or if any references do not exist yet}
##    }
## @param strict_get \code{\link{numeric}}.
##    Relevant if retrieving object when reactive reference has been broken
##    (i.e. one of the referenced objects does not exist anymore).
##    reactive relationship.
##    \itemize{
##      \item{\code{0}: } {return last cached value}
##      \item{\code{1}: } {object value is set to \code{NULL} and is returned}
##      \item{\code{2}: } {object value is set to an instance of condition class 
##          \code{BrokenReactiveReference} and this condition is triggered whenever
##          the object's value is requested by \code{\link[base]{get}} or 
##          its syntactical surgars \code{{obj-name} or \code{}}
##      }
##    }
#' @param strict_set \code{\link{numeric}}.
#'    Relevant if assigning an explicit value to an object with reactive 
#'    dependency on other objects.
#'    reactive relationship.
#'    \itemize{
#'      \item{\code{0}: } {ignore without warning}
#'      \item{\code{1}: } {ignore with Warning}
#'      \item{\code{2}: } {stop with error}
#'    }
## @param verbose \code{\link{logical}}.
##    \code{TRUE}: output certain status information;
##    \code{FALSE}: no status information.
#' @param ... Further arguments to be passed to subsequent functions/methods.
#'    In particular, all environments of references that you are referring to
#'    in the body of the binding function. 
#'    See section \emph{Referenced environments}.
#' @example inst/examples/setShinyReactive.r
#' @seealso \code{
#'     \link[reactr]{setReactiveS3}
#' }
#' @template author
#' @template references
#' @import conditionr
#' @import shiny
#' @export 
setShinyReactive <- function(
    id,
    value = NULL,
    where = parent.frame(),
    lazy = FALSE,
#     integrity = TRUE,
    push = FALSE,
    typed = FALSE,
#     strict = c(0, 1, 2),
#     strict_get = c(0, 1, 2),
    strict_set = c(0, 1, 2),
#     verbose = FALSE,
    ...
  ) {

  ## Argument checks //
#   strict <- as.numeric(match.arg(as.character(strict), 
#                                  as.character(c(0, 1, 2))))
#   strict_get <- as.numeric(match.arg(as.character(strict_get), 
#                                  as.character(c(0, 1, 2))))
  strict_set <- as.numeric(match.arg(as.character(strict_set), 
                                 as.character(c(0, 1, 2))))

  ## Ensure that shiny let's us do our thing //
  shiny_opt <- getOption("shiny.suppressMissingContextError")
  if (is.null(shiny_opt) || !shiny_opt) {
    options(shiny.suppressMissingContextError = TRUE)  
#     shiny:::setAutoflush(TRUE)
  }
  
  ## Check if regular value assignment or reactive function //
  if (!inherits(value, "ReactiveBinding")) {    
#       makeReactiveBinding(symbol = id, env = where)
#       visible <- assign(id, value, envir = where, inherits = FALSE)
    visible <- setReactiveSource(id = id, value = value, 
      where = where, typed = typed)
  } else {
    o <- shiny:::Observable$new(value$fun, value$label, value$domain)
    shiny:::registerDebugHook(".func", o, "Reactive")
    visible <- structure(o$getValue, observable = o, class = "reactive") 
    
    ## Call to 'makeActiveBinding' //
    makeActiveBinding(
      id,
      env = where,
      fun = local({
        visible
        o
        function(v) {
          if (missing(v)) {
#             o$.value <- v
            ## --> this should only be allowed for bi-directional relationships
          } else {
            if (strict_set == 0) {
#                 o$.value <<- v    
            } else if (strict_set == 1) {
              conditionr::signalCondition(
                call = substitute(
                  assign(x= ID, value = VALUE, envir = WHERE, inherits = FALSE),
                  list(ID = id, VALUE = v, WHERE = where)
                ),
                condition = "AbortedWithReactiveDependencyWarning",
                msg = c(
                  Reason = "trying to set value of object with reactive dependency",
                  ID = id,
#                     UID = o$.uid,
                  Location = capture.output(where)
                ),
                ns = "reactr",
                type = "warning"
              )
            } else if (strict_set == 2) {
              conditionr::signalCondition(
                call = substitute(
                  assign(x= ID, value = VALUE, envir = WHERE, inherits = FALSE),
                  list(ID = id, VALUE = v, WHERE = where)
                ),
                condition = "AbortedWithReactiveDependencyError",
                msg = c(
                  Reason = "trying to set value of object with reactive dependency",
                  ID = id,
#                     UID = o$.uid,
                  Location = capture.output(where)
                ),
                ns = "reactr",
                type = "error"
              )
            }
          }
  
          ## Return //
          if (!lazy) {
            visible()
          } else {
            visible
          }
        }
      })
    )
    if (push) {
      shiny:::setAutoflush(TRUE)
      observe(visible())
    }
  }

  invisible(visible)  
}

