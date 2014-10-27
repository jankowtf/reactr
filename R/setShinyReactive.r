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
#' @param strict \code{\link{numeric}}.
#'    Relevant when initially setting a reactive object
#'    \itemize{
#'      \item{\code{0}: } {no checks are performed}
#'      \item{\code{1}: } {warning if object is already a non-reactive or 
#'      reactive object or if any references does not exist yet}
#'      \item{\code{2}: } {error if object is already a non-reactive or 
#'      reactive object or if any references do not exist yet}
#'    }
#' @param strict_get \code{\link{numeric}}.
#'    Relevant if retrieving object when reactive reference has been broken
#'    (i.e. one of the referenced objects does not exist anymore).
#'    reactive relationship.
#'    \itemize{
#'      \item{\code{0}: } {return last cached value}
#'      \item{\code{1}: } {object value is set to \code{NULL} and is returned}
#'      \item{\code{2}: } {object value is set to an instance of condition class 
#'          \code{BrokenReactiveReference} and this condition is triggered whenever
#'          the object's value is requested by \code{\link[base]{get}} or 
#'          its syntactical surgars \code{{obj-name} or \code{}}
#'      }
#'    }
#' @param strict_set \code{\link{numeric}}.
#'    Relevant if assigning an explicit value to an object with reactive 
#'    dependency on other objects.
#'    reactive relationship.
#'    \itemize{
#'      \item{\code{0}: } {
#'      
#'      Value that is actually determined by reactive binding is 
#'      overwritten and cached until on of the referenced objects is updated
#'      again (which then also triggers an updated of the object and thus a
#'      complete restoring of the reactive relationship)
#'      }
#'      \item{\code{1}: } {ignore without warning}
#'      \item{\code{2}: } {ignore with Warning}
#'      \item{\code{3}: } {stop with error}
#'    }
#' @param verbose \code{\link{logical}}.
#'    \code{TRUE}: output certain status information;
#'    \code{FALSE}: no status information.
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
    strict = c(0, 1, 2),
    strict_get = c(0, 1, 2),
    strict_set = c(0, 1, 2, 3),
    verbose = FALSE,
    ...
  ) {

  ## Argument checks //
  strict <- as.numeric(match.arg(as.character(strict), 
                                 as.character(c(0, 1, 2))))
  strict_get <- as.numeric(match.arg(as.character(strict_get), 
                                 as.character(c(0, 1, 2))))
  strict_set <- as.numeric(match.arg(as.character(strict_set), 
                                 as.character(c(0, 1, 2, 3))))
  
  ## Ensure that shiny let's us do this //
  shiny_opt <- getOption("shiny.suppressMissingContextError")
  if (is.null(shiny_opt) || !shiny_opt) {
    options(shiny.suppressMissingContextError = TRUE)  
  }

  ## Check if regular value assignment or reactive function //
  if (!is.function(value)) {    
    is_reactive <- FALSE
    refs_pull <- character()
    
#     value_initial <- value
#     value_expr <- quote(obj$value <<- value)
    value_expr <- NULL
    func <- NULL
  } else {
    is_reactive <- TRUE

    yaml <- exprToFunction2(expr = value, env = where, quoted = quoted)
    func <- yaml$src
    
    # Attach a label and a reference to the original user source for debugging
    if (is.null(label))
      label <- sprintf('reactive(%s)', paste(deparse(body(func)), collapse='\n'))
    srcref <- attr(substitute(x), "srcref")
    if (length(srcref) >= 2) attr(label, "srcref") <- srcref[[2]]
    attr(label, "srcfile") <- shiny:::srcFileOfRef(srcref[[1]])
    
    refs_pull <- yaml$parsed
# print(refs_pull)    
  }

  o <- Observable3$new(
    id = id, 
    value = if (!is_reactive) value,
    where = where,
    refs_pull = refs_pull,
    func = func, 
    label = label, 
    domain = domain
  )
# print(ls(o$.refs_pull))
  if (is_reactive) {
    shiny:::registerDebugHook(".func", o, "Reactive")
  }

  ## Class //
  o$.class <- class(o$.value)
  ## Register //
  o$.register(overwrite = TRUE)
  ## Check prerequisites //
  checkReactivityPrerequisites(input = o, strict = strict)
  ## Push //
  if (push) {
    o$.registerPushReferences()
  }

  ## Call to 'makeActiveBinding' //
#   expr <- substitute(
#     makeActiveBinding(
#       id,
#       env = WHERE,
#       local({
# #         func <- VALUE
# #         value <- VALUE
# #         VALUE
#         value
# #         this
#         function(v) {
#           if (!missing(v)) {
#               value <<- v
# #               this <<- v
#           } else {
#               VALUE_EXPR
#           }
#           value
# #           this
#         }
#       })
#     ),
#     list(
# #       VALUE = if (!is_reactive) quote(value <- value) else quote(value),
# #       VALUE = if (!is_reactive) quote(value) else x_2(),
#       VALUE_EXPR = value_expr,
#       WHERE = where
#      )
#   )
#   eval(expr)

################################################################################

  makeActiveBinding(
    id,
    env = where,
    fun = local({
      o
#       O
      function(v) {
        if (missing(v)) {
          
          ##--------------------------------------------------------------------
          ## Handler for 'get' (i.e. 'get()' or '{obj-name}' or '${obj-name}) //
          ##--------------------------------------------------------------------   
          
          if (o$.hasPullReferences()) {
# print(ls(o$.refs_pull))            
            needs_update <- sapply(ls(o$.refs_pull), function(ref_uid) {
# message("ref_uid:")
# print(ref_uid)
              ## Ensure integrity //
              if (integrity) {
                o$.ensurePullReferencesIntegrity(ref_uid = ref_uid)
              }
              ## Compare checksums //
              (needs_update <- o$.compareChecksums(
                ref_uid = ref_uid, 
                strict_get = strict_get,
                verbose = verbose
              ))
            })
          } else {
            needs_update <- FALSE
          }
        
          ##----------------------------------------------------------------
          ## Actual update or initial caching //
          ##----------------------------------------------------------------
# message("needs_update:")
# print(needs_update)  
          if (is_reactive && (any(needs_update) || !o$.has_cached)) {
            if (verbose) {
              if (!o$.has_cached) {
                message("Initializing ...")  
              }
              if (any(needs_update)) {
                message("Updating ...")  
              }
            }
            
            ## Cache new value //
# message(".has_cached:")
# print(o$.has_cached)            
            o$.value <<- withRestarts(
              tryCatch(
                {     
                  ## Handle predence of cached values vs. calling binding
                  ## functions in situations where bi-directional 
                  ## relationships exist, the value of one object has 
                  ## been **explicitly** set but then the **other** object
                  ## was called before the object itself was called.
                  ## Withough this `.force_cached` mechanism, the explicit
                  ## setting would be overwritten as the request of the
                  ## other object would trigger the binding function of 
                  ## this object and thus the explicit change would be lost
                  if (o$.has_cached && o$.force_cached) {
                    out <- o$.value
                  } else {                  
                    if (o$.invalidated) {                      
                      out <- o$getValue()
                    } else {
                      out <- o$.updateValue()
                    }
## TODO: issue #20        
                    o$.force_cached <- FALSE
                  }
                  
                  
                  o$.condition <<- NULL
                  o$.has_cached <<- TRUE
                  out 
                ## For debugging/testing purposes 
  #                     stop("Intentional update fail"),
                },
                warning = function(cond) {
                  invokeRestart("muffleWarning")
                },
                error = function(cond) {
                  invokeRestart("ReactiveUpdateFailed", cond = cond)
                }
              ),
              muffleWarning = function(cond) {
                message(cond)
                invokeRestart("muffleWarning")
              },
              ReactiveUpdateFailed = function(cond) {
                ## Custom condition //
                cond <- conditionr::signalCondition(
                  call = substitute(
                      get(x= ID, envir = WHERE, inherits = FALSE),
                      list(ID = o$.id, WHERE = o$.where)
                    ),
                  condition = "AbortedReactiveUpdateWithError",
                  msg = c(
                    "Update failed",
                    Reason = conditionMessage(cond),
                    ID = o$.id,
                    UID = o$.uid,
                    Location = capture.output(o$.where)
                  ),
                  ns = "reactr",
                  type = "error",
                  signal = FALSE
                )
                ## Transfer condition //
                o$.condition <<- cond
                NULL
              }
            )
            ## Update fields //
            o$.computeChecksum()
          }
        } else {
        
          ##--------------------------------------------------------------------
          ## Handler for 'set' (i.e. 'assign()' or '<-') //
          ##--------------------------------------------------------------------   
          
          if (typed) {
            o$.checkClass(v = v)
          }
          
          ## Set //
          if (o$.hasPullReferences()) {
            if (strict_set == 0) {
              o$.value <<- v    
              o$.force_cached <- TRUE
            } else if (strict_set == 1) {
              ## Do nothing //
            } else if (strict_set == 2) {
              conditionr::signalCondition(
                call = substitute(
                  assign(x= ID, value = VALUE, envir = WHERE, inherits = FALSE),
                  list(ID = o$.id, VALUE = v, WHERE = o$.where)
                ),
                condition = "AbortedWithReactiveDependencyWarning",
                msg = c(
                  Reason = "trying to set value of object with reactive dependency",
                  ID = o$.id,
                  UID = o$.uid,
                  Location = capture.output(o$.where),
                  References = paste(ls(o$.refs_pull, all.names = TRUE), collapse = ", ")
                ),
                ns = "reactr",
                type = "warning"
              )
            } else if (strict_set == 3) {
              conditionr::signalCondition(
                call = substitute(
                  assign(x= ID, value = VALUE, envir = WHERE, inherits = FALSE),
                  list(ID = o$.id, VALUE = v, WHERE = o$.where)
                ),
                condition = "AbortedWithReactiveDependencyError",
                msg = c(
                  Reason = "trying to set value of object with reactive dependency",
                  ID = o$.id,
                  UID = o$.uid,
                  Location = capture.output(o$.where),
                  References = paste(ls(o$.refs_pull, all.names = TRUE), collapse = ", ")
                ),
                ns = "reactr",
                type = "error"
              )
            }
          } else {
            o$.value <<- v 
          }
          
          ## Update checksum //
          o$.computeChecksum()
          
          ## Push //
          if (  o$.must_push &&
                o$.hasPushReferences() && 
                !o$.has_pushed && 
                !o$.is_running_push
          ) {
            o$.pushToReferences(verbose = verbose)
            ## Reset value of push control field //
            o$.has_pushed <- FALSE
          }
        }

        ##----------------------------------------------------------------------
        ## Return //
        ##----------------------------------------------------------------------
  
        ## Condition handling //
        if (!is.null(o$.condition)) {           
          if (inherits(o$.condition, "BrokenReactiveReference")) {
            o$.value <- stop(o$.condition)
          } else {            
            o$.value <- stop(o$.condition)
          }
        }
# message("o$.value:")
# print(o$.value)
        o$.value
      }
    })
  )
#   eval(expr)

#   structure(o$getValue, observable = o, class = "reactive2")
#   structure(o$.cache, observable = o, class = "reactive2")
#   return(o$.value)

  ## Initialize //
#   (out <- get(id, envir = env, inherits = FALSE))
  


################################################################################

  ## Return value //
  if (is_reactive) {
    out <- get(id, envir = where, inherits = FALSE)
  } else {
    out <- value
  }
 
  invisible(out)
  
}

