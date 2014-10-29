## fix: issue #11
##      Inconsistency for initial values
## fix: issue #16 (application of strictness settings)

#' @title
#' Set Reactive Object (S3)
#'
#' @description 
#' Creates an reactive object.
#' 
#' @details
#' Implicitly, an instance of class 
#' \code{\link[reactr]{ReactiveObject.S3}} is created of which only field
#' \code{value} will be visible to the outside. The rest of the object is
#' stored in a invisible way.
#' 
#' In order to keep the invisible object accessible and also due to the 
#' implementation of the caching mechanism, the invisible object is also stored
#' in a registry (see \code{\link[reactr]{getRegistry}}).
#' 
#' @section Self-contained reactivity:
#' 
#' The package strives to make the reactive nature of reactive objects as 
#' self-contained as possible. So, ideally, there would be no need for an
#' explicit registry. In order to realize this goal at least partially, 
#' each reactive object instance of class \code{\link[reactr]{ReactiveObject.S3}}
#' also hold \strong{references} to the respective objects in the registry.
#' As instances of class \code{\link[reactr]{ReactiveObject.S3}} are eventually
#' nothing but objects of class \code{\link{environment}}, the reference 
#' should mostly have a \emph{pass-by-reference} nature and thus lead to 
#' minimal overhead only. 
#' 
#' However, \strong{removing} or \strong{reassigning} (as opposed to merely
#' \strong{altering} an instance from the \strong{registry}
#' does \strong{not} change the referenced environment in the \strong{actual instances} 
#' itself (see illustration \emph{"References to environment"} in examples).
#' This is a potential risk for inconsistencies and thus class 
#' \code{\link[reactr]{ReactiveObject.S3}} offers a method that takes care 
#' that the registry instance references are equal to the actual registry 
#' instances. This comes at the cost of a minimal overhead of about 2.3e-08
#' seconds for each request of an reactive object. 
#' 
#' @section Referenced environments:
#' 
#' When referencing objects from environments other than the environment that
#' is assigned to argument \code{where}, it is recommended to provide those
#' environments explicitly as additional arguments. E.g., when using 
#' object \code{x_1} from environment \code{where_1}, then include \code{where_1}
#' as additional argument in the call to \code{\link[reactr]{setReactiveS3}}. 
#' This might not always be necessary due to the way lexical scoping works, but
#' it is probably generally a good idea.
#' 
#' @param id \code{\link{character}}.
#'    Name of the object to set.
#' @param value \code{\link{ANY}}.
#'    Variable value or binding.
#' @param where \code{\link{environment}}.
#'    Environment to create the object in.
#' @param cache \code{\link{logical}}.
#'    \code{TRUE}: use caching mechanism;
#'    \code{FALSE}: no caching mechanism used.
#'    Note that features \emph{bi-directional bindings} and 
#'    \emph{pushing changes} are only supported if \code{cache = TRUE}.
#' @param integrity \code{\link{logical}}.
#'    \code{TRUE}: ensures structural integrity of underlying reactive object
#'    (instance of class \code{\link[reactr]{ReactiveObject.S3}}).
#'    \code{FALSE}: no integrity measures are carried out.
#'    Note that \code{TRUE} adds a minimal overhead of 2.3e-08 seconds 
#'    to the runtime. See details.
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
#'    Note that initial values of \code{NULL} are disregarded, i.e. each value
#'    will be a valid value for overwriting \code{NULL} values in \code{.value}.
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
#'      \item{\code{0}: } {ignore without warning}
#'      \item{\code{1}: } {ignore with Warning}
#'      \item{\code{2}: } {stop with error}
#'    }
#' @param verbose \code{\link{logical}}.
#'    \code{TRUE}: output certain status information;
#'    \code{FALSE}: no status information.
#' @param ... Further arguments to be passed to subsequent functions/methods.
#'    In particular, all environments of references that you are referring to
#'    in the body of the binding function. See section \emph{Referenced environments}.
#' @example inst/examples/setReactiveS3.r
#' @seealso \code{
#'   	\link[reactr]{setReactiveS3}
#' }
#' @template author
#' @template references
#' @import conditionr
#' @import shiny
#' @import yamlr
#' @export 
setReactiveS3 <- function(
    id,
    value = NULL,
    where = parent.frame(),
    cache = TRUE,
    integrity = TRUE,
    push = FALSE,
    typed = FALSE,
    strict = c(0, 1, 2),
    strict_get = c(0, 1, 2),
    strict_set = c(0, 1, 2),
    verbose = FALSE,
    .debug = FALSE,
    ...
  ) {
  
  ## Argument checks //
  strict <- as.numeric(match.arg(as.character(strict), 
                                 as.character(c(0, 1, 2))))
  strict_get <- as.numeric(match.arg(as.character(strict_get), 
                                 as.character(c(0, 1, 2))))
  strict_set <- as.numeric(match.arg(as.character(strict_set), 
                                 as.character(c(0, 1, 2))))
  
  ## Check if regular value assignment or reactive function //
  if (!is.function(value)) {    
    is_reactive <- FALSE
    pull_refs <- character()
    
#     value_initial <- value
#     value_expr <- quote(obj$.value <<- value)
    value_expr <- NULL
    func <- NULL
  } else {
    is_reactive <- TRUE
    
    ##--------------------------------------------------------------------------
    ## Recognition of references //
    ##--------------------------------------------------------------------------
    ## In order of preferred specification in 'value'
    
# where =environment()

    ## 1) Recognize via markup in comments //
    yaml <- yamlr::processYaml(
      from = value, 
      ctx = yamlr::YamlContext.ObjectReference.S3(),
      where = where
    )
    if (length(yaml$original)) { 
      refs <- yaml
#       pull_refs <- unname(sapply(names(yaml$parsed), function(ii) {
#         computeObjectUid(id = ii, where = eval(yaml$parsed[[ii]]$where))
#       }))
      pull_refs <- lapply(yaml$parsed, function(ii) {
        ii$uid <- computeObjectUid(id = ii$id, where = eval(ii$where))
        ii
      })
      value <- yaml$src
      if (.debug) {
        message("Updated binding function:")
        print(value)
      }
    } else {
      refs <- NULL
    }

    ## 2) Recognize via argument 'refs' //
    if (is.null(refs)) {
      refs <- .getReferencesFromArguments(
        arguments = formals(value), 
        where = where
      )
      if (!is.null(refs)) {
        value <- .transformReactiveFunction(refs = refs, fun = value)
        if (.debug) {
          message("Updated binding function:")
          print(value)
        }
#         pull_refs <- unname(sapply(names(refs), function(ii) {
#           computeObjectUid(id = ii, where = eval(refs[[ii]]$where))
#         }))
        pull_refs <- refs
      } 
    }

    ## 3) Recognize via '.ref_*' specification in body //
    if (is.null(refs)) {
#       pull_refs <- as.character(unlist(.getReferences(expr = body(value))))
#       refs <- .getReferences(expr = body(value), where = where)
      res <- .getReferencesFromBody2(fun = value, where = where)
      refs <- res$refs
      value <- res$fun
      if (.debug) {
        message("Updated binding function:")
        print(value)
      }
      pull_refs <- .getActualReferencesFromBody(refs = refs, where = where)
#       pull_refs <- .getActualReferencesFromBody(refs = refs)
#       pull_refs <- "abcd"
    }

#     value_initial <- tryCatch(value(), error = function(cond) NULL)
    value_expr <- quote(obj$.value <<- value())
    func <- value
  }

  ## Clean up //
#   rmReactive(id = id, where = where)

  ## Instance of class 'ReactiveObject.S3' //
  obj <- reactr::ReactiveObject.S3(
    id = id,
#     value = value_initial,
    value = if (!is_reactive) value,
    where = where,
    ## Alphabetically //
    cache = cache,
    exists_visible = TRUE,
    func = func,
    has_cached = FALSE,
    pull_refs_list = pull_refs
  )

  ## Push //
  if (cache && push) {
    obj$.registerPushReferences()
  }

  ## Check prerequisites //
  checkReactivityPrerequisites(input = obj, strict = strict)

  ## Call to 'makeActiveBinding()' //
  expr <- substitute(
    makeActiveBinding(
      id,
      local({
        OBJ
        function(v) {
          if (missing(v)) {
            
          ##--------------------------------------------------------------------
          ## Handler for 'get' (i.e. 'get()' or '{obj-name}' or '${obj-name}) //
          ##--------------------------------------------------------------------            

            if (cache) {
              ## Process references //
              if (obj$.hasPullReferences()) {
                ## Update decision //
                ## Compare checksum values of all references with the ones in 
                ## own cache; 
                ## - if any has changed --> update
                ## - if not --> return cached value
                do_update <- sapply(pull_refs, function(ref_uid) {
                  ref_uid <- ref_uid$uid
  #                 do_update <- FALSE
                  
                  ## Ensure integrity //
                  if (integrity) {
                    obj$.ensurePullReferencesIntegrity(ref_uid = ref_uid)
                  }
                  ## Compare checksums //
                  do_update <- obj$.compareChecksums(
                    ref_uid = ref_uid, 
                    strict_get = strict_get,
                    verbose = verbose
                  )
                  return(do_update)
                })
                ## Handle scope of update cycle //
                if (do_update && obj$.blockUpdate(verbose = verbose)) {
                  do_update <- FALSE
                }
                if (obj$.needs_update) {
                  do_update <- TRUE
                }
              } else {
#                 do_update <- FALSE
                do_update <- obj$.needs_update
              }
            } else {
              do_update <- TRUE
            }

            ##----------------------------------------------------------------
            ## Actual update //
            ##----------------------------------------------------------------

            if (is_reactive && (any(do_update) || !obj$.has_cached)) {
              if (verbose) {
                if (!obj$.has_cached) {
                  message("Initializing ...")  
                }
                if (any(do_update)) {
                  message("Updating ...")  
                }
              }
              
              ## Cache new value //
              obj$.value <<- withRestarts(
                tryCatch(
                  {
                    out <- value()
                    
                    ## Object state updates //
                    obj$.condition <- NULL
                    obj$.has_cached <- TRUE
                    obj$.needs_update <- FALSE
                    
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
                  signal <- FALSE
                  if (!cache) { 
                    if (grepl("object.*not found", conditionMessage(cond))) {
                      msg <- paste0("caching disabled -->", conditionMessage(cond))
                    } else if (grepl("evaluation nested too deeply.*infinite recursion", conditionMessage(cond))) {                
                      message(conditionMessage(cond))
                      msg <- "caching disabled --> infinite recursion"
                      signal <- TRUE
                    }
                  } else {
                    msg <- conditionMessage(cond)
                  }
                  cond <- conditionr::signalCondition(
                    call = substitute(
                      get(x= ID, envir = WHERE, inherits = FALSE),
                      list(ID = obj$.id, WHERE = obj$.where)
                    ),
                    condition = "AbortedReactiveUpdateWithError",
                    msg = c(
                      "Update failed",
                      Reason = msg,
                      ID = obj$.id,
                      UID = obj$.uid,
                      Location = capture.output(where)
                    ),
                    ns = "reactr",
                    type = "error",
                    signal = signal
                  )
                  
                  ## Transfer condition //
                  obj$.condition <<- cond
                  NULL
                }
              )

              ## Update fields //
              obj$.computeChecksum()
            }
            
            ## Object statue updates //
            obj$.caller <- obj
            ## --> after an calling cycle is complete, the caller field
            ## can be reset so that for "self-requests" everything is handled 
            ## appropriately.
            obj$.is_modcycle_complete <- TRUE
          } else {
          
          ##--------------------------------------------------------------------
          ## Handler for 'set' (i.e. 'assign()' or '<-') //
          ##--------------------------------------------------------------------            
            
            ## Class check //
            if (typed) {
              obj$.checkClass(v = v)
            }
          
            ## Set //
            if (obj$.hasPullReferences()) {
              if (strict_set == 0) {
                obj$.value <<- v    
                if (!obj$.has_bidir) {
                  obj$.needs_update <- TRUE
                }
                obj$.is_modcycle_complete <- FALSE
              } else if (strict_set == 1) {
                conditionr::signalCondition(
                  call = substitute(
                    assign(x= ID, value = VALUE, envir = WHERE, inherits = FALSE),
                    list(ID = obj$.id, VALUE = v, WHERE = obj$.where)
                  ),
                  condition = "AbortedWithReactiveDependencyWarning",
                  msg = c(
                    Reason = "trying to set value of object with reactive dependency",
                    ID = obj$.id,
                    UID = obj$.uid,
                    Location = capture.output(where),
                    References = paste(ls(obj$.refs_pull, all.names = TRUE), collapse = ", ")
                  ),
                  ns = "reactr",
                  type = "warning"
                )
              } else if (strict_set == 2) {
                conditionr::signalCondition(
                  call = substitute(
                    assign(x= ID, value = VALUE, envir = WHERE, inherits = FALSE),
                    list(ID = obj$.id, VALUE = v, WHERE = obj$.where)
                  ),
                  condition = "AbortedWithReactiveDependencyError",
                  msg = c(
                    Reason = "trying to set value of object with reactive dependency",
                    ID = obj$.id,
                    UID = obj$.uid,
                    Location = capture.output(where),
                    References = paste(ls(obj$.refs_pull, all.names = TRUE), collapse = ", ")
                  ),
                  ns = "reactr",
                  type = "error"
                )
              }
            } else {
              obj$.value <<- v 
            }
            
            ## Update checksum //
#             assign(obj$.uid, digest::digest(v), envir = obj$.registry[[obj$.uid]])   
            obj$.computeChecksum()

            ## Push //
            if (  cache &&
                  obj$.must_push &&
                  obj$.hasPushReferences() && 
                  !obj$.has_pushed && 
                  !obj$.is_running_push
            ) {
              obj$.pushToReferences(verbose = verbose)
              ## Reset value of push control field //
              obj$.has_pushed <- FALSE
            }

          }

          ##--------------------------------------------------------------------
          ## Return //
          ##--------------------------------------------------------------------

          ## Condition handling //
          if (!is.null(obj$.condition)) {           
            if (inherits(obj$.condition, "BrokenReactiveReference")) {
              obj$.value <- stop(obj$.condition)
            } else {            
              obj$.value <- stop(obj$.condition)
            }
          }

          ## Return value //
          if (.debug) {
            obj
          } else {
            obj$.value
          }
        }
      }),
      env = where
    ),
    list(OBJ = obj)
  )
  if (.debug) {
    message("Expression for setting the active binding:")
    print(expr)
  }
  eval(expr)

  ## Initialize and return value //
  if (is_reactive) {
    out <- get(id, envir = where, inherits = FALSE)
#     out <- obj$.value
  } else {
    out <- value
  }
  
  invisible(out)
  
}
