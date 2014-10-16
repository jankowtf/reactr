## fix: github #11
##      Inconsistency for initial values

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
#' stored in a hidden way.
#' 
#' In order to keep the hidden object accessible and also due to the 
#' implementation of the caching mechanism, the hidden object is also stored
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
#' @param id \code{\link{character}}.
#'    Name of the object to set.
#' @param value \code{\link{ANY}}.
#'    Variable value or binding.
#' @param where \code{\link{environment}}.
#'    Environment to create the object in.
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
#' @param strict \code{\link{numeric}}.
#'    Relevant when initially setting a reactive object
#'    \itemize{
#'      \item{\code{0}: } {no checks are performed}
#'      \item{\code{1}: } {warning if object is already a non-reactive or 
#'      reactive object}
#'      \item{\code{2}: } {error if object is already a non-reactive or 
#'      reactive object}
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
#' @template threedots
#' @example inst/examples/setReactiveS3.r
#' @seealso \code{
#'   	\link[reactr]{setReactiveS3}
#' }
#' @template author
#' @template references
#' @export 
#' @import shiny
#' @import yamlr
setReactiveS3 <- function(
    id,
    value = NULL,
    where = parent.frame(),
    integrity = TRUE,
    push = FALSE,
    strict = 0,
    strict_get = 0,
    strict_set = 0,
    .debug = FALSE,
    ...
  ) {
  
  ## Argument checks //
  strict <- as.numeric(match.arg(as.character(strict), 
                                 as.character(c(0, 1, 2))))
  strict_get <- as.numeric(match.arg(as.character(strict_get), 
                                 as.character(c(0, 1, 2))))
  strict_set <- as.numeric(match.arg(as.character(strict_set), 
                                 as.character(c(0, 1, 2, 3))))

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
      references <- unname(sapply(names(yaml$parsed), function(ii) {
        getObjectUid(id = ii, where = eval(yaml$parsed[[ii]]$where))
      }))
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
        ii="x_1"
        references <- unname(sapply(names(refs), function(ii) {
          getObjectUid(id = ii, where = eval(refs[[ii]]$where))
        }))
      } 
    }

    ## 3) Recognize via '.ref_*' specification in body //
    if (is.null(refs)) {
#       references <- as.character(unlist(.getReferences(expr = body(value))))
#       refs <- .getReferences(expr = body(value), where = where)
      res <- .getReferencesFromBody2(fun = value, where = where)
      refs <- res$refs
      value <- res$fun
      if (.debug) {
        message("Updated binding function:")
        print(value)
      }
      references <- .getActualReferencesFromBody(refs = refs, where = where)
#       references <- "abcd"
    }

#     value_initial <- tryCatch(value(), error = function(cond) NULL)
    value_expr <- quote(obj$value <<- value())
    func <- value
  }

  ## Instance of class 'ReactiveObject.S3' //
  obj <- reactr::ReactiveObject.S3(
    id = id,
#     value = value_initial,
    value = if (!is_reactive) value,
    where = where,
    .references = references,
    has_cached = FALSE,
    func = func,
    exists_visible = TRUE
  )
#   reg_res <- obj$register()
  reg_res <- obj$register(overwrite = TRUE)

  ## No self-references //
#   if (any(references == obj$uid) && !reg_res) {
  if (any(references == obj$uid)) {    
    conditionr::signalCondition(
      condition = "NoSelfReferenceAllowed",
      msg = c(
        Reason = "tried to set a self-reference",
        ID = obj$id,
        UID = obj$uid,
        Location = capture.output(where)
      ),
      ns = "reactr",
      type = "error"
    )
  }

  ## Push //
# push <- FALSE
  if (push && length(references)) {
    sapply(references, function(ref_uid) {
# print(ref_uid)
# print(obj$references_pull[[ref_uid]])
# print(ls(obj$references_pull[[ref_uid]]))
      if (!exists(obj$uid, envir = obj$references_pull[[ref_uid]]$references_push)) {
      ## Ensure a push reference is created //
        assign(obj$uid, obj, obj$references_pull[[ref_uid]]$references_push)
        obj$references_pull[[ref_uid]]$has_push_refs <- TRUE
        TRUE
      } else {
        FALSE
      }
    })        
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
# print(references)            
            ## Process references //
            if (length(references)) {
              ## Update decision //
              ## Compare checksum values of all references with the ones in 
              ## own cache; 
              ## - if any has changed --> update
              ## - if not --> return cached value
              do_update <- sapply(references, function(ref_uid) {
                do_update <- FALSE
                
                ## Ensure integrity //
                if (integrity) {
                  obj$ensureIntegrity(ref_uid = ref_uid)
                }
                
                ## Handle invalidation of referenced objects //
                ## Ensures that "in-object" reference (as compared to the 
                ## "in-registry" reference) is updated once a reference has 
                ## become invalid
# print(ref_uid)
# print(obj$references)
# print(ls(obj$references))
# print(ls(obj$references[[ref_uid]]))
                if (  is.null(obj$references_pull[[ref_uid]]) ||
                      obj$references_pull[[ref_uid]]$is_invalid
                ) {                 
                  obj$references_pull[[ref_uid]] <- obj$registry[[ref_uid]]
                }
                ## TODO: fix #14 (strictness for invalidation)
                          
                ## Get last-known reference checksum //
                ref_chk_own <- obj$checksums_ref[[ref_uid]]
                if (  !is.null(obj$references_pull[[ref_uid]]$checksum) 
                      ## --> due to invalidation
#                       !is.null(ref_chk <- obj$references_pull[[ref_uid]]$checksum)
                      ## --> can't be anymore
                ) {
                  ref_chk <- obj$references_pull[[ref_uid]]$checksum                 
                  if (is.null(ref_chk_own) || ref_chk != ref_chk_own) {
                  ## Hash reference missing or reference has changed 
                  ## --> update                    
                    message(paste0("Modified reference: ", ref_uid))
                    obj$updateReferenceChecksum(
                      ref = ref_uid, 
                      checksum = ref_chk
                    )
                    do_update <- TRUE
                  }
                } else {
                ## Check if 'broken-binding' condition exists //
                  if (!is.null(ref_chk_own)) {
                  ## This can only be the case if there has been a reactive 
                  ## binding that was valid/working at one point in time
                    if (strict_get == 0) {
                      ## Do nothing //
                    } else if (strict_get == 1) {                    
                      conditionr::signalCondition(
                        call = substitute(
                          get(x= ID, envir = WHERE, inherits = FALSE),
                          list(ID = obj$id, WHERE = obj$where)
                        ),
                        condition = "BrokenReactiveReference",
                        msg = c(
                          Reason = "broken reactive reference",
                          ID = obj$id,
                          UID = obj$uid,
                          Location = capture.output(where),
                          "Reference UID" = ref_uid
                        ),
                        ns = "reactr",
                        type = "warning"
                      )
                      obj$value <<- NULL
                    } else if (strict_get == 2) {
                      cond <- conditionr::signalCondition(
                        call = substitute(
                          get(x= ID, envir = WHERE, inherits = FALSE),
                          list(ID = obj$id, WHERE = obj$where)
                        ),
                        condition = "BrokenReactiveReference",
                        msg = c(
                          Reason = "broken reactive reference",
                          ID = id,
                          UID = obj$uid,
                          Location = capture.output(where),
                          "Reference UID" = ref_uid
                        ),
                        ns = "reactr",
                        type = "error",
                        signal = FALSE
                      )
                      
                      ## Transfer condition //
                      obj$condition <<- cond
                    }
                  }
                }
                do_update
              })

              ##----------------------------------------------------------------
              ## Actual update //
              ##----------------------------------------------------------------

              if (any(do_update) || !obj$has_cached) {
                if (!obj$has_cached) {
                  message("Initializing ...")  
                }
                if (any(do_update)) {
                  message("Updating ...")  
                }
                
                ## Cache new value //
                obj$value <<- withRestarts(
                  tryCatch(
                    {
                      value()
                    
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
                    registry <- getRegistry()
                    ## Custom condition //
                    cond <- conditionr::signalCondition(
                      condition = "AbortedReactiveUpdateWithError",
                      msg = c(
                        "Update failed",
                        ID = obj$id,
                        UID = obj$uid,
                        Location = capture.output(where),
## TODO: GitHub #2
## Think of ways of making conditions more informative with respect 
## to which references cause the failure
                        Reason = conditionMessage(cond)
                      ),
                      ns = "reactr",
                      type = "error",
                      signal = FALSE
                    )
                    ## Transfer condition //
                    obj$condition <<- cond
                    NULL
                  }
                )
                ## Update fields //
                obj$computeChecksum()
                obj$has_cached <- TRUE
                obj$condition <- NULL
              }
            } 
          } else {
          
          ##--------------------------------------------------------------------
          ## Handler for 'set' (i.e. 'assign()' or '<-') //
          ##--------------------------------------------------------------------            
            
            ## Set //
            if (obj$hasPullReferences()) {
              if (strict_set == 0) {
                obj$value <<- v    
              } else if (strict_set == 1) {
                ## Do nothing //
              } else if (strict_set == 2) {
                conditionr::signalCondition(
                  call = substitute(
                    assign(x= ID, value = VALUE, envir = WHERE, inherits = FALSE),
                    list(ID = obj$id, VALUE = v, WHERE = obj$where)
                  ),
                  condition = "AbortedWithReactiveDependencyWarning",
                  msg = c(
                    Reason = "trying to set value of object with reactive dependency",
                    ID = obj$id,
                    UID = obj$uid,
                    Location = capture.output(where),
                    References = paste(ls(obj$references_pull, all.names = TRUE), collapse = ", ")
                  ),
                  ns = "reactr",
                  type = "warning"
                )
              } else if (strict_set == 3) {
                conditionr::signalCondition(
                  call = substitute(
                    assign(x= ID, value = VALUE, envir = WHERE, inherits = FALSE),
                    list(ID = obj$id, VALUE = v, WHERE = obj$where)
                  ),
                  condition = "AbortedWithReactiveDependencyError",
                  msg = c(
                    Reason = "trying to set value of object with reactive dependency",
                    ID = obj$id,
                    UID = obj$uid,
                    Location = capture.output(where),
                    References = paste(ls(obj$references_pull, all.names = TRUE), collapse = ", ")
                  ),
                  ns = "reactr",
                  type = "error"
                )
              }
            } else {
              obj$value <<- v 
            }
            
            ## Update checksum //
#             assign(obj$uid, digest::digest(v), envir = obj$registry[[obj$uid]])   
            obj$computeChecksum()

            ## Push //
            if (	obj$hasPushReferences() && 
                  !obj$has_pushed && 
                  !obj$is_running_push
            ) {
              message("Pushing ...")
              obj$pushToReferences()
              ## Reset value of push control field //
              obj$has_pushed <- FALSE
            }

          }

          ## Condition handling //
          if (!is.null(obj$condition)) {           
            if (inherits(obj$condition, "BrokenReactiveReference")) {
              obj$value <- stop(obj$condition)
            } else {            
              obj$value <- stop(obj$condition)
            }
          }

          ## Return value //
          if (.debug) {
            obj
          } else {
            obj$value
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
#     out <- obj$value
  } else {
    out <- value
  }
  
  return(out)
  
}
