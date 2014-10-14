## fix: github #11
##      Inconsistency for initial values

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
#'      (this also triggers an updated of the object and thus reactivity is 
#'      then completely restored)
#'      }
#'      \item{\code{1}: } {Warning is issued and object value is not changed}
#'      \item{\code{2}: } {Error is issued and object value is not changed}
#'      }
#'    }
#' @template threedot
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
                                 as.character(c(0, 1, 2))))

  ## Check if regular value assignment or reactive function //
  if (!is.function(value)) {    
    is_reactive <- FALSE
    references <- character()
    
    value_initial <- substitute(VALUE, list(VALUE = value))
#     value_expr <- quote(obj$value <<- value)
    value_expr <- NULL
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
# where=environment()      
    if (length(yaml$original)) { 
      refs <- yaml
      references <- unname(sapply(names(yaml$parsed), function(ii) {
        getReactiveUid(id = ii, where = eval(yaml$parsed[[ii]]$where))
      }))
      value <- yaml$src
      if (.debug) {
        message("Updated binding function:")
        print(value)
      }
    } else {
      refs <- NULL
    }
# where=parent.frame()
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
          getReactiveUid(id = ii, where = eval(refs[[ii]]$where))
        }))
      } 
    }

    ## 3) Recognize via '.ref_*' specification in body //
    if (is.null(refs)) {
#       references <- as.character(unlist(.getReferences(expr = body(value))))
#       refs <- .getReferences(expr = body(value), where = where)
      res <- .getReferencesFromBody2(expr = value, where = where)
      refs <- res$refs
      value <- res$fun
      if (.debug) {
        message("Updated binding function:")
        print(value)
      }
      references <- .getActualReferencesFromBody(refs = refs, where = where)
    }

    value_initial <- tryCatch(value(), error = function(cond) NULL)
    value_expr <- quote(obj$value <<- value())
  }

  ## Instance of class 'Reactive.S3' //
  obj <- reactr::Reactive.S3()
  
  obj <- prepareReactiveInstance(
    input = obj, 
    id = id,
    value = value_initial,
    where = where,
    references = references
  )
# ref_uids <- ls(obj$references)
# ls(obj$references[[ref_uids[1]]])
# obj$hasReferences()

  ## Check prerequisites //
  checkReactivityPrerequisites(input = obj, strict = strict)

  ## Call to 'makeActiveBinding()' //
  expr <- substitute(
    makeActiveBinding(
      id,
      local({
        OBJ
        assign(obj$uid, digest::digest(obj$value), envir = obj$hash[[obj$uid]])      
        function(v) {
          if (missing(v)) {
            
          ##--------------------------------------------------------------------
          ## Handler for 'get' (i.e. 'get()' or '{obj-name}' or '${obj-name}) //
          ##--------------------------------------------------------------------            
            
            ## Process references //
            if (length(references)) {
              ## Update decision //
              ## Compare hash values of all references with the ones in 
              ## own cache; 
              ## - if any has changed --> update
              ## - if not --> return cached value
              do_update <- sapply(references, function(ref_uid) {
                do_update <- FALSE
                ## Get last-known stored hash value for reference //
                own_ref_hash <- obj$hash[[obj$uid]][[ref_uid]]
                if (!is.null(ref_hash <- obj$hash[[ref_uid]][[ref_uid]])) {
                  if (is.null(own_ref_hash) || ref_hash != own_ref_hash) {
                  ## Hash reference missing or reference has changed 
                  ## --> update                    
                    message(paste0("Reference has changed: ", ref_uid))
                    assign(ref_uid, ref_hash, obj$hash[[obj$uid]])
                    do_update <- TRUE
                  }
                } else {
                ## Check if 'broken-binding' condition exists //
                  if (!is.null(own_ref_hash)) {
                  ## This can only be the case if there has been a reactive 
                  ## binding that was valid/working at one time
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

              if (any(do_update)) {
                message("Updating ...")
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
                    hash <- getHashRegistry()
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
                ## Update own hash //
                assign(obj$uid, digest::digest(obj$value), 
                  envir = obj$hash[[obj$uid]])      
              }
            }

          } else {
          
          ##--------------------------------------------------------------------
          ## Handler for 'set' (i.e. 'assign()' or '<-') //
          ##--------------------------------------------------------------------            
            
            ## Set //
            if (obj$hasReferences()) {
              if (strict_set == 0) {
                obj$value <<- v    
              } else if (strict_set == 1) {
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
                    References = paste(ls(obj$references, all.names = TRUE), collapse = ", ")
                  ),
                  ns = "reactr",
                  type = "warning"
                )
              } else if (strict_set == 2) {
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
                    References = paste(ls(obj$references, all.names = TRUE), collapse = ", ")
                  ),
                  ns = "reactr",
                  type = "error"
                )
              }
            } else {
              obj$value <<- v 
            }
            
            ## Update hash //
            assign(obj$uid, digest::digest(v), envir = obj$hash[[obj$uid]])   
            
          
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

  ## Return value //
  if (is_reactive) {
    out <- get(id, envir = where, inherits = FALSE)
  } else {
    out <- value
  }
  
  return(out)
  
}
