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
#'    Relevant if a referenced object has been removed thus breaking the 
#'    reactive relationship.
#'    \itemize{
#'      \item{\code{0}: } {function returns last cached value}
#'      \item{\code{1}: } {object value is set to \code{NULL} and is returned}
#'      \item{\code{2}: } {object value is set to an instance of condition class 
#'          \code{BrokenReactiveBinding} and this condition is triggered whenever
#'          the object's value is requested by \code{\link[base]{get}} or 
#'          its syntactical surgars \code{{obj-name} or \code{}}
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
    .debug = FALSE,
    ...
  ) {
  
  ## Argument checks //
  strict <- as.numeric(match.arg(as.character(strict), 
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
      ctx = yamlr::YamlContext.ReactiveReference.S3(),
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

    ## 2) Recognize via argument 'refs' //
    if (is.null(refs)) {
      refs <- .getReferencesFromArguments(expr = formals(value), where = where)
      if (!is.null(refs)) {
        value <- .transformReactiveFunction(refs = refs, fun = value)
        if (.debug) {
          message("Updated binding function:")
          print(value)
        }
        references <- unname(sapply(names(refs), function(ii) {
          getReactiveUid(id = ii, where = eval(refs[[ii]]$where))
        }))
      } 
    }

    ## 3) Recognize via '.ref_*' specification in body //
    if (is.null(refs)) {
#       references <- as.character(unlist(.getReferences(expr = body(value))))
#       refs <- .getReferences(expr = body(value), where = where)
      refs <- .getReferencesFromBody(expr = value, where = where)
# print(refs)
# ref=refs[[1]]
# ref=refs[[1]][[3]]
      references <- sapply(refs, function(ref) {
        if (class(ref) == "<-") {
          ref_this <- ref[[3]]
        } else if (class(ref) == "call" && ref[[1]] == "get") {
          ref_this <- ref 
        }
        id_this <- ifelse(is.null(ref_this$x), ref_this[[2]], ref_this$x)
# ref_this <<- ref_this
        ## Decompose //
        ref_this_dec <- lapply(ref_this, function(ii) ii)

        ## Recognize 'x' and 'envir' even though they might not have 
        ## been named //
        ## TODO: GitHub #7
        ## --> fixed
        id_this <- if ("x" %in% names(ref_this_dec)) {
          ref_this_dec$x
        } else {
          idx_id <- which(sapply(ref_this_dec, is.character))
          if (!length(idx_id)) {
            stop(paste0("No name/ID found (argument 'x' of 'get()')"))
          }
          ref_this_dec[[idx_id]]
        }
        envir_this <- if ("envir" %in% names(ref_this_dec)) {
          ref_this_dec$envir
        } else {
          ## Throw out potential entry for 'value' //
          if (length(idx_out <- which(names(ref_this_dec) %in% "value"))) {
            ref_this_dec <- ref_this_dec[-idx_out]
          }
          idx_envir <- which(sapply(ref_this_dec, is.environment))
          if (!length(idx_envir)) {
            stop(paste0("No environment found (argument 'envir' of 'get()')"))
          }
          ref_this_dec[[max(idx_envir)]]
        }
        
        ## UIDs //
        getReactiveUid(
          id = id_this, 
          where = envir_this
        )
      })

      value <- .transformReactiveFunction(refs = refs, fun = value)
      if (.debug) {
        message("Updated binding function:")
        print(value)
      }

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
    where = where
  )

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
          if (!missing(v)) {
            
          ##--------------------------------------------------------------------
          ## Handler for 'set' (i.e. 'assign()' or '<-') //
          ##--------------------------------------------------------------------            
            
            ## Set //
            obj$value <<- v
            ## TODO: GitHub #5
            ## Implement strictness levels when setting values of 
            ## objects that have an "observing-only" character with 
            ## respect to the reactive bindings to others.
            ## In a very strict interpretation, setting an value for such
            ## objects would not be considered valid as they are only supposed
            ## to "observe and react".
            ## Possible catch this in 'checkReactivityPrerequistes()` already.
            ## Might involve some new "helper fields" in class 'Reactive.S3' 
          
            ## Update hash //
            assign(obj$uid, digest::digest(v), envir = obj$hash[[obj$uid]]) 
          } else {
            
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
                    if (strict == 0) {
                      ## Do nothing //
                    } else if (strict == 1) {                    
                      obj$value <<- NULL
                    } else if (strict == 2) {
                      cond <- conditionr::signalCondition(
                        condition = "BrokenReactiveBinding",
                        msg = c(
                          "Broken reactive binding",
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
                      condition = "ReactiveUpdateFailed",
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
          }
          
          ## Condition handling //
          if (!is.null(obj$condition)) {
            obj$value <- stop(obj$condition)
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
