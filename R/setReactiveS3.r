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
#'    \itemiz{
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
    ## Local functions //
    ##--------------------------------------------------------------------------
    
    ## TODO: GitHub #1
    ## Refactor local functions
    
    .getReferencesFromArguments <- function(expr, where) {
      arglist <- formals(value)
      refs <- NULL
      if ("refs" %in% names(arglist)) {
        refs <- arglist["refs"]
        tmp <- lapply(arglist$refs, function(ii) ii)[-1]
        if (is.null(tmp[[1]])) {
          tmp[[1]] <- where
        }
        refs <- list(list(id = names(tmp), where = tmp[[1]]))
        names(refs) <- names(tmp)
        refs
      }
    }
    .getReferences <- function(expr, where) {
      buffer <- new.env()
      buffer$out <- list()
# expr_2=expr[[1]]      
# expr_2 <- lapply(expr, function(ii) ii)[[1]]
      lapply(expr, function(expr_2) {
        if (is.call(expr_2)) {
          ## Catch brackets //
          if (expr_2[[1]] == "{") {
            expr_2 <- expr_2[[2]]
          }
          if (class(expr_2) == "<-") {
            if (grepl("\\.ref_", as.character(expr_2[[2]]))) {
#               return(expr_2[[3]])
              tmp <- expr_2[[3]]
#               expr <- substitute(digest::digest(list(id = ID, where = WHERE)), 
#                 list(ID = tmp$x, WHERE = tmp$envir))
# print(tmp$envir)
# print(eval(tmp$envir))
# print(eval(eval(tmp$envir)))
#               return(eval(expr))
              if (is.null(tmp$envir)) {
#                 tmp$envir <- eval(where)
                expr_2[[3]]$envir <- eval(where)
              }
#               return(getReactiveUid(id = tmp$x, where = eval(tmp$envir)))
#               buffer$out <<- c(buffer$out, tmp)
              buffer$out <<- c(buffer$out, expr_2)
              
            }
          }
        }
      })
      buffer$out
    }
    .transformReactiveFunction <- function(refs, fun) {
      idx <- if (!is.null(names(refs))) {
        names(refs)
      } else {
        seq(along = refs)
      }
# ref <- refs[[1]]      
      expr <- lapply(idx, function(ii) {
        ref <- refs[[ii]]
        if (is.call(ref)) {
          return(ref)
        }
        
        id <- if (!is.null(ref$as)) {
          as.name(ref$as)
        } else {
          as.name(ii)
        }
        
        code <- capture.output(body(fun))
        ## "Actual code before markup" principle //
        ## Make sure that we don't overwrite explicitly stated code 
        ## with potentially erroneous markup information
        ## TODO: GitHub #3
        ## Make sure that erroneous/diverging markup does not cause any 
        ## trouble
        if (any(grepl(paste0(id, "\\s?<-"), code))) {
          out <- NULL
        } else {
          out <- substitute(ID <- get(X, envir = ENVIR, inherits = FALSE), 
            list(
              ID = id, 
              X = ii, 
              ENVIR = if (!is.null(ref$where)) {
                ref$where
              } else {
                refs[[ii]]
              }
            )
          )
        }
        out
      })
      
      ## Transform body //
      ## Ensure brackets:
      bdy <- body(fun)
      body_scope <- length(bdy)
      if (body_scope > 1) {
        if (bdy[[1]] != "{") {
          bdy <- substitute({BODY}, list(BODY = bdy))
        }
      } else {
        bdy <- substitute({BODY}, list(BODY = bdy))        
      }
      body_scope <- length(bdy)

      expr_list <- if (body_scope > 0 && body_scope <= 1) {
        body(fun) 
      } else {
        lapply(2:body_scope, function(ii) bdy[[ii]])
      }
      
      ##------------------------------------------------------------------------
      ## Patch to ensure default 'where' if it has not been specified //
      ##------------------------------------------------------------------------
      ## Reeeally dirty, but seems to work ;-)
      
      ## TODO: GitHub #4
      ## Improve reference recognition/specification
# ii_sub=1
# ii_expr=1
      for (ii_sub in seq(along = expr)) {
        sub_this <- expr[[ii_sub]]
        if (class(sub_this) == "<-") {
          sub_this <- sub_this[[3]]
        } else if (class(sub_this) == "call" && sub_this[[1]] == "get") {
#           sub_this <- sub_this
        }
        if (sub_this[[1]] == "get") {
          for (ii_expr in seq(along = expr_list)) {
            expr_this <- expr_list[[ii_expr]]
#             deparse(expr_this)
            if (any(grepl(paste0("get\\(.*", sub_this$x), expr_this))) {
              if (expr_this[[1]] == "<-") {
              ## Assignment with get expression //
                expr_list[[ii_expr]][[3]]$envir <- sub_this$envir
              } else if (expr_this[[1]] == "get") {
              ## Get expression //
                expr_list[[ii_expr]][[1]]$envir <- sub_this$envir
              }
              expr[[ii_sub]] <- NULL
            }
          }    
        }
      }
      
      ## Filter out 'NULL' //
      idx_null <- sapply(expr, is.null)
      if (any(idx_null)) {
        expr <- expr[-which(idx_null)]
      }
      
      body(fun) <- substitute(
        {
          eval(E1)
          ## --> Not all that pretty especially when 'expr' is 'NULL',
          ## but it works
          eval(E2)
        },
        list(
          E1 = if (length(expr)) as.expression(expr) else NULL,
          E2 = as.expression(expr_list)
        )
      )
      fun
    }
    .getReferenceMarkup <- function(expr) {
      pattern_1 <- "\\[@reactive-ref:"
      code <- capture.output(expr)
      grep(pattern_1, code, value = TRUE)
    }
    .getReferencesFromMarkup <- function(markup, where) {
      pattern_1 <- "^.*\\[@reactive-ref:\\s?|\\]$"      
      has_where <- grepl("\\s?in\\s?\\w+", markup)
      has_as <- grepl("\\s?as\\s?\\w+", markup)
      refs <- gsub(pattern_1, "", markup)
      refs <- strsplit(refs, split = "\\s?in\\s?|\\s?as\\s?", perl = TRUE)
      ## Remove whitespace //
      refs <- lapply(refs, function(ref) {
        gsub("\\s", "", ref)
      })
      
      ids <- sapply(refs, "[[", 1)
# ii=1      
      refs <- lapply(seq(along = refs), function(ii) {
        has_where <- has_where[ii]
        has_as <- has_as[ii]
        
        ref <- refs[[ii]]
        out <- list()
        out[["id"]] <- ref[1]
        if (length(ref) >= 2) {
          
          out[["where"]] <- if (has_where) {
            tryCatch(
              eval(as.name(ref[2])),
              error = function(cond) {
              ## This means that wrong 'where' value or wrong order 
              ## of elements in markup code
              ## TODO: github #
                conditionr::signalCondition(
                  condition = "InvalidReferenceMarkupStructure",
                  msg = c(
                    "Invalid reference markup structure",
                    Error = conditionMessage(cond),
                    "Most likely reason" = "missspecified markup code",
                    "Your markup" = gsub("^.*#+", "", markup[ii]),
                    "Expected markup" = "[@reactive-ref: {id} in {where} as {ref-id}",
                    Note = "{where} and {ref-id} are optional, but if they are provided it must be in the stated order"
                  ),
                  ns = "reactr",
                  type = "error"
                )
              }
            )
          } else {
            if (has_as) {
              out[["as"]] <- as.name(ref[2])
            }
            where
          }
        } else {
#           quote(parent.frame())
#           quote(where)
          out[["where"]] <- where
        } 
        if (length(ref) == 3) {
          out[["as"]] <- ref[3]
        }
        out
      })
      names(refs) <- ids
      refs
    }

    ##--------------------------------------------------------------------------
    ## Recognition of references //
    ##--------------------------------------------------------------------------
    ## In order of preferred specification in 'value'

    ## 1) Recognize via markup in comments //
    refs <- .getReferenceMarkup(expr = value)
    if (length(refs)) { 
      refs <- .getReferencesFromMarkup(markup = refs, where = where)
      references <- unname(sapply(names(refs), function(ii) {
        getReactiveUid(id = ii, where = refs[[ii]]$where)
      }))
# refs <<- refs      
      value <- .transformReactiveFunction(refs = refs, fun = value)
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
      refs <- .getReferences(expr = value, where = where)
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
