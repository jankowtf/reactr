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
#' @param force \code{\link{logical}}.
#'    \code{TRUE}: force a binding reset even though there might already
#'    have been defined another one;
#'    \code{FALSE}: in case a binding has already been defined it is not
#'    overwritten.
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
    force = FALSE,
    .debug = FALSE,
    ...
  ) {

  ## Ensure shiny let's us do this //
  shiny_opt <- getOption("shiny.suppressMissingContextError")
  if (is.null(shiny_opt) || !shiny_opt) {
    options(shiny.suppressMissingContextError = TRUE)  
  }
  
  ## Check if regular value assignment or reactive function //
#   if (!inherits(value, "reactive")) {
  if (!is.function(value)) {    
    is_reactive <- FALSE
    dependencies <- character()
    
    value_initial <- substitute(VALUE, list(VALUE = value))
#     value_expr <- quote(obj$value <<- value)
    value_expr <- NULL
    
    ##############################
    ## Using shiny functionality #
    ##############################
    
#     shiny::makeReactiveBinding(symbol = id, env = where)
#       assign(id, value, where)
#     value_expr <- substitute(OBJ, list(OBJ = value))
  } else {
    is_reactive <- TRUE
    
    ## Local functions //
    .getDependenciesFromArguments <- function(expr) {
      arglist <- formals(value)
      deps <- NULL
      if ("deps" %in% names(arglist)) {
        deps <- lapply(arglist$deps, function(ii) ii)[-1]
      }
    }
    .transformBindingFunction <- function(deps) {
      expr <- lapply(names(deps), function(ii) {
        substitute(OBJ <- get(X, envir = ENVIR), 
                   list(OBJ = as.name(ii), X = ii, ENVIR = deps[[ii]]))
      })
      body_scope <- length(body(value))
      expr_list <- if (body_scope == 1) {
        body(value) 
      } else {
        lapply(1:body_scope, function(ii) body(value)[[ii]])
      }
      body(value) <- substitute(
        {
          eval(E1)
          eval(E2)
        },
        list(
          E1 = as.expression(expr),
          E2 = as.expression(expr_list)
        )
      )
      value
    }
    .getDependencies <- function(expr) {
      lapply(expr, function(expr_2) {
  #       lapply(expr, function(ii) ii)
        if (is.call(expr_2)) {
          if (class(expr_2) == "<-") {
            if (grepl("\\.react_", as.character(expr_2[[2]]))) {
#               return(expr_2[[3]])
              tmp <- expr_2[[3]]
#               expr <- substitute(digest::digest(list(id = ID, where = WHERE)), 
#                 list(ID = tmp$x, WHERE = tmp$envir))
#               return(eval(expr))
              return(getReactiveUid(id = tmp$x, where = tmp$envir))
              
            }
          }
        }
      })
    }

    ## Dependencies //
    deps <- .getDependenciesFromArguments(formals(value))
    if (!is.null(deps)) {
      value <- .transformBindingFunction(deps = deps)
      if (.debug) {
        message("Updated binding function:")
        print(value)
      }
#       deps_2 <- lapply(deps, function(ii) eval(ii))
      dependencies <- unname(sapply(names(deps), function(ii) {
        getReactiveUid(id = ii, where = deps[[ii]])
      }))
#       getReactiveUid(id = "x_1", where = where_1)
    } else {
      dependencies <- as.character(unlist(.getDependencies(expr = body(value))))   
    }
    value_initial <- tryCatch(value(), error = function(cond) NULL)
    value_expr <- quote(obj$value <<- value())
  
    ##############################
    ## Using shiny functionality #
    ##############################
  
    ## Putting together the "line of lines" //
    ## Approach 1:
#     value_expr <- substitute(obj <<- OBJ(), list(OBJ = value))
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
#     reactive_expr <- attributes(value)$observable$.label
#     value_expr <- substitute(obj <<- eval(OBJ)(), 
#       list(OBJ = parse(text = reactive_expr)))
  }

  ## Instance of 'Reactive.S3' //
  obj <- reactr::Reactive.S3(
    value = value_initial
  )
  obj$id <- id
  obj$uid <- getReactiveUid(id = id, where = where)
#     expr <- substitute(digest::digest(list(id = ID, where = WHERE)), 
#     list(ID = obj$id, WHERE = eval(where)))
  obj$where <- where
  
  ## Ensure subenvironment in hash registry //
  if (!exists(obj$uid, envir = obj$hash)) {
    assign(obj$uid, new.env(parent = emptyenv()), envir = obj$hash)      
  }

  ## Ensure 'id' entry in hash registry //
  assign("id", id, envir = obj$hash[[obj$uid]])      
  ## Ensure 'id' entry in hash registry //
  assign("where", where, envir = obj$hash[[obj$uid]])     

  has_binding <- try(bindingIsActive(id, where), silent = TRUE)
  if (inherits(has_binding, "try-error") || force) {
    has_binding <- FALSE
  } 
  if (!has_binding) {
    if (exists(id, envir = where, inherits = FALSE)) {
      rm(list = id, envir = where, inherits = TRUE)
    }
  }
  
  ## Call to 'makeActiveBinding()' //
  expr <- substitute(
    makeActiveBinding(
      id,
      local({
        obj
        assign(obj$uid, digest::digest(obj$value), envir = obj$hash[[obj$uid]])      
        function(v) {
          if (!missing(v)) {
            ## Set //
            obj$value <<- v
            ## Update hash //
            assign(obj$uid, digest::digest(v), envir = obj$hash[[obj$uid]]) 
          } else {
            if (length(dependencies)) {
              ## Update decision //
              ## Compare hash values of all dependencies with the ones in the
              ## own cache --> if any has changed: update
              do_update <- sapply(dependencies, function(dep_uid) {
                do_update <- FALSE
                if (!is.null(dep_hash <- obj$hash[[dep_uid]][[dep_uid]])) {
                  own_dep_hash <- obj$hash[[obj$uid]][[dep_uid]]
                  if (is.null(own_dep_hash) || dep_hash != own_dep_hash) {
                    message(paste0("Dependency has changed: ", dep_uid))
                    assign(dep_uid, dep_hash, obj$hash[[obj$uid]])
                    do_update <- TRUE
                  }
                }
                do_update
              })

              ## Actual update //
              if (any(do_update)) {
                message("Updating ...")
                ## Cache new value //
                obj$value <<- withRestarts(
                  value(),
                  error = function(cond) NULL,
                  warning = function(cond) invokeRestart("muffleWarning")
                )
                ## New own hash //
                assign(obj$uid, digest::digest(obj$value), 
                  envir = obj$hash[[obj$uid]])      
              }
            }
          }
          
          if (.debug) {
            obj
          } else {
            obj$value
          }
        }
      }),
      env = where
    ),
    list(
#       OBJ = obj
#       VALUE_EXPR = value_expr
     )
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
