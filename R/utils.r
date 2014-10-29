##--------------------------------------------------------------------------
## Local functions //
##--------------------------------------------------------------------------

## TODO: GitHub #1
## Refactor local functions

.getReferencesFromArguments <- function(arguments, where) {
  refs <- NULL
  if ("refs" %in% names(arguments)) {
    tmp <- lapply(arguments$refs, function(ii) ii)[-1]
    refs <- lapply(tmp, function(ii) {
      tmp <- eval(ii, envir = where)
      ## --> extremely important to grab the correct parent environment where
      ## arguments have been specified!
      if (is.null(tmp$where)) {
        tmp$where <- where
      }
      if (is.null(tmp$as)) {
        tmp$as <- substitute(AS, list(AS = as.name(tmp$id)))
      }
      tmp$uid <- computeObjectUid(
        id = tmp$id, 
        where = tmp$where
      )
      tmp
    })
  }
  refs
}
# .getReferencesFromArguments(arguments = formals(value), where = where)
.getReferencesFromBody2 <- function(fun, where) {
  buffer <- new.env()
  buffer$refs <- list()
# fun_2=fun[[1]]      
# fun_2 <- lapply(fun, function(ii) ii)[[1]]
  bdy <- body(fun)
  if (class(bdy) != "{") {
    bdy <- substitute({BODY}, list(BODY = bdy))
    body(fun) <- bdy    
  }
ii=2
  lapply(seq(along = body(fun)), function(ii) {
    bdy_el <- bdy[[ii]]
    if (class(bdy_el) %in% c("<-", "=") && any(grepl("\\.ref_", bdy_el))) {
      tmp <- bdy_el[[3]]  
      if (any(grepl("get", tmp))) {
        if (is.null(tmp$envir)) {
          bdy_el[[3]]$envir <- eval(where)
          body(fun)[[ii]][[3]]$envir <<- eval(where)
        }
        if (is.null(tmp$inherits)) {
          bdy_el[[3]]$inherits <- FALSE
          body(fun)[[ii]][[3]]$inherits <<- FALSE
        }
        buffer$refs <<- c(buffer$refs, bdy_el)
      }
    }
  })
#   buffer$refs
  list(refs = buffer$refs, fun = fun)
}

.getActualReferencesFromBody <- function(refs, where) {
  nms <- vector("character", length(refs))
  
  out <- lapply(seq(along = refs), function(ref_ii) {
    ref <- refs[[ref_ii]]
    if (class(ref) %in% c("<-", "=")) {
      ref_this <- ref[[3]]
    } else if (class(ref) == "call" && ref[[1]] == "get") {
      ref_this <- ref 
    }
#     id_this <- eval(ifelse(is.null(ref_this$x), ref_this[[2]], ref_this$x))
# ref_this <<- ref_this
    ## Decompose //
    ref_this_dec <- lapply(ref_this, function(ii) ii)
    ## Recognize 'x' and 'envir' even though they might not have 
    ## been named //
    ## TODO: GitHub #7
    ## --> fixed
    id_this <- eval(if ("x" %in% names(ref_this_dec)) {
      ref_this_dec$x
    } else {
      idx_id <- which(sapply(ref_this_dec, is.character))
      if (!length(idx_id)) {
        stop(paste0("No name/ID found (argument 'x' of 'get()')"))
      }
      ref_this_dec[[idx_id]]
    })
    envir_this <- eval(if ("envir" %in% names(ref_this_dec)) {
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
    })

#     ## UIDs //
#     computeObjectUid(
#       id = id_this, 
#       where = envir_this
#     )
    nms[[ref_ii]] <<- id_this
    list(
      id = id_this, 
      uid = computeObjectUid(
        id = id_this, 
        where = envir_this
      ),
      where = envir_this
    )
  })
  names(out) <- nms
  out
}
.transformReactiveFunction <- function(refs, fun) {
#   idx <- unname(if (!is.null(names(refs))) {
# #     names(refs)
#     sapply(refs, "[[", "id")
#   } else {
#     seq(along = refs)
#   })
  idx <- seq(along = refs)
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
          X = ref$id, 
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
# refs <- .getReferenceYaml(expr = value)
# refs <- .parseReferenceYaml(yaml = refs$yaml)
# value <- .transformReactiveFunction(refs = refs, fun = value)

##------------------------------------------------------------------------------
## shiny
##------------------------------------------------------------------------------

.getReferenceYaml <- function(expr) {
  idx_yaml <- which(sapply(expr, function(expr) {
    any(grepl("^reactive:", expr))
  }))
  if (length(idx_yaml)) {
    yaml <- structure(
      list(
        yaml = sapply(idx_yaml, function(idx) expr[[idx]]),
        index = idx_yaml
       ),
      class = c("ReactiveReferenceYaml", "list")
    )
  } else {
    yaml <- structure(
      list(yaml = character(), index = character()),
      class = c("ReactiveReferenceYaml", "list")
    )
  }
}
# yaml <- .getReferenceYaml(expr = expr)
.parseYaml <- function(yaml) {
  nms <- vector("character", length(yaml))
  yaml_parsed <- lapply(seq(along=yaml), function(ii) {
    out <- yaml.load(yaml[ii])[[1]]
    if (is.null(out$where)) {
      out$where <- as.name("where")
    } else {
      out$where <- as.name(out$where)
    }
    if (is.null(out$as)) {
      out$as <- as.name(out$id)
    } else {
      out$as <- as.name(out$as)
    }
    nms[[ii]] <<- out$id
    out
  })
  names(yaml_parsed) <- nms
  yaml_parsed
}
# yaml_parsed <- .parseYaml(yaml = yaml)
.constructGetExpressionFromYaml <- function(yaml, where) {
  yaml_parsed <- .parseYaml(yaml = yaml)
  expr_get <- lapply(yaml_parsed, function(el) {
    substitute(AS <- get(x = ID, envir = WHERE, inherits = FALSE),
               list(AS = el$as, ID = el$id, WHERE = eval(el$where))
               )
  })
  expr_get
}
.computeObjectUids <- function(yaml_parsed, where) {
  sapply(yaml_parsed, function(el) {
    digest::digest(list(
      id = el$id,
      where = capture.output(eval(el$where))
      ## --> very important to use `capture.output` instead of the
      ## environment itself as changes in the environment will be
      ## recognized by `digest()` and thus would lead to a different
      ## UID being assigned each time
    ))
  })
}
.updateReactiveExpression <- function(expr, expr_add, idx_yaml) {
  for (ii in seq(along=idx_yaml)) {
    expr[[idx_yaml[ii]]] <- expr_add[[ii]]
  }
  expr
}
.processReferenceYaml <- function(expr, where) {
  yaml <- .getReferenceYaml(expr = expr)
  if (length(yaml$yaml)) {
    yaml_parsed <- .parseYaml(yaml = yaml$yaml)
    .computeObjectUids(yaml_parsed, where = where)
    expr_add <- .constructGetExpressionFromYaml(yaml = yaml$yaml, where = where)

    out <- .updateReactiveExpression(
      expr = expr,
      expr_add = expr_add,
      idx_yaml = yaml$index
    )
  } else {
    out <- expr
  }
  out
}
.constructGetChecksumExpressionFromYaml <- function(yaml, where) {
  yaml_parsed <- .parseYaml(yaml = yaml)
  expr <- lapply(yaml_parsed, function(el) {
    uid <- .computeObjectUid(id = el$id, where = eval(el$where))
    expr <- substitute(get(
        x = UID,
        envir = getOption("shiny")$.registry,
        inherits = FALSE
      )$checksum,
      list(UID = uid)
    )
    list(uid = uid, id = el$id, as = el$as, where = el$where, expr = expr)
  })
  names(expr) <- sapply(yaml_parsed, "[[", "as")
  expr
}
.computeObjectUid <- function(id, where) {
  eval(substitute(digest::digest(list(id = ID, where = WHERE)),
    list(
      ID = id,
      WHERE = capture.output(eval(where))
      ## --> very important to use `capture.output` instead of the
      ## environment itself as changes in the environment will be
      ## recognized by `digest()` and thus would lead to a different
      ## UID being assigned each time
    )
  ))
}

getReactiveReferenceInfo <- function(x, env=parent.frame(2), quoted=FALSE,
  caller_offset=1) {
  # Get the quoted expr from two calls back
  expr_sub <- eval(substitute(substitute(x)), parent.frame(caller_offset))
  yaml <- .getReferenceYaml(expr = expr_sub)
  if (length(yaml$yaml)) {
# tmp <- .parseYaml(yaml = yaml$yaml)
# .computeObjectUid(id = tmp$x_1$id, where = eval(tmp$x_1$where))
# .computeObjectUid(id = tmp$x_1$id, where = where)
    .constructGetChecksumExpressionFromYaml(yaml$yaml, where = env)
# envir <- new.env()
# envir$.registry <- new.env()
# options("shiny" = envir)
# eval(out[[1]])
  }
}

##------------------------------------------------------------------------------
## OLD
##------------------------------------------------------------------------------

## Keep as a reference

# .getReferencesFromBody <- function(expr, where) {
#   buffer <- new.env()
#   buffer$out <- list()
# # expr_2=expr[[1]]      
# # expr_2 <- lapply(expr, function(ii) ii)[[1]]
#   lapply(expr, function(expr_2) {
#     if (is.call(expr_2)) {
#       ## Catch brackets //
#       if (expr_2[[1]] == "{") {
#         expr_2 <- expr_2[[2]]
#       }
#       if (class(expr_2) == "<-") {
#         if (grepl("\\.ref_", as.character(expr_2[[2]]))) {
# #               return(expr_2[[3]])
#           tmp <- expr_2[[3]]
# #               expr <- substitute(digest::digest(list(id = ID, where = WHERE)), 
# #                 list(ID = tmp$x, WHERE = tmp$envir))
# # print(tmp$envir)
# # print(eval(tmp$envir))
# # print(eval(eval(tmp$envir)))
# #               return(eval(expr))
#           if (is.null(tmp$envir)) {
# #                 tmp$envir <- eval(where)
#             expr_2[[3]]$envir <- eval(where)
#           }
# #               return(computeObjectUid(id = tmp$x, where = eval(tmp$envir)))
# #               buffer$out <<- c(buffer$out, tmp)
#           buffer$out <<- c(buffer$out, expr_2)
#           
#         }
#       }
#     }
#   })
#   buffer$out
# }
# .getReferencesFromMarkup <- function(markup, where) {
#   pattern_1 <- "^.*\\object-ref:\\s?|\\}$"      
#   has_where <- grepl("\\s?in\\s?\\w+", markup)
#   has_as <- grepl("\\s?as\\s?\\w+", markup)
#   refs <- gsub(pattern_1, "", markup)
#   refs <- strsplit(refs, split = "\\s?in\\s?|\\s?as\\s?", perl = TRUE)
#   ## Remove whitespace //
#   refs <- lapply(refs, function(ref) {
#     gsub("\\s", "", ref)
#   })
#   
#   ids <- sapply(refs, "[[", 1)
# # ii=1      
#   refs <- lapply(seq(along = refs), function(ii) {
#     has_where <- has_where[ii]
#     has_as <- has_as[ii]
#     
#     ref <- refs[[ii]]
#     out <- list()
#     out[["id"]] <- ref[1]
#     if (length(ref) >= 2) {
#       
#       out[["where"]] <- if (has_where) {
#         tryCatch(
#           eval(as.name(ref[2])),
#           error = function(cond) {
#           ## This means that wrong 'where' value or wrong order 
#           ## of elements in markup code
#           ## TODO: github #
#             conditionr::signalCondition(
#               condition = "InvalidReferenceMarkupStructure",
#               msg = c(
#                 "Invalid reference markup structure",
#                 Error = conditionMessage(cond),
#                 "Most likely reason" = "missspecified markup code",
#                 "Your markup" = gsub("^.*#+", "", markup[ii]),
#                 "Expected markup" = "object-ref: {id: {id}, where: {where}, as: {ref-id}}",
#                 Note = "{where} and {ref-id} are optional, but if they are provided it must be in the stated order"
#               ),
#               ns = "reactr",
#               type = "error"
#             )
#           }
#         )
#       } else {
#         if (has_as) {
#           out[["as"]] <- as.name(ref[2])
#         }
#         where
#       }
#     } else {
# #           quote(parent.frame())
# #           quote(where)
#       out[["where"]] <- where
#     } 
#     if (length(ref) == 3) {
#       out[["as"]] <- ref[3]
#     }
#     out
#   })
#   names(refs) <- ids
#   refs
# }
# 
