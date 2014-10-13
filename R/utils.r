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
.getReferencesFromBody <- function(expr, where) {
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
# refs <- .getReferenceYaml(expr = value)
# refs <- .parseReferenceYaml(yaml = refs$yaml)
# value <- .transformReactiveFunction(refs = refs, fun = value)


##------------------------------------------------------------------------------
## OLD
##------------------------------------------------------------------------------

## Keep as a reference

# .getReferencesFromMarkup <- function(markup, where) {
#   pattern_1 <- "^.*\\[@reactive-ref:\\s?|\\]$"      
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
#                 "Expected markup" = "[@reactive-ref: {id} in {where} as {ref-id}",
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
