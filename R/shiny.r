##------------------------------------------------------------------------------
## Classes
##------------------------------------------------------------------------------

Observable <- R6Class(
  'Observable',
  portable = FALSE,
  public = list(
    .func = 'function',
    .label = character(0),
    .domain = NULL,
    .dependents = 'Dependents',
    .invalidated = logical(0),
    .running = logical(0),
    .value = NULL,
    .visible = logical(0),
    .execCount = integer(0),
    .mostRecentCtxId = character(0),

    initialize = function(func, label = deparse(substitute(func)),
                          domain = shiny:::getDefaultReactiveDomain()) {
      if (length(formals(func)) > 0)
        stop("Can't make a reactive expression from a function that takes one ",
             "or more parameters; only functions without parameters can be ",
             "reactive.")
      .func <<- func
      .label <<- label
      .domain <<- domain
      .dependents <<- shiny:::Dependents$new()
      .invalidated <<- TRUE
      .running <<- FALSE
      .execCount <<- 0L
      .mostRecentCtxId <<- ""
    },
    getValue = function() {     
      .dependents$register()
# message("getValue:")
# print(.invalidated)
# print(.running)
      if (.invalidated || .running) {
        self$.updateValue()
      }

      shiny:::.graphDependsOnId(shiny:::getCurrentContext()$id, .mostRecentCtxId)

      if (identical(class(.value), 'try-error'))
        stop(attr(.value, 'condition'))

      if (.visible)
        .value
      else
        invisible(.value)
    },
    .updateValue = function() {
# print(".updateValue")
      ctx <- shiny:::Context$new(.domain, .label, type = 'observable',
                         prevId = .mostRecentCtxId)
      .mostRecentCtxId <<- ctx$id
      ctx$onInvalidate(function() {
        .invalidated <<- TRUE
        .dependents$invalidate()
      })
      .execCount <<- .execCount + 1L

      .invalidated <<- FALSE

      wasRunning <- .running
      .running <<- TRUE
      on.exit(.running <<- wasRunning)

      ctx$run(function() {
# message(".func:")        
# print(.func)        
        result <- withVisible(try(shiny:::shinyCallingHandlers(.func()), silent=TRUE))
        .visible <<- result$visible
        .value <<- result$value
      })
    }
  )
)

#' @import R6
Observable3 <- R6Class(
  'Observable3',
  inherit = Observable,
  portable = FALSE,
  public = list(
    ## [@change: jat, start]
    .checksum = character(0),
    .class = character(0),
    .condition = NULL,
    .id = character(0),
    
    .registry = "environment", 
    .refs_pull = "environment", 
    .refs_push = "environment", 
    .refs_checksum = "environment",
    
    .uid = character(0),
#     .where = parent.frame(6),
    .where = "environment",

    ## Questions //
    .exists_visible = TRUE,
    .force_cached = FALSE,
    .has_cached = FALSE,
    .has_pull_refs = FALSE,
    .has_push_refs = FALSE,
    .has_pushed = FALSE,
    .is_invalid = FALSE,
    .is_running_push = FALSE,
    .must_push = FALSE,
    .needs_update = TRUE,
    initialize = function(
      id, 
      value = NULL,
      where = parent.frame(6), 
      ## --> Corresponds to environment from which `Observable$new()` is called,
      refs_pull = list(),
#       func = NULL, 
      ...
    ) {

#       self <- super$initialize(...)
# print(self)   
      super$initialize(...)
      .id <<- id
      .value <<- value
      .where <<- where
      .registry <<- getRegistry()
      .refs_pull <<- new.env(parent = emptyenv())
      .refs_push <<- new.env(parent = emptyenv())
      .refs_checksum <<- new.env(parent = emptyenv())
      .needs_update <<- TRUE

      ## Other initialization steps //
      .computeChecksum()
      .computeUid()
      .register(overwrite = TRUE)

      if (length(refs_pull)) {
# print(self$.id)        
# print(length(refs_pull))
# print(ls(.refs_pull))
        .registerPullReferences(refs = refs_pull, where = .where)
# print(ls(.refs_pull))
      }

      ## Catch self-reference situations //
      if (.uid %in% ls(.refs_checksum)) {    
        conditionr::signalCondition(
          condition = "NoSelfReferenceAllowed",
          msg = c(
            Reason = "tried to set a self-reference",
            ID = .id,
            UID = .uid,
            Location = capture.output(.where)
          ),
          ns = "reactr",
          type = "error"
        )
      }
    },
    .checkClass = function(v) {
      if (!inherits(v, .class)) {
        num_clss <- c("integer", "numeric")
        if (all(c(class(v), .class) %in% num_clss)) {
          
        } else {
          conditionr::signalCondition(
            call = substitute(
              assign(x= ID, value = VALUE, envir = WHERE, inherits = FALSE),
              list(ID = .id, VALUE = v, WHERE = .where)
            ),
            condition = "AbortedWithClassError",
            msg = c(
              Reason = "class of assignment value does not inherit from initial class",
              ID = .id,
              UID = .uid,
              Location = capture.output(.where),
              "Class expected" = .class,
              "Class provided" = class(v)
            ),
            ns = "reactr",
            type = "error"
          )
        }
      }
    },
    .compareChecksums = function(ref_uid, strict_get = 0, verbose = FALSE) {
      do_update <- FALSE
      ## Get last-known reference checksum //
      ref_chk_own <- .refs_checksum[[ref_uid]]
# message("ref_chk_own:")        
# print(ref_chk_own)         
      if (!is.null(.refs_pull[[ref_uid]]$.checksum)) {
      ## --> due to invalidation
        ref_chk <- .refs_pull[[ref_uid]]$.checksum      
# message("ref_chk:")        
# print(ref_chk)        
        if (is.null(ref_chk_own) || ref_chk != ref_chk_own) {
        ## --> checksum missing or reference has changed 
        ## --> update               
          if (verbose) {
            message(paste0("Object: ", self$.uid))
            message(paste0("Modified reference: ", ref_uid))
          }
          .updateReferenceChecksum(
            ref = ref_uid, 
            checksum = ref_chk
          )
          do_update <- TRUE
        }
      } else {
      ## Check if 'broken-binding' condition exists //
        if (!is.null(ref_chk_own)) {
        ## --> this can only be the case if there has been a reactive 
        ## binding that was valid/working at one point in time
          if (strict_get == 0) {
            ## Do nothing //
          } else if (strict_get == 1) {                    
            conditionr::signalCondition(
              call = substitute(
                get(x= ID, envir = WHERE, inherits = FALSE),
                list(ID = .id, WHERE = .where)
              ),
              condition = "BrokenReactiveReference",
              msg = c(
                Reason = "broken reactive reference",
                ID = .id,
                UID = .uid,
                Location = capture.output(.where),
                "Reference UID" = ref_uid
              ),
              ns = "reactr",
              type = "warning"
            )
            .value <<- NULL
          } else if (strict_get == 2) {
            cond <- conditionr::signalCondition(
              call = substitute(
                get(x= ID, envir = WHERE, inherits = FALSE),
                list(ID = .id, WHERE = .where)
              ),
              condition = "BrokenReactiveReference",
              msg = c(
                Reason = "broken reactive reference",
                ID = .id,
                UID = .uid,
                Location = capture.output(.where),
                "Reference UID" = ref_uid
              ),
              ns = "reactr",
              type = "error",
              signal = FALSE
            )
            
            ## Transfer condition //
            .condition <<- cond
          }
        }
      }
      return(do_update)
    },
    .computeChecksum = function() {
      chk <- digest::digest(.value)
      .checksum <<- chk
      chk
    },
    .computeUid = function() {
      out <- if (length(.id)) {
# print(.where)        
        .uid <<- eval(substitute(digest::digest(list(id = ID, where = WHERE)), 
          list(ID = .id, WHERE = capture.output(eval(.where)))))
# print(.uid)        
        .uid
      } else {
        character()
      }
      out
    },
    .copy = function(id, where = parent.frame(8)) {
print(ls(where))      
      if (!is.null(.func)) {
        setShinyReactive(id = id, value = .func, where = where)
      } else {
        setShinyReactive(id = id, value = .value, where = where)
      }
    },
    .ensurePullReferencesIntegrity = function(ref_uid) {
      if (exists(ref_uid, envir = .registry, inherits = FALSE)) {     
        assign(ref_uid, 
          get(ref_uid, envir = .registry, inherits = FALSE), 
          envir = .refs_pull
        )      
      }
      
      ## Handle invalidation of referenced objects //
      ## Ensures that "in-object" reference (as compared to the 
      ## "in-registry" reference) is updated once a reference has 
      ## become invalid
      if (  is.null(.refs_pull[[ref_uid]]) ||
            .refs_pull[[ref_uid]]$.is_invalid
      ) {                 
        .refs_pull[[ref_uid]] <- .registry[[ref_uid]]
      }
      
      TRUE
    },
    .getVisible = function() {
      get(.id, .where, inherits = FALSE)
    },
    .hasPullReferences = function() {
      (.has_pull_refs <<- length(ls(.refs_pull, all.names = TRUE)) > 0)
    },
    .hasPushReferences = function() {
      (.has_push_refs <<- length(ls(.refs_push, all.names = TRUE)) > 0)
    },
    .pushToReferences = function(verbose = FALSE) {
      .has_pushed <- FALSE
      .is_running_push <- TRUE
      push_refs_env <- .refs_push
      push_refs <- ls(push_refs_env)
      out <- if (length(push_refs)) {
        sapply(push_refs, function(ref) {
          if (verbose) {
            message(paste0("Pushing to: ", ref))
          }
          push_refs_env[[ref]]$.getVisible()
        })
        TRUE
      } else {
        FALSE
      }
      .is_running_push <- FALSE
      .has_pushed <- TRUE
      out
    },
    .register = function(overwrite = FALSE) {
      out <- if (!exists(.uid, envir = .registry) || overwrite) {     
        assign(.uid, self, envir = .registry)      
        TRUE
      } else {
        FALSE
      }
      out
    },
    .registerPullReferences = function(refs = list(), where = parent.frame(6)) {
      out <- if (length(refs)) {
        .has_pull_refs <<- TRUE
        sapply(refs, function(ref) {
          ref_uid <- computeObjectUid(id = ref$id, where = eval(ref$where))
          if (!exists(ref_uid, envir = .registry)) {
          ## Ensure a valid ref instance exists in registry    
            ref_inst <- Observable3$new(
              id = ref$id, 
              where = eval(ref$where),
              func = NULL
            )
            ref_inst$.computeChecksum()
            ref_inst$.register()
          }
          ## Pointer //
          assign(ref_uid, .registry[[ref_uid]], envir = .refs_pull)
          ## Cached checksums //
          .updateReferenceChecksum(
            ref = ref_uid, 
            checksum = .refs_pull[[ref_uid]]$.checksum
          )
        })
        TRUE
      } else {
        FALSE
      }
      out
    },
    .registerPushReferences = function() {
      out <- if (.hasPullReferences()) {
        sapply(ls(.refs_pull), function(ref_uid) {
          ## Pointer //
          assign(ref_uid, .registry[[ref_uid]], envir = .refs_push)
          if (!exists(.uid, envir = .refs_pull[[ref_uid]]$.refs_push)) {
          ## Ensure a push reference is created //
            assign(.uid, self, .refs_pull[[ref_uid]]$.refs_push)
            .refs_pull[[ref_uid]]$.has_push_refs <- TRUE
            .refs_pull[[ref_uid]]$.must_push <- TRUE
            TRUE
          } else {
            FALSE
          }
        })  
      }
    },
    .remove = function() {
      out <- if (exists(.id, envir = .where)) {
        tryCatch({
            ## Propagate invalidity to dependees //
            .is_invalid <<- TRUE
            .unregister()
            rm(list = .id, envir = .where, inherits = FALSE)        
          },
          error = function(cond) {
            stop("Removal failed")
          }
        )
        TRUE
      } else {
        FALSE
      }
      out
    },
    .unregister = function() {
      out <- if (exists(.uid, envir = .registry)) {
        ## Propagate invalidity to dependees //
        .is_invalid <- TRUE
        rm(list = .uid, envir = .registry, inherits = FALSE)      
        TRUE
      } else {
        FALSE
      }
      out
    },
    .unset = function() {
      if (exists(.id, envir = .where, inherits = FALSE)) {
        has_binding <- try(bindingIsActive(.id, .where))
        if (inherits(has_binding, "try-error")) {
          has_binding <- FALSE
        } 
        if (has_binding) {
          tmp <- get(.id, envir = .where, inherits = FALSE)
          rm(list = .id, envir = .where, inherits = FALSE)
          assign(.id, tmp, .where)
          ## Propagate invalidity to dependees //
          .is_invalid <<- TRUE
          .unregister()
          TRUE
        }
      } else {
        FALSE
      }
    },
    .updateReferenceChecksum = function(ref, checksum) {
      assign(ref, checksum, envir = .refs_checksum)
    }
  )
)

# x <- Observable3$new(id = "abcd", func = function() print("hello world!"))
# x$label

##------------------------------------------------------------------------------
## Functions
##------------------------------------------------------------------------------

exprToFunction2 <- function(
  expr, 
  env = parent.frame(2), 
  quoted = FALSE,
  caller_offset = 1
) {
  
  # Get the quoted expr from two calls back
  expr_sub <- eval(substitute(substitute(expr)), parent.frame(caller_offset))
#   expr_sub <- yamlr::captureExpression(expr = expr, caller_offset = 2)

  ## Retrieve dependencies from YAML markup //
  yaml <- yamlr::processYaml(
    from = if (class(expr_sub) == "{") {
      expr_sub 
    } else {
      eval(expr_sub)
    }, 
    ctx = yamlr::YamlContext.ObjectReference.S3(),
    where = env
  )
  expr_sub <- yaml$src

  # Check if expr is a function, making sure not to evaluate expr, in case it
  # is actually an unquoted expression.
  # If expr is a single token, then indexing with [[ will error; if it has multiple
  # tokens, then [[ works. In the former case it will be a name object; in the
  # latter, it will be a language object.
#   if (  !is.null(expr_sub) && 
#           !is.name(expr_sub) && 
#           (is.function(expr_sub) || expr_sub[[1]] == "function)
#       ) {
#     # Get name of function that called this function
#     called_fun <- sys.call(-1 * caller_offset)[[1]]
# 
#     shiny:::shinyDeprecated(msg = paste("Passing functions to '", called_fun,
#         "' is deprecated. Please use expressions instead. See ?", called_fun,
#         " for more information.", sep=""))
#     return(expr)
#   }

  if (quoted) {
    # expr is a quoted expression
    yaml$src <- shiny:::makeFunction(body = expr, env = env)
  } else {
    # expr is an unquoted expression
    yaml$src <- shiny:::makeFunction(
      body = if (class(expr_sub) == "{") {
        expr_sub 
      } else {
        body(eval(expr_sub))
      }, 
      env = env
    )
  }
  yaml
}

