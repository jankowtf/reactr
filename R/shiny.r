##------------------------------------------------------------------------------
## Classes
##------------------------------------------------------------------------------

#' @title
#' Class: ReactrObservable
#'
#' @description
#' Class that implements major parts of the reactivity mechanism of this 
#' package.
#' 
#' @details
#' Clone of \code{shiny:::Observable}. Only necessary because not all class
#' components of \code{shiny:::Observable} are exported.
#' 
#' @section Fields:
#' 
#' The fields correspond to the fields of \code{shiny:::Observable}.

#' @example inst/examples/ReactrObserver.r
#' @seealso \code{
#'     \link[reactr]{ReactiveShinyObject},
#'     \link[reactr]{setShinyReactive},
#'     \link[shiny]{reactive},
#'     \link[shiny]{makeReactiveBinding},
#'     \link[reactr]{setReactive}
#' }
#' @template author
#' @template references
#' @import R6
#' @export 
ReactrObservable <- R6Class(
  'ReactrObservable',
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
        result <- withVisible(try(shiny:::shinyCallingHandlers(.func()), silent=TRUE))
        .visible <<- result$visible
        .value <<- result$value
      })
    }
  )
)

#' @title
#' Class: ReactiveShinyObject
#'
#' @description
#' Class that implements major parts of the reactivity mechanism of this 
#' package.
#' 
#' @details
#' Extends class \code{shiny::Observable}, or, to be more precise its
#' clone \code{\link[reactr]{ReactrObservable}}.
#' 
#' @field .cache \code{\link{logical}}.
#'    \code{TRUE}: use caching mechanism and everything associated with it;
#'    \code{FALSE}: no caching.
#'    Initial: \code{TRUE}.
#' @field .caller \code{\link{environment}}.
#'    Referenced environment corresponding to instance of 
#'    \code{\link[reactr]{SetShinyReactive}} that calls a reference (which can
#'    also be the object itself).
#'    Very important in order to fine tune update processes for bi-directional
#'    bindings.
#'    Default: \code{self} (after initialization).
#' @field .checksum \code{\link{character}}.
#'    Checksum of visible object value.
#'    Initial: \code{character()}.
#' @field .class \code{\link{character}}.
#'    Class of visible object value (\code{.value}). 
#'    If strongly typed (argument \code{typed = TRUE} in
#'    \code{\link[reactr]{setShinyReactive}}, then this field is used 
#'    to determine if an assignment value is valid or not.
#'    Initial: \code{character()}.
#' @field condition \code{\link{condition}} (at least by inheritance).
#'    If a condition has been signaled, this field is assigned a respective 
#'    custom condition object that is triggered when the visible object value
#'    (or \code{self$.value}) is requested.
#'    Also see \code{\link[base]{signalCondition}} and 
#'    \code{\link[conditionr]{signalCondition}}
#'    Initial: \code{NULL}.
#' @field .id \code{\link{character}}.
#'    Object ID.
#'    Initial: \code{character()}.
#' @field .refs_checksum \code{\link{environment}}.
#'    Environment for storing the cached checksums of pull references.
#'    Initial: \code{new.env(parent = emptyenv())}.
#' @field .refs_pull \code{\link{environment}}.
#'    Environment for storing information of inbound/pull references.
#'    Initial: \code{new.env(parent = emptyenv())}.
#' @field .refs_push \code{\link{environment}}.
#'    Environment for storing information of outbound/push references.
#'    Initial: \code{new.env(parent = emptyenv())}.
#' @field .registry \code{\link{environment}}.
#'    Reference to the registry environment 
#'    (see \code{\link[reactr]{getRegistry}}. 
#'    Important for retrieving and comparing checksum values, enabling push
#'    and other useful things (integrity checks etc.)
#'    Initial: \code{getRegistry()}.
#' @field .uid \code{\link{character}}.
#'    Object ID.
#'    Initial: \code{character()}. 
#'    Automatically computed once \code{.id} is 
#'    specified: 
#'    \code{digest::digest(list(id = .id, where = capture.output(eval(.where))))}.
#' @field .where \code{\link{environment}}.
#'    Environment of reactive object.
#'    Initial: \code{parent.frame()}.
#' @field .exists_visible \code{\link{logical}}.
#'    Field for tracking if the visible object value actually exists already 
#'    or if this is a mere "empty container" in the registry. 
#'    It is set to \code{TRUE} when the visible object is actually set/created
#'    via \code{\link[reactr]{setShinyReactive}}.
#'    Initial: \code{FALSE}.
#' @field .has_bidir \code{\link{logical}}.
#'    Field for signaling that an instance has bi-directional references to
#'    other objects. \strong{Very} important in order to carry out updates for 
#'    bi-directional bindings correctly.
#'    A system-wide check for the existence of bi-directional references is 
#'    run via \code{$.hasBidirectional} inside 
#'    \code{\link[reactr]{setShinyReactive}}. The field is set to \code{TRUE}
#'    if there are any, else it remains \code{FALSE}.
#'    Initial: \code{FALSE}.
#' @field .has_cached \code{\link{logical}}.
#'    Field for tracking if the instance already has a cached value or not.
#'    If \code{FALSE}, the binding function (if there is any) is executed and 
#'    after that the field is set to \code{TRUE} to signal that a cached value
#'    exists.
#'    Initial: \code{FALSE}.
#' @field .has_pull_refs \code{\link{logical}}.
#'    \code{TRUE}: instance has inbound/pull references;
#'    \code{FALSE}: instance has no inbound/pull references
#'    Initial: \code{FALSE}.
#' @field .has_push_refs \code{\link{logical}}.
#'    \code{TRUE}: instance has outbound/push references;
#'    \code{FALSE}: instance has no outbound/push references
#'    Initial: \code{FALSE}.
#' @field .has_pushed \code{\link{logical}}.
#'    \code{TRUE}: change has been pushed to all push references;
#'    \code{FALSE}: change has not been pushed to push references yet.
#'    Initial: \code{FALSE}.
#' @field .is_modcycle_complete \code{\link{logical}}.
#'    \code{TRUE}: modification cycle complete;
#'    \code{FALSE}: modification cycle not complete yet.
#'    Only relevant for bi-directional bindings and in case of explicitly
#'    changing visible object values via \code{\link[base]{<-}} or 
#'    \code{\link[base]{assign}}. Very important to determine the scope of 
#'    object updates.
#'    Initial: \code{TRUE}.
#' @field .is_invalid \code{\link{logical}}.
#'    Field for propagating the invalidity of referenced objects to its 
#'    referencees. It is set to \code{TRUE} when an reactive object is unset or
#'    removed.
#'    Initial: \code{FALSE}.
#' @field .is_running_push \code{\link{logical}}.
#'    \code{TRUE}: push process is currently running;
#'    \code{FALSE}: no push process is currently running.
#'    Initial: \code{FALSE}.
#' @field .must_push \code{\link{logical}}.
#'    Field that controls if push is enabled.
#'    \code{TRUE}: push changes to outbound/push references;
#'    \code{FALSE}: changes need to be \emph{pulled} by objects referencing this
#'    instance, no push.
#'    Initial: \code{FALSE}.
#' @field .needs_update \code{\link{logical}}.
#'    Field that controls if update is to be carried out.
#'    \code{TRUE}: update;
#'    \code{FALSE}: no update, use cached value
#'    Initial: \code{TRUE} (in order to trigger initialization).
#' @example inst/examples/ReactiveShinyObject.r
#' @seealso \code{
#'     \link[reactr]{ReactiveShinyObject},
#'     \link[reactr]{setShinyReactive},
#'     \link[shiny]{reactive},
#'     \link[shiny]{makeReactiveBinding},
#'     \link[reactr]{setReactive}
#' }
#' @template author
#' @template references
#' @import conditionr
#' @import R6
#' @export 
ReactiveShinyObject <- R6Class(
  'ReactiveShinyObject',
  inherit = ReactrObservable,
  portable = FALSE,
  public = list(
    .cache = TRUE,
    .caller = "environment",
    .checksum = character(0),
    .class = character(0),
    .condition = NULL,
    .id = character(0),
    
    .refs_checksum = "environment",
    .refs_pull = "environment", 
    .refs_push = "environment", 
    .registry = "environment", 
    
    .uid = character(0),
#     .where = parent.frame(6),
    .where = "environment",

    ## Questions //
    .exists_visible = TRUE,
    .has_bidir = FALSE,
    .has_cached = FALSE,
    .has_pull_refs = FALSE,
    .has_push_refs = FALSE,
    .has_pushed = FALSE,
    .is_modcycle_complete = TRUE,
    .is_invalid = FALSE,
    .is_running_push = FALSE,
    .must_push = FALSE,
    .needs_update = TRUE,
    initialize = function(
      id, 
      value = NULL,
      where = parent.frame(6), 
      ## --> Corresponds to environment from which `ReactrObservable$new()` is called,
      refs_pull = list(),
#       func = NULL, 
      cache = TRUE,
      ...
    ) {
      super$initialize(...)
      .cache <<- cache
      .id <<- id
      .refs_checksum <<- new.env(parent = emptyenv())
      .refs_pull <<- new.env(parent = emptyenv())
      .refs_push <<- new.env(parent = emptyenv())
      .registry <<- getRegistry()
#      .needs_update <<- TRUE
      .needs_update <<- FALSE
      .value <<- value
      .where <<- where

      ## Other initialization steps //
      .caller <- self
      .class <- class(.value)
      .computeChecksum()
      .computeUid()
      if (.cache && length(refs_pull)) {
        .registerPullReferences(refs = refs_pull, where = .where)
      }
      if (.cache && length(.uid)) {
        .register(overwrite = TRUE)
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
      ## Check for bi-directional references //
      .hasBidirectional(system_wide = TRUE)
    },
    ## Should further updates be blocked as they would lead to inconsitencies
    ## with respect to the **expected** values of bi-directional references
    ## when at least one of them has been explicitly modified via `<-`
    .blockUpdate = function(verbose = FALSE) {
      out <- FALSE
      if (  self$.has_bidir && 
            self$.caller$.uid != self$.uid && 
            self$.caller$.is_modcycle_complete
      ) {
      ## Only relevant for bi-directional relationships:
      ## --> for cetain systems constellations, it is necessary that 
      ## certain updates are blocked as they would lead to 
      ## inconsistencies with respect to **expected** object values
      ## after bi-directionally referenced objects have been 
      ## explicitly modified (i.e. `<-` has been used).
      ## Distinction:
      ## 1) When **only** A has changed and B is requested 
      ##    **before** A:
      ##    --> block update cycle at the point where A is called 
      ##        by B in order to avoid the re-setting of A to 
      ##        the **old** (but at this point still current) value 
      ##        of B (which would happen if the full update cycle 
      ##        was carried out).
      ## 2) When **both** A and B have changed **before** 
      ##    any object has been requested and one of them is 
      ##    requested **afterwards**:
      ##    --> carry out full update cycle so the system recognizes
      ##        the last value that was explicitly set 
        if (verbose) {
          message("Intentional update block to ensure consistency")
        }
        self$.is_modcycle_complete <- TRUE
        out <- TRUE
      }
      out
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
      ref_chk_last <- .refs_checksum[[ref_uid]]
      if (!is.null(.refs_pull[[ref_uid]]$.checksum)) {
      ## --> due to invalidation
        ref_chk_current <- .refs_pull[[ref_uid]]$.checksum      
        if (is.null(ref_chk_last) || ref_chk_current != ref_chk_last) {
        ## --> checksum missing or reference has changed 
        ## --> update               
          .updateReferenceChecksum(
            ref = ref_uid, 
            checksum = ref_chk_current
          )
          ## Who is calling //
          self$.refs_pull[[ref_uid]]$.caller <- self
          
          if (verbose) {
            message(paste0("Object: ", self$.uid))
            message(paste0("Called by: ", self$.caller$.uid))
            message(paste0("Modified reference: ", ref_uid))
            message(paste0("\t- Checksum last: ", ref_chk_last))
            message(paste0("\t- Checksum current: ", ref_chk_current))
          }
          
          do_update <- TRUE
        }
      } else {
      ## Check if 'broken-binding' condition exists //
        if (!is.null(ref_chk_last)) {
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
        .uid <<- eval(substitute(digest::digest(list(id = ID, where = WHERE)), 
          list(ID = .id, WHERE = capture.output(eval(.where)))))
        .uid
      } else {
        character()
      }
      out
    },
    .copy = function(id, where = parent.frame(8)) {  
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
    .hasBidirectional = function(system_wide = FALSE) {
      uid <- self$.uid
      refs_pull <- self$.refs_pull
      (self$.has_bidir <- any(sapply(ls(refs_pull), function(ref_uid) {
        true <-  uid %in% ls(refs_pull[[ref_uid]]$.refs_pull)
        if (system_wide && true && !refs_pull[[ref_uid]]$.has_bidir) {
        ## --> system wide and not carried out yet in reference          
          self$.has_bidir <- true
          ## --> necessary to immediately make that information available 
          ## system-wide
          refs_pull[[ref_uid]]$.ensurePullReferencesIntegrity(ref_uid = uid)
          refs_pull[[ref_uid]]$.hasBidirectional(system_wide = system_wide)
        }
        true
      })))
    },
    .hasPullReferences = function() {
      (.has_pull_refs <<- length(ls(.refs_pull, all.names = TRUE)) > 0)
    },
    .hasPushReferences = function() {
      (.has_push_refs <<- length(ls(.refs_push, all.names = TRUE)) > 0)
    },
    .isCallerModcycleComplete = function(caller_uid = self$.caller$.uid) {
      self$.registry[[caller_uid]]$.is_modcycle_complete
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
            ref_inst <- ReactiveShinyObject$new(
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

##------------------------------------------------------------------------------
## Functions
##------------------------------------------------------------------------------

#' @import yamlr
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

