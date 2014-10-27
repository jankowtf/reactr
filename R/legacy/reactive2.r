##------------------------------------------------------------------------------
## Classes
##------------------------------------------------------------------------------

# #' @import R6
# #' @import shiny
# Observable2 <- R6Class(
#   'Observable2',
#   inherit = shiny:::Observable,
#   portable = FALSE,
#   public = list(
#     ## [@change: jat, start]
#     .checksum = character(0),
#     .checksums = "environment",
#     .class = character(0),
#     .condition = NULL,
#     .id = character(0),
#     
#     .registry = "environment", 
#     .refs_pull = "environment",
#     .refs_push = "environment",
#     .refs_checksum = "environment",
#     
#     .uid = character(0),
# #     .where = parent.frame(6),
#     .where = "environment",
# 
#     ## Questions //
#     .exists_visible = TRUE,
#     .has_cached = FALSE,
#     .has_pull_refs = FALSE,
#     .has_push_refs = FALSE,
#     .has_pushed = FALSE,
#     .is_invalid = FALSE,
#     .is_running_push = FALSE,
#     .must_push = FALSE,
#     .needs_update = TRUE,
#     ## [@change: jat, end]
# #     .func = 'function',
# #     .label = character(0),
# #     .domain = NULL,
# #     .dependents = 'Dependents',
# #     .invalidated = logical(0),
# #     .running = logical(0),
# #     .value = NULL,
# #     .visible = logical(0),
# #     .execCount = integer(0),
# #     .mostRecentCtxId = character(0),
# 
#     initialize = function(
#       id, 
#       where = parent.frame(6), 
#       ## --> Corresponds to environment from which `Observable$new()` is called,
#       refs_pull = list(),
#       func = NULL, 
#       label = deparse(substitute(func)),
#       domain = shiny:::getDefaultReactiveDomain()
#     ) {
#       if (length(formals(func)) > 0)
#         stop("Can't make a reactive expression from a function that takes one ",
#           "or more parameters; only functions without parameters can be ",
#           "reactive.")
#       .func <<- func
#       .label <<- label
#       .domain <<- domain
#       .dependents <<- shiny:::Dependents$new()
#       .invalidated <<- TRUE
#       .running <<- FALSE
#       .execCount <<- 0L
#       .mostRecentCtxId <<- ""
#       ## [@change: jat, start]
# #       print(ls(.where))
#       .id <<- id
#       .where <<- where
#       .registry <<- getRegistry()
#       .checksums <<- new.env(parent = emptyenv())
#       .refs_pull <<- new.env(parent = emptyenv())
#       .refs_push <<- new.env(parent = emptyenv())
#       .refs_checksum <<- new.env(parent = emptyenv())
#       .needs_update <<- TRUE
# 
#       ## Other initialization steps //
#       .uid <<- .computeUid()
#       .register(overwrite = TRUE)
#       if (length(refs_pull)) {
#         .registerPullReferences(refs = refs_pull, where = .where)
#       }
#       if (.uid %in% ls(.refs_checksum)) {    
#         conditionr::signalCondition(
#           condition = "NoSelfReferenceAllowed",
#           msg = c(
#             Reason = "tried to set a self-reference",
#             ID = .id,
#             UID = .uid,
#             Location = capture.output(.where)
#           ),
#           ns = "reactr",
#           type = "error"
#         )
#       }
# print("initialization")
#       ## [@change: jat, end]
#     },
#     getValue = function() {
# print("getValue()/start")      
#       .dependents$.register()
#       ## [@change: jat, start]
# #       message("Observable2$getValue()/.dependents$.dependents$keys():")
# #       tmp_keys <- .dependents$.dependents$keys()
# #       print(tmp_keys)
# #       message("Observable2$getValue()/.dependents$.dependents$values():")
# #       print(.dependents$.dependents$values())
# #       message("Observable2$getValue()/.dependents$.dependents$values()/first value:")
# #       print(ls(.dependents$.dependents$values()[[tmp_keys[[1]]]]))
# #       message("Observable2$getValue()/.dependents$.dependents$private$env:")
# #       print(ls(.dependents$.dependents$private$env))
# 
#       ## HERE:
#       ## Here would have to go a comparison of all hash values of dependents
#       ## in order to determine if an update is neccessary or not.
#       ## The check result would go into variable/field '.needs_update'
#       ## which in turn would then also considered inside the 'if()' part:
#       ##          if (.invalidated || .running || .needs_update)
#       ## Pseudo:
#       .checkIfUpdateIsNeeded <- function() {
# #        inst$getHash()
#         TRUE
#       }
#       .needs_update <- .checkIfUpdateIsNeeded()
# 
#       if (.invalidated || .running || .needs_update) {
#         self$.updateValue()
#       }
#       ## [@change: jat, end]
#       shiny:::.graphDependsOnId(getCurrentContext()$id, .mostRecentCtxId)
# 
#       if (identical(class(.value), 'try-error'))
#         stop(attr(.value, 'condition'))
# print("getValue()/end")
#       if (.visible)
#         .value
#       else
#         invisible(.value)
#     },
#     .updateValue = function() {
#       ctx <- shiny:::Context$new(.domain, .label, type = 'observable',
#         prevId = .mostRecentCtxId)
# #      message("Observable2$.updateValue()/ctx:")
# #      print(ctx)
# #      print(ls(ctx))
#       .mostRecentCtxId <<- ctx$id
#       ctx$onInvalidate(function() {
#           .invalidated <<- TRUE
#           .dependents$invalidate()
#         })
#       .execCount <<- .execCount + 1L
# #      message("Observable2$.updateValue()/.execCount:")
# #      print(.execCount)
#       .invalidated <<- FALSE
# 
#       wasRunning <- .running
#       .running <<- TRUE
#       on.exit(.running <<- wasRunning)
# print("updateValue()/before")
#       ctx$run(function() {
#           result <- withVisible(try(shiny:::shinyCallingHandlers(.func()), silent=TRUE))
#           .visible <<- result$visible
#           .value <<- result$value
#         })
# print("updateValue()/after")  
#     },
#     ## {@change: jt, start}
#     .checkClass = function(v) {
#       if (!inherits(v, .class)) {
#         num_clss <- c("integer", "numeric")
#         if (all(c(class(v), .class) %in% num_clss)) {
#           
#         } else {
#           conditionr::signalCondition(
#             call = substitute(
#               assign(x= ID, value = VALUE, envir = WHERE, inherits = FALSE),
#               list(ID = .id, VALUE = v, WHERE = .where)
#             ),
#             condition = "AbortedWithClassError",
#             msg = c(
#               Reason = "class of assignment value does not inherit from initial class",
#               ID = .id,
#               UID = .uid,
#               Location = capture.output(.where),
#               "Class expected" = .class,
#               "Class provided" = class(v)
#             ),
#             ns = "reactr",
#             type = "error"
#           )
#         }
#       }
#     },
#     .compareChecksums = function(ref_uid, strict_get = 0) {
#       do_update <- FALSE
#       ## Get last-known reference checksum //
#       ref_chk_own <- .refs_checksum[[ref_uid]]
#       if (!is.null(.refs_pull[[ref_uid]]$.checksum)) {
#       ## --> due to invalidation
#         ref_chk <- .refs_pull[[ref_uid]]$.checksum                 
#         if (is.null(ref_chk_own) || ref_chk != ref_chk_own) {
#         ## --> checksum missing or reference has changed 
#         ## --> update                    
#           message(paste0("Modified reference: ", ref_uid))
#           .updateReferenceChecksum(
#             ref = ref_uid, 
#             checksum = ref_chk
#           )
#           do_update <- TRUE
#         }
#       } else {
#       ## Check if 'broken-binding' condition exists //
#         if (!is.null(ref_chk_own)) {
#         ## --> this can only be the case if there has been a reactive 
#         ## binding that was valid/working at one point in time
#           if (strict_get == 0) {
#             ## Do nothing //
#           } else if (strict_get == 1) {                    
#             conditionr::signalCondition(
#               call = substitute(
#                 get(x= ID, envir = WHERE, inherits = FALSE),
#                 list(ID = .id, WHERE = .where)
#               ),
#               condition = "BrokenReactiveReference",
#               msg = c(
#                 Reason = "broken reactive reference",
#                 ID = .id,
#                 UID = .uid,
#                 Location = capture.output(.where),
#                 "Reference UID" = ref_uid
#               ),
#               ns = "reactr",
#               type = "warning"
#             )
#             .value <<- NULL
#           } else if (strict_get == 2) {
#             cond <- conditionr::signalCondition(
#               call = substitute(
#                 get(x= ID, envir = WHERE, inherits = FALSE),
#                 list(ID = .id, WHERE = .where)
#               ),
#               condition = "BrokenReactiveReference",
#               msg = c(
#                 Reason = "broken reactive reference",
#                 ID = id,
#                 UID = .uid,
#                 Location = capture.output(.where),
#                 "Reference UID" = ref_uid
#               ),
#               ns = "reactr",
#               type = "error",
#               signal = FALSE
#             )
#             
#             ## Transfer condition //
#             .condition <<- cond
#           }
#         }
#       }
#       return(do_update)
#     },
#     .computeChecksum = function() {
#       chk <- digest::digest(.value)
#       .checksum <<- chk
#       chk
#     },
#     .computeUid = function() {
#       out <- if (length(.id)) {
# print(.where)        
#         .uid <<- eval(substitute(digest::digest(list(id = ID, where = WHERE)), 
#           list(ID = .id, WHERE = capture.output(eval(.where)))))
# print(.uid)        
#         .uid
#       } else {
#         character()
#       }
#       out
#     },
#     .ensurePullReferencesIntegrity = function(ref_uid) {
#       if (exists(ref_uid, envir = .registry, inherits = FALSE)) {     
#         assign(ref_uid, 
#           get(ref_uid, envir = .registry, inherits = FALSE), 
#           envir = .refs_pull
#         )      
#       }
#       
#       ## Handle invalidation of referenced objects //
#       ## Ensures that "in-object" reference (as compared to the 
#       ## "in-registry" reference) is updated once a reference has 
#       ## become invalid
#       if (  is.null(.refs_pull[[ref_uid]]) ||
#             .refs_pull[[ref_uid]]$.is_invalid
#       ) {                 
#         .refs_pull[[ref_uid]] <- .registry[[ref_uid]]
#       }
#       
#       TRUE
#     },
#     .getVisible = function() {
#       get(.id, .where, inherits = FALSE)
#     },
#     .hasPullReferences = function() {
#       (.has_pull_refs <<- length(ls(.refs_pull, all.names = TRUE)) > 0)
#     },
#     .hasPushReferences = function() {
#       (.has_push_refs <<- length(ls(.refs_push, all.names = TRUE)) > 0)
#     },
#     .pushToReferences = function() {
#       .has_pushed <- FALSE
#       .is_running_push <- TRUE
#       push_refs_env <- .refs_push
#       push_refs <- ls(push_refs_env)
#       out <- if (length(push_refs)) {
#         sapply(push_refs, function(ref) {
#           message(paste0("Pushing to: ", ref))
#           push_refs_env[[ref]]$.getVisible()
#         })
#         TRUE
#       } else {
#         FALSE
#       }
#       .is_running_push <- FALSE
#       .has_pushed <- TRUE
#       out
#     },
#     .register = function(overwrite = FALSE) {
#       out <- if (!exists(.uid, envir = .registry) || overwrite) {     
#         assign(.uid, self, envir = .registry)      
#         TRUE
#       } else {
#         FALSE
#       }
#       out
#     },
#     .registerPullReferences = function(refs = list(), where = parent.frame(6)) {
#       out <- if (length(refs)) {
#         .has_pull_refs <<- TRUE
#         sapply(refs, function(ref) {
#           ref_uid <- computeObjectUid(id = ref$id, where = eval(ref$where))
#           if (!exists(ref_uid, envir = .registry)) {
#           ## Ensure a valid ref instance exists in registry    
#             ref_inst <- Observable2$new(id = ref$id, where = eval(ref$where))
#             ref_inst$.computeChecksum()
#             ref_inst$.register()
#           }
#           ## Pointer //
#           assign(ref_uid, .registry[[ref_uid]], envir = .refs_pull)
#           ## Cached checksums //
#           .updateReferenceChecksum(
#             ref = ref_uid, 
#             checksum = .refs_pull[[ref_uid]]$.checksum
#           )
#         })
#         TRUE
#       } else {
#         FALSE
#       }
#       out
#     },
#     .registerPushReferences = function() {
#       out <- if (.hasPullReferences()) {
#         .must_push <- TRUE
# #         .has_push_refs <- TRUE
#         sapply(ls(.refs_pull), function(ref_uid) {
#           ## Pointer //
#           assign(ref_uid, .registry[[ref_uid]], envir = .refs_push)
#           if (!exists(.uid, envir = .refs_pull[[ref_uid]]$.refs_push)) {
#           ## Ensure a push reference is created //
#             assign(.uid, self, .refs_pull[[ref_uid]]$.refs_push)
# #             .refs_pull[[ref_uid]]$.has_push_refs <- TRUE
#             TRUE
#           } else {
#             FALSE
#           }
#         })  
#       }
#     },
#     .updateReferenceChecksum = function(ref, checksum) {
#       assign(ref, checksum, envir = .refs_checksum)
#     }
#     ## {@change: jt, end}
#   )
# )

#' @title
#' Set Reactive Shiny Object with Extended Features
#'
#' @description 
#' Creates an reactive object.
#'  
#' @param id \code{\link{character}}.
#'    Name of the object to set.
#' @template threedots
#' @example inst/examples/setReactiveS3.r
#' @seealso \code{
#'     \link[reactr]{setReactiveS3}
#' }
#' @template author
#' @template references
#' @export 
#' @import shiny
reactive2 <- function(
  x, 
  env = parent.frame(), 
  quoted = FALSE, 
  label = NULL,
  domain = getDefaultReactiveDomain(), 
  ## JAT //
  id,
  integrity = TRUE,
  push = FALSE,
  typed = FALSE
) {
  
  ## Ensure that shiny let's us do this //
  shiny_opt <- getOption("shiny.suppressMissingContextError")
  if (is.null(shiny_opt) || !shiny_opt) {
    options(shiny.suppressMissingContextError = TRUE)  
  }
  
  yaml <- exprToFunction2(x, env, quoted)
# yaml_tmp <<- yaml  
  fun <- yaml$src
# message("fun:")
# print(fun)
  # Attach a label and a reference to the original user source for debugging
  if (is.null(label))
    label <- sprintf('reactive(%s)', paste(deparse(body(fun)), collapse='\n'))
  srcref <- attr(substitute(x), "srcref")
  if (length(srcref) >= 2) attr(label, "srcref") <- srcref[[2]]
  attr(label, "srcfile") <- shiny:::srcFileOfRef(srcref[[1]])
  o <- Observable3$new(
    id = id, 
    where = env,
    refs_pull = yaml$parsed,
    func = fun, 
    label = label, 
    domain = domain
  )
# o <<- o
  shiny:::registerDebugHook(".func", o, "Reactive")

  ## Some preparations //
  o$.class <- class(o$.value)

  ## Push //
  if (push) {
    o$.registerPushReferences()
  }
  
# o <<- o
#   stop("intentionalStop")
  makeActiveBinding(
    id,
    env = env,
    fun = local({
      o
      function(v) {
        if (missing(v)) {
          
          ##--------------------------------------------------------------------
          ## Handler for 'get' (i.e. 'get()' or '{obj-name}' or '${obj-name}) //
          ##--------------------------------------------------------------------   
          
          if (o$.hasPullReferences()) {
            needs_update <- sapply(ls(o$.refs_pull), function(ref_uid) {
              ## Ensure integrity //
              if (integrity) {
                o$.ensurePullReferencesIntegrity(ref_uid = ref_uid)
              }
              ## Compare checksums //
              (needs_update <- o$.compareChecksums(
                ref_uid = ref_uid, 
                strict_get = strict_get
              ))
            })
          } else {
            needs_update <- FALSE
          }
        
          ##----------------------------------------------------------------
          ## Actual update or initial caching //
          ##----------------------------------------------------------------
  
          if (any(needs_update) || !o$.has_cached) {
            if (!o$.has_cached) {
              message("Initializing ...")  
            }
            if (any(needs_update)) {
              message("Updating ...")  
            }
            
            ## Cache new value //
            o$.value <<- withRestarts(
              tryCatch(
                {     
                  out <- o$getValue()
                  o$.condition <<- NULL
                  o$.has_cached <<- TRUE
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
                ## Custom condition //
                cond <- conditionr::signalCondition(
                  condition = "AbortedReactiveUpdateWithError",
                  msg = c(
                    "Update failed",
                    Reason = conditionMessage(cond),
                    ID = o$.id,
                    UID = o$.uid,
                    Location = capture.output(o$.where)
                  ),
                  ns = "reactr",
                  type = "error",
                  signal = FALSE
                )
                ## Transfer condition //
                o$.condition <<- cond
                NULL
              }
            )
            ## Update fields //
            o$.computeChecksum()
          }
        } else {
        
        ##--------------------------------------------------------------------
        ## Handler for 'set' (i.e. 'assign()' or '<-') //
        ##--------------------------------------------------------------------   
          
        message("----- set (reactive) -----")
        
        if (typed) {
          o$.checkClass(v = v)
        }
        
        ## Set //
        if (o$.hasPullReferences()) {
          if (strict_set == 0) {
            o$.value <<- v    
          } else if (strict_set == 1) {
            ## Do nothing //
          } else if (strict_set == 2) {
            conditionr::signalCondition(
              call = substitute(
                assign(x= ID, value = VALUE, envir = WHERE, inherits = FALSE),
                list(ID = o$.id, VALUE = v, WHERE = o$.where)
              ),
              condition = "AbortedWithReactiveDependencyWarning",
              msg = c(
                Reason = "trying to set value of object with reactive dependency",
                ID = o$.id,
                UID = o$.uid,
                Location = capture.output(o$.where),
                References = paste(ls(o$.refs_pull, all.names = TRUE), collapse = ", ")
              ),
              ns = "reactr",
              type = "warning"
            )
          } else if (strict_set == 3) {
            conditionr::signalCondition(
              call = substitute(
                assign(x= ID, value = VALUE, envir = WHERE, inherits = FALSE),
                list(ID = o$.id, VALUE = v, WHERE = o$.where)
              ),
              condition = "AbortedWithReactiveDependencyError",
              msg = c(
                Reason = "trying to set value of object with reactive dependency",
                ID = o$.id,
                UID = o$.uid,
                Location = capture.output(o$.where),
                References = paste(ls(o$.refs_pull, all.names = TRUE), collapse = ", ")
              ),
              ns = "reactr",
              type = "error"
            )
          }
        } else {
          o$.value <<- v 
        }
        
        ## Update checksum //
        o$.computeChecksum()
        
        ## Push //
        if (  o$.must_push &&
              o$.hasPushReferences() && 
              !o$.has_pushed && 
              !o$.is_running_push
        ) {
          o$.pushToReferences()
          ## Reset value of push control field //
          o$.has_pushed <- FALSE
        }
      }

      ##------------------------------------------------------------------------
      ## Return //
      ##------------------------------------------------------------------------

      ## Condition handling //
      if (!is.null(o$.condition)) {           
        if (inherits(o$.condition, "BrokenReactiveReference")) {
          o$.value <- stop(o$.condition)
        } else {            
          o$.value <- stop(o$.condition)
        }
      }
      o$.value
    }
  })
)
#   structure(o$getValue, observable = o, class = "reactive2")
#   structure(o$.cache, observable = o, class = "reactive2")
#   return(o$.value)

  ## Initialize //
  (out <- get(id, envir = env, inherits = FALSE))
  
}
