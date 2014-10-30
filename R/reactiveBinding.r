#' @title
#' Create Reactive Binding Object
#'
#' @description 
#' Creates a reactive object binding object. This is a slightly adapted form
#' of \code{\link[shiny]{reactive}} in order to make it compatible with being
#' called as an argument of \code{\link[reactr]{setShinyReactive}}.
#'  
#' @param x \code{\link{expression}} (quoted or unquoted).
#' @param env	\code{\link{environment}}.
#'    The parent environment for the reactive expression. 
#'    By default, this is the calling environment, the same as when defining 
#'    an ordinary non-reactive expression.
#' @param quoted \code{\link{logical}}.
#'    \code{TRUE}: expression is quoted;
#'    \code{FALSE}: expression is not quoted.
#'    Default: \code{FALSE}. This is useful when you want to use an expression 
#'    that is stored in a variable; to do so, it must be quoted with \code{quote()}.
#' @param label	\code{\link{character}}.
#'    A label for the reactive expression, useful for debugging.
#' @template threedots
#' @example inst/examples/reactiveBinding.r
#' @seealso \code{
#'     \link[shiny]{reactive}
#' }
#' @template author
#' @template references
#' @import shiny
#' @export 
reactiveBinding <- function(
  x, 
  env = parent.frame(), 
  quoted = FALSE, 
  label = NULL,
  domain = getDefaultReactiveDomain()
) {
  ## Ensure that shiny let's us do this //
  shiny_opt <- getOption("shiny.suppressMissingContextError")
  if (is.null(shiny_opt) || !shiny_opt) {
    options(shiny.suppressMissingContextError = TRUE)  
  }
  
  fun <- exprToFunction(x, env, quoted)
  # Attach a label and a reference to the original user source for debugging
  if (is.null(label))
    label <- sprintf('reactiveBinding(%s)', paste(deparse(body(fun)), collapse='\n'))
  srcref <- attr(substitute(x), "srcref")
  if (length(srcref) >= 2) attr(label, "srcref") <- srcref[[2]]
  attr(label, "srcfile") <- shiny:::srcFileOfRef(srcref[[1]])
  
  out <- new.env(parent = emptyenv())
  out$fun <- fun
  out$label <- label
  out$domain <- domain
#   o <- Observable$new(fun, label, domain)
#   registerDebugHook(".func", o, "Reactive")
  structure(out, class = c("ReactiveBinding", "environment"))
}

# reactiveBinding <- function(
#   x, 
#   env = parent.frame(), 
#   quoted = FALSE, 
#   label = NULL,
#   domain = getDefaultReactiveDomain(),
#   ...
# ) {
#   
#   ## Ensure that shiny let's us do this //
#   shiny_opt <- getOption("shiny.suppressMissingContextError")
#   if (is.null(shiny_opt) || !shiny_opt) {
#     options(shiny.suppressMissingContextError = TRUE)  
#   }
#   
#   yaml <- exprToFunction2(x, env, quoted)
# # yaml_tmp <<- yaml  
#   fun <- yaml$src
# # message("fun:")
# # print(fun)
#   # Attach a label and a reference to the original user source for debugging
#   if (is.null(label))
#     label <- sprintf('reactive(%s)', paste(deparse(body(fun)), collapse='\n'))
#   srcref <- attr(substitute(x), "srcref")
#   if (length(srcref) >= 2) attr(label, "srcref") <- srcref[[2]]
#   attr(label, "srcfile") <- shiny:::srcFileOfRef(srcref[[1]])
#   o <- ReactiveShinyObject$new(
#     id = id, 
#     where = env,
#     refs_pull = yaml$parsed,
#     func = fun, 
#     label = label, 
#     domain = domain
#   )
# # o <<- o
#   shiny:::registerDebugHook(".func", o, "Reactive")
# 
#   ## Some preparations //
#   o$.class <- class(o$.value)
# 
#   ## Push //
#   if (push) {
#     o$.registerPushReferences()
#   }
#   
# # o <<- o
# #   stop("intentionalStop")
#   makeActiveBinding(
#     id,
#     env = env,
#     fun = local({
#       o
#       function(v) {
#         if (missing(v)) {
#           
#           ##--------------------------------------------------------------------
#           ## Handler for 'get' (i.e. 'get()' or '{obj-name}' or '${obj-name}) //
#           ##--------------------------------------------------------------------   
#           
#           if (o$.hasPullReferences()) {
#             needs_update <- sapply(ls(o$.refs_pull), function(ref_uid) {
#               ## Ensure integrity //
#               if (integrity) {
#                 o$.ensurePullReferencesIntegrity(ref_uid = ref_uid)
#               }
#               ## Compare checksums //
#               (needs_update <- o$.compareChecksums(
#                 ref_uid = ref_uid, 
#                 strict_get = strict_get
#               ))
#             })
#           } else {
#             needs_update <- FALSE
#           }
#         
#           ##----------------------------------------------------------------
#           ## Actual update or initial caching //
#           ##----------------------------------------------------------------
#   
#           if (any(needs_update) || !o$.has_cached) {
#             if (!o$.has_cached) {
#               message("Initializing ...")  
#             }
#             if (any(needs_update)) {
#               message("Updating ...")  
#             }
#             
#             ## Cache new value //
#             o$.value <<- withRestarts(
#               tryCatch(
#                 {     
#                   out <- o$getValue()
#                   o$.condition <<- NULL
#                   o$.has_cached <<- TRUE
#                   out 
#                 ## For debugging/testing purposes 
#   #                     stop("Intentional update fail"),
#                 },
#                 warning = function(cond) {
#                   invokeRestart("muffleWarning")
#                 },
#                 error = function(cond) {
#                   invokeRestart("ReactiveUpdateFailed", cond = cond)
#                 }
#               ),
#               muffleWarning = function(cond) {
#                 message(cond)
#                 invokeRestart("muffleWarning")
#               },
#               ReactiveUpdateFailed = function(cond) {
#                 ## Custom condition //
#                 cond <- conditionr::signalCondition(
#                   condition = "AbortedReactiveUpdateWithError",
#                   msg = c(
#                     "Update failed",
#                     Reason = conditionMessage(cond),
#                     ID = o$.id,
#                     UID = o$.uid,
#                     Location = capture.output(o$.where)
#                   ),
#                   ns = "reactr",
#                   type = "error",
#                   signal = FALSE
#                 )
#                 ## Transfer condition //
#                 o$.condition <<- cond
#                 NULL
#               }
#             )
#             ## Update fields //
#             o$.computeChecksum()
#           }
#         } else {
#         
#         ##--------------------------------------------------------------------
#         ## Handler for 'set' (i.e. 'assign()' or '<-') //
#         ##--------------------------------------------------------------------   
#           
#         message("----- set (reactive) -----")
#         
#         if (typed) {
#           o$.checkClass(v = v)
#         }
#         
#         ## Set //
#         if (o$.hasPullReferences()) {
#           if (strict_set == 0) {
#             o$.value <<- v    
#           } else if (strict_set == 1) {
#             ## Do nothing //
#           } else if (strict_set == 2) {
#             conditionr::signalCondition(
#               call = substitute(
#                 assign(x= ID, value = VALUE, envir = WHERE, inherits = FALSE),
#                 list(ID = o$.id, VALUE = v, WHERE = o$.where)
#               ),
#               condition = "AbortedWithReactiveDependencyWarning",
#               msg = c(
#                 Reason = "trying to set value of object with reactive dependency",
#                 ID = o$.id,
#                 UID = o$.uid,
#                 Location = capture.output(o$.where),
#                 References = paste(ls(o$.refs_pull, all.names = TRUE), collapse = ", ")
#               ),
#               ns = "reactr",
#               type = "warning"
#             )
#           } else if (strict_set == 3) {
#             conditionr::signalCondition(
#               call = substitute(
#                 assign(x= ID, value = VALUE, envir = WHERE, inherits = FALSE),
#                 list(ID = o$.id, VALUE = v, WHERE = o$.where)
#               ),
#               condition = "AbortedWithReactiveDependencyError",
#               msg = c(
#                 Reason = "trying to set value of object with reactive dependency",
#                 ID = o$.id,
#                 UID = o$.uid,
#                 Location = capture.output(o$.where),
#                 References = paste(ls(o$.refs_pull, all.names = TRUE), collapse = ", ")
#               ),
#               ns = "reactr",
#               type = "error"
#             )
#           }
#         } else {
#           o$.value <<- v 
#         }
#         
#         ## Update checksum //
#         o$.computeChecksum()
#         
#         ## Push //
#         if (  o$.must_push &&
#               o$.hasPushReferences() && 
#               !o$.has_pushed && 
#               !o$.is_running_push
#         ) {
#           o$.pushToReferences()
#           ## Reset value of push control field //
#           o$.has_pushed <- FALSE
#         }
#       }
# 
#       ##------------------------------------------------------------------------
#       ## Return //
#       ##------------------------------------------------------------------------
# 
#       ## Condition handling //
#       if (!is.null(o$.condition)) {           
#         if (inherits(o$.condition, "BrokenReactiveReference")) {
#           o$.value <- stop(o$.condition)
#         } else {            
#           o$.value <- stop(o$.condition)
#         }
#       }
#       o$.value
#     }
#   })
# )
# #   structure(o$getValue, observable = o, class = "reactive2")
# #   structure(o$.cache, observable = o, class = "reactive2")
# #   return(o$.value)
# 
#   ## Initialize //
#   (out <- get(id, envir = env, inherits = FALSE))
#   
# }
