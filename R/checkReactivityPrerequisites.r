#' @title
#' Check Prerequisits for Setting Reactive Objects (generic)
#'
#' @description 
#' Checks prerequisites to determine if its valid to set the reactive object as
#' desired. This decision is influenced by the level of strictness that is 
#' imposed via \code{strict}.
#'   	
#' @param input \strong{Signature argument}.
#'    Object containing a suitable object for the check. 
#'    Typically, this corresponds to the instance of class 
#'    \code{\link[reactr]{ReactiveObject.S3}}.
#' @param strict \code{\link{numeric}}.
#'    Relevant when initially setting a reactive object
#'    \itemize{
#'      \item{\code{0}: } {no checks are performed}
#'      \item{\code{1}: } {warning if object is already a non-reactive or 
#'      reactive object or if any references does not exist yet}
#'      \item{\code{2}: } {error if object is already a non-reactive or 
#'      reactive object or if any references do not exist yet}
#'    }
#' @template threedots
#' @example inst/examples/checkReactivityPrerequisites.r
#' @seealso \code{
#'   	\link[reactr]{checkReactivityPrerequisites-ReactiveObject.S3-method},
#'     \link[reactr]{setReactiveS3}
#' }
#' @template author
#' @template references
setGeneric(
  name = "checkReactivityPrerequisites",
  signature = c(
    "input"
  ),
  def = function(
    input = NULL,
    strict = 0,
    ...
  ) {
    standardGeneric("checkReactivityPrerequisites")       
  }
)

#' @title
#' Check Prerequisits for Setting Reactive Objects (ReactiveShinyObject) 
#'
#' @description 
#' See generic: \code{\link[reactr]{checkReactivityPrerequisites}}
#'      
#' @inheritParams checkReactivityPrerequisites
#' @param input \code{\link{ReactiveShinyObject}}.
#' @return See method 
#'    \code{\link[reactr]{checkReactivityPrerequisites-ReactiveObject.S3-method}}. 
#' @example inst/examples/checkReactivityPrerequisites.r
#' @seealso \code{
#'    \link[reactr]{checkReactivityPrerequisites},
#'     \link[reactr]{setReactiveS3}
#' }
#' @template author
#' @template references
#' @export
#' @aliases checkReactivityPrerequisity-ReactiveShinyObject-method
setMethod(
  f = "checkReactivityPrerequisites", 
  signature = signature(
    input = "ReactiveShinyObject"
  ), 
  definition = function(
    input,
    strict,
    ...
  ) {
    
  mthd <- selectMethod(f = "checkReactivityPrerequisites",
               signature = c(input = "ReactiveObject.S3"))  
  mthd(
    input = input, 
    strict = strict, 
    ...
  )
    
  }
)

#' @title
#' Check Prerequisits for Setting Reactive Objects (ReactiveObject.S3) 
#'
#' @description 
#' See generic: \code{\link[reactr]{checkReactivityPrerequisites}}
#'      
#' @inheritParams checkReactivityPrerequisites
#' @param input \code{\link{ReactiveObject.S3}}.
#' @return \code{\link{logical}}. 
#'    \code{TRUE}: check passed; 
#'    \code{FALSE}: check not passed.
#' @example inst/examples/checkReactivityPrerequisites.r
#' @seealso \code{
#'    \link[reactr]{checkReactivityPrerequisites},
#'     \link[reactr]{setReactiveS3}
#' }
#' @template author
#' @template references
#' @aliases checkReactivityPrerequisity-ReactiveObject.S3-method
#' @import conditionr
#' @export
setMethod(
  f = "checkReactivityPrerequisites", 
  signature = signature(
    input = "ReactiveObject.S3"
  ), 
  definition = function(
    input,
    strict,
    ...
  ) {
    
  ## Argument checks //
  strict <- as.numeric(match.arg(as.character(strict), 
                                 as.character(c(0, 1, 2))))
    
  out <- FALSE
  
  ## Check self //
  idx_exist <- exists(input$.id, input$.where, inherits = FALSE)
  has_binding <- try(bindingIsActive(input$.id, input$.where), silent = TRUE)
  if (inherits(has_binding, "try-error")) {
    has_binding <- FALSE
  } 

  if (idx_exist && !has_binding) {
    if (strict == 0) {
      out <- TRUE
    } else if (strict == 1) {
      conditionr::signalCondition(
        condition = "ReactivityPrerequisitesNotMetButOverwrite",
        msg = c(
          Reason = "already a non-reactive object",
          Action = "overwrite existing object",
          ID = input$.id,
          UID = input$.uid,
          Location = capture.output(input$.where)
        ),
        ns = "reactr",
        type = "warning"
      )
      out <- TRUE
    } else if (strict == 2) {
      conditionr::signalCondition(
        condition = "ReactivityPrerequisitesNotMet",
        msg = c(
          Reason = "already a non-reactive object",
          Action = "exit with error",
          ID = input$.id,
          UID = input$.uid,
          Location = capture.output(input$.where)
        ),
        ns = "reactr",
        type = "error"
      )
    }
  } else if (idx_exist && has_binding) {
    if (strict == 0) {
      out <- TRUE
    } else if (strict == 1) {
      conditionr::signalCondition(
        condition = "ReactivityPrerequisitesNotMetButOverwrite",
        msg = c(
          Reason = "already an reactive object",
          Action = "existing object is overwritten",
          ID = input$.id,
          UID = input$.uid,
          Location = capture.output(input$.where)
        ),
        ns = "reactr",
        type = "warning"
      )
      out <- TRUE
    } else if (strict == 2) {
      conditionr::signalCondition(
        condition = "ReactivityPrerequisitesNotMet",
        msg = c(
          Reason = "already an reactive object",
          Action = "exit with error",
          ID = input$.id,
          UID = input$.uid,
          Location = capture.output(input$.where)
        ),
        ns = "reactr",
        type = "error"
      )
    }
  } else if (!has_binding) {
    if (strict == 0) {
      out <- TRUE
    } else if (strict == 1) {
      conditionr::signalCondition(
        condition = "ReactivityPrerequisitesNotMetButOverwrite",
        msg = c(
          Reason = "object does not exist yet",
          Action = "object is created",
          ID = input$.id,
          UID = input$.uid,
          Location = capture.output(input$.where)
        ),
        ns = "reactr",
        type = "warning"
      )
    } else if (strict == 2) {
      conditionr::signalCondition(
        condition = "ReactivityPrerequisitesNotMet",
        msg = c(
          Reason = "object does not exist yet",
          Action = "exit with error",
          ID = input$.id,
          UID = input$.uid,
          Location = capture.output(input$.where)
        ),
        ns = "reactr",
        type = "error"
      )
    } 
  }
  
  if (out && idx_exist) {
    rm(list = input$.id, envir = input$.where, inherits = TRUE)
  }

  ## Check pull references //
  out_ref <- all(sapply(ls(input$.refs_pull), function(ref_uid) {
    ref <- get(ref_uid, input$.refs_pull, inherits = FALSE)
    idx_exist <- exists(ref$.id, ref$.where, inherits = FALSE)
# message("idx_exist:")    
# print(idx_exist)    
    out <- FALSE
    if (!idx_exist) {
      if (strict == 0) {
        out <- TRUE
        input$.has_cached <- TRUE
        ## --> this prevents the binding function being executed "too early",
        ## i.e. in cases where not all refs exist yet. 
        ## Possibly think about how to solve this in a more encapsulated or
        ## in a "more obvious" way with respect to maintainability.
      } else if (strict == 1) {
        conditionr::signalCondition(
          condition = "ReactivityPrerequisitesNotMetButOverwrite",
          msg = c(
            Reason = "reference does not exist yet",
            Action = "exit with warning",
            ID = input$.id,
            UID = input$.uid,
            Location = capture.output(input$.where),
            "Reference ID" = ref$.id,
            "Reference UID" = ref$.uid,
            "Reference location" = ref$.where
          ),
          ns = "reactr",
          type = "warning"
        )
      } else if (strict == 2) {
        conditionr::signalCondition(
          condition = "ReactivityPrerequisitesNotMet",
          msg = c(
            Reason = "reference does not exist yet",
            Action = "exit with error",
            ID = input$.id,
            UID = input$.uid,
            Location = capture.output(input$.where),
            "Reference ID" = ref$.id,
            "Reference UID" = ref$.uid,
            "Reference location" = ref$.where
          ),
          ns = "reactr",
          type = "error"
        )
      } 
    }
    out
  }))
  
  out <- all(c(out, out_ref))
  
  return(out)
  
  }
)
