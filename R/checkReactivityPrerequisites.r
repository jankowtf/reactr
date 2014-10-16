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
#' @export
#' @aliases checkReactivityPrerequisity-ReactiveObject.S3-method
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
  idx_exist <- exists(input$id, input$where, inherits = FALSE)
  has_binding <- try(bindingIsActive(input$id, input$where), silent = TRUE)
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
          ID = input$id,
          UID = input$uid,
          Location = capture.output(input$where)
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
          ID = input$id,
          UID = input$uid,
          Location = capture.output(input$where)
        ),
        ns = "reactr",
        type = "error"
      )
    }
  } else if (has_binding) {
    if (strict == 0) {
      out <- TRUE
    } else if (strict == 1) {
      conditionr::signalCondition(
        condition = "ReactivityPrerequisitesNotMetButOverwrite",
        msg = c(
          Reason = "already an reactive object",
          Action = "existing object is overwritten",
          ID = input$id,
          UID = input$uid,
          Location = capture.output(input$where)
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
          ID = input$id,
          UID = input$uid,
          Location = capture.output(input$where)
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
          ID = input$id,
          UID = input$uid,
          Location = capture.output(input$where)
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
          ID = input$id,
          UID = input$uid,
          Location = capture.output(input$where)
        ),
        ns = "reactr",
        type = "error"
      )
    } 
  }
  
  if (out && idx_exist) {
    rm(list = input$id, envir = input$where, inherits = TRUE)
  }
  
  return(out)
  
  }
)
