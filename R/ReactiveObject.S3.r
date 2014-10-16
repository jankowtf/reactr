#' @title
#' Class: ReactiveObject.S3 
#'
#' @description
#' Class representing the system state (S3) and its constructor function.
#' 
#' @details
#' Instances of this class are implicitly created when calling 
#' \code{\link[reactr]{setReactiveS3}} and stored in the registry. Objects of 
#' this class can be thought of as the "hidden parts" of the reactive objects 
#' of whom actually only field \code{value} is visible to the user or other 
#' functions consuming the reactive object.
#' 
#' @template intended-use
#'
#' @param .x \code{\link{ANY}}. An object of an arbitrary class whose class
#'    attribute should be updated so that it becomes an instance of class
#'    \code{ReactiveObject.S3}. Mainly intended for rapid prototyping 
#'    purposes
#'    
#' @field id \code{\link{character}}.
#'    Object ID.
#'    Initial: \code{character()}.
#' @field uid \code{\link{character}}.
#'    Object ID.
#'    Initial: \code{character()}. 
#'    Automatically computed once \code{id} is 
#'    specified: 
#'    \code{digest::digest(list(id = id, where = capture.output(eval(where))))}.
#' @field value \code{\link{ANY}}.
#'    Actual object value / cached value.
#'    Initial: \code{NULL}.
#' @field where \code{\link{environment}}.
#'    Environment of reactive object.
#'    Initial: \code{parent.frame()}.
#' @field checksum \code{\link{character}}.
#'    Object value checksum.
#'    Initial: \code{character()}.
#' @field exists_visible \code{\link{logical}}.
#'    Field for tracking if the visible object actually exists already or if this
#'    is a mere "empty container" in the registry. 
#'    It is set to \code{TRUE} when the visible object is actually set/created
#'    via \code{\link[reactr]{setReactiveS3}}.
#'    Initial: \code{FALSE}.
#' @field has_cached \code{\link{logical}}.
#'    Field for tracking if the object already has a cached value or not.
#'    If \code{FALSE}, the binding function (if there is any) is executed and 
#'    after that the field is set to \code{TRUE} to signal that a cached value
#'    exists.
#'    Initial: \code{FALSE}.
#' @field is_invalid \code{\link{logical}}.
#'    Field for propagating the invalidity of referenced objects to its 
#'    dependees. It is set to \code{TRUE} when an reactive object is unset or
#'    removed.
#'    Initial: \code{FALSE}.
#' @field registry \code{\link{environment}}.
#'    Reference to the registry environment. Important for retrieving and 
#' 		comparing checksum values.
#'    Initial: \code{getRegistry()}.
#' @field references \code{\link{environment}}.
#'    Environment storing information of referenced objects.
#'    Initial: \code{new.env(parent = emptyenv())}.
#' @field func \code{\link{function}}.
#'    Binding function.
#'    Initial: \code{NULL}.
#' @field checksums_ref \code{\link{environment}}.
#'    Environment for caching checksums of referenced objects.
#'    Initial: \code{new.env(parent = emptyenv())}.
#' @field condition \code{\link{condition}} (at least by inheritance).
#'    If a condition has been signaled, this field is assigned a respectiv 
#'    condition object that is triggered when \code{.self$value} is requested.
#'    See \code{\link[base]{signalCondition}} and 
#'    \code{\link[conditionr]{signalCondition}}
#'    Initial: \code{NULL}.
#' @return Instance of class \code{ReactiveObject.S3}.
#' @example inst/examples/ReactiveObject.S3.r
#' @seealso \code{
#'   	\link[reactr]{setReactiveS3}
#' }
#' @template author
#' @template references
#' @export
#' @import digest
ReactiveObject.S3 <- function(
  .x,
  .references = character(),
  
  id = character(),
  uid = character(),
  value = NULL,
  where = parent.frame(),
  checksum = character(),
  
  exists_visible = FALSE,
  has_cached = FALSE,
  is_invalid = FALSE,
  
  ## Registry //
  registry = getRegistry(),
  
  ## References //
  references = new.env(parent = emptyenv()),
  func = NULL,
  checksums_ref = new.env(parent = emptyenv()),
  
  ## Conditions and status messages //
  condition = NULL
  
) {
  if (!missing(.x)) {
    class(.x) <- c("ReactiveObject.S3", class(.x))
    this <- .x
  } else {
    this <- new.env()
    
    ## Fields //
    this$checksum <- checksum
    this$checksums_ref <- checksums_ref
    this$condition <- condition
    
    this$exists_visible <- exists_visible
    
    this$func <- func
    this$registry <- registry
    this$has_cached <- has_cached
    
    this$id <- id
    this$is_invalid <- is_invalid
    
    this$references <- references
    this$value <- value
    this$where <- where
    
    
    ## Methods //
    this$computeChecksum <- function(self = this) {
      cs <- digest::digest(self$value)
      self$checksum <- cs
      cs
    }
    this$computeUid <- function(self = this) {
      out <- if (length(id)) {
        self$uid <- eval(substitute(digest::digest(list(id = ID, where = WHERE)), 
          list(ID = self$id, WHERE = capture.output(eval(self$where)))))
        self$uid
      } else {
        character()
      }
      out
    }
    this$copy <- function(self = this, id, where = parent.frame()) {
#       print(ls(where))
      if (length(id)) {
        if (!is.null(self$func)) {
          setReactiveS3(id = id, value = self$func, where = where)
        } else {
          setReactiveS3(id = id, value = self$value, where = where)
        }
        TRUE
      } else {
        FALSE
      }
    }
    this$hasReferences <- function(self = this) {
      length(ls(self$references, all.names = TRUE)) > 0
    }
    this$register <- function(self = this) {
      out <- if (!exists(self$uid, envir = self$registry)) {     
        assign(self$uid, self, envir = self$registry)      
        TRUE
      } else {
        FALSE
      }
      out
    }
    this$registerReferences <- function(self = this, references = list()) {
      out <- if (length(references)) {
        sapply(references, function(ref) {
          if (!exists(ref, envir = self$registry)) {
          ## Ensure a valid ref instance exists in registry            
            ref_inst <- ReactiveObject.S3(
              id = ref,
              checksum = digest::digest(NULL)
            )      
            ref_inst$register(self = ref_inst)
          }
          ## Pointer //
          assign(ref, self$registry[[ref]], envir = self$references)
          ## Cached checksums //
          this$updateReferenceChecksum(
            ref = ref, 
            checksum = self$references[[ref]]$checksum
          )
        })
        TRUE
      } else {
        FALSE
      }
      out
    }
    this$remove <- function(self = this) {
      out <- if (exists(self$id, envir = self$where)) {
        tryCatch({
            ## Propagate invalidity to dependees //
            self$is_invalid <- TRUE
            self$unregister()
            rm(list = self$id, envir = self$where, inherits = FALSE)        
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
    }
    this$unregister <- function(self = this) {
      out <- if (exists(self$uid, envir = self$registry)) {
        ## Propagate invalidity to dependees //
        self$is_invalid <- TRUE
        rm(list = self$uid, envir = self$registry, inherits = FALSE)      
        TRUE
      } else {
        FALSE
      }
      out
    }
    this$unset <- function(self = this) {
      id <- self$id
      uid <- self$uid
      where <- self$where
      if (exists(id, envir = where, inherits = FALSE)) {
        has_binding <- try(bindingIsActive(id, where))
        if (inherits(has_binding, "try-error")) {
          has_binding <- FALSE
        } 
        if (has_binding) {
          tmp <- get(id, envir = where, inherits = FALSE)
          rm(list = id, envir = where, inherits = FALSE)
          assign(id, tmp, where)
          ## Propagate invalidity to dependees //
          self$is_invalid <- TRUE
          self$unregister()
          TRUE
        }
      } else {
        FALSE
      }
    }
    this$updateReferenceChecksum <- function(self = this, ref, checksum) {
      assign(ref, checksum, envir = self$checksums_ref)
    }
    

    ## Initialize //
    this$computeChecksum()
    this$computeUid()
    if (length(.references)) {
      this$registerReferences(references = .references)
    }
    
    class(this) <- c("ReactiveObject.S3", class(this))
  }
  return(this)
}
