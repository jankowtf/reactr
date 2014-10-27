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
#' of whom actually only field \code{.value} is visible to the user or other 
#' functions consuming the reactive object.
#' 
#' @template intended-use
#'
#' @param .x \code{\link{ANY}}. An object of an arbitrary class whose class
#'    attribute should be updated so that it becomes an instance of class
#'    \code{ReactiveObject.S3}. Mainly intended for rapid prototyping 
#'    purposes
#'    
#' @field .id \code{\link{character}}.
#'    Object ID.
#'    Initial: \code{character()}.
#' @field .uid \code{\link{character}}.
#'    Object ID.
#'    Initial: \code{character()}. 
#'    Automatically computed once \code{.id} is 
#'    specified: 
#'    \code{digest::digest(list(id = .id, where = capture.output(eval(.where))))}.
#' @field .value \code{\link{ANY}}.
#'    Actual object value / cached value.
#'    Initial: \code{NULL}.
#' @field .where \code{\link{environment}}.
#'    Environment of reactive object.
#'    Initial: \code{parent.frame()}.
#' @field .checksum \code{\link{character}}.
#'    Object value checksum.
#'    Initial: \code{character()}.
#' @field .class \code{\link{character}}.
#'    Class of visible object (\code{.value}). If strongly typed (argument \code{typed = TRUE} in
#'    \code{\link[reactr]{setReactiveS3}}, then this field is used to determine
#'    if an assignment value is valid or not.
#'    Initial: \code{character()}.
#' @field .exists_visible \code{\link{logical}}.
#'    Field for tracking if the visible object actually exists already or if this
#'    is a mere "empty container" in the registry. 
#'    It is set to \code{TRUE} when the visible object is actually set/created
#'    via \code{\link[reactr]{setReactiveS3}}.
#'    Initial: \code{FALSE}.
#' @field .has_cached \code{\link{logical}}.
#'    Field for tracking if the object already has a cached value or not.
#'    If \code{FALSE}, the binding function (if there is any) is executed and 
#'    after that the field is set to \code{TRUE} to signal that a cached value
#'    exists.
#'    Initial: \code{FALSE}.
#' @field .is_invalid \code{\link{logical}}.
#'    Field for propagating the invalidity of referenced objects to its 
#'    dependees. It is set to \code{TRUE} when an reactive object is unset or
#'    removed.
#'    Initial: \code{FALSE}.
#' @field .cache \code{\link{logical}}.
#'    \code{TRUE}: use caching mechanism and everything associated with it;
#'    \code{FALSE}: no caching.
#'    Initial: \code{TRUE}.
#' @field .registry \code{\link{environment}}.
#'    Reference to the registry environment. Important for retrieving and 
#' 		comparing checksum values.
#'    Initial: \code{getRegistry()}.
#' @field .refs_pull \code{\link{environment}}.
#'    Environment storing information of inbound/pull references.
#'    Initial: \code{new.env(parent = emptyenv())}.
#' @field .refs_push \code{\link{environment}}.
#'    Environment storing information of outbound/push references.
#'    Initial: \code{new.env(parent = emptyenv())}.
#' @field .has_pull_refs \code{\link{logical}}.
#'    \code{TRUE}: object has no inbound/pull references;
#'    \code{FALSE}: object has no inbound/pull references
#'    Initial: \code{FALSE}.
#' @field .has_push_refs \code{\link{logical}}.
#'    \code{TRUE}: object has no outbound/push references;
#'    \code{FALSE}: object has no outbound/push references
#'    Initial: \code{FALSE}.
#' @field .must_push \code{\link{logical}}.
#'    Field that controls if push is enabled.
#'    \code{TRUE}: push changes to outbound references;
#'    \code{FALSE}: changes need to be pulled by references, no push.
#'    Initial: \code{FALSE}.
#' @field .has_pushed \code{\link{logical}}.
#'    \code{TRUE}: pushed change to push references;
#'    \code{FALSE}: change not pushed to push references yet.
#'    Initial: \code{FALSE}.
#' @field .is_running_push \code{\link{logical}}.
#'    \code{TRUE}: push process is currently running;
#'    \code{FALSE}: no push process is currently running.
#'    Initial: \code{FALSE}.
#' @field .func \code{\link{function}}.
#'    Binding function.
#'    Initial: \code{NULL}.
#' @field .refs_checksum \code{\link{environment}}.
#'    Environment for caching checksums of referenced objects.
#'    Initial: \code{new.env(parent = emptyenv())}.
#' @field condition \code{\link{condition}} (at least by inheritance).
#'    If a condition has been signaled, this field is assigned a respectiv 
#'    condition object that is triggered when \code{self$.value} is requested.
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
  
  .id = character(),
  .uid = character(),
  .value = NULL,
  .where = parent.frame(),
  .checksum = character(),
  .class = class(.value),
  
  .exists_visible = FALSE,
  .cache = TRUE,
  .has_cached = FALSE,
  .is_invalid = FALSE,
  
  ## Registry //
  .registry = getRegistry(),
  
  ## References //
  .refs_pull = new.env(parent = emptyenv()),
  .refs_push = new.env(parent = emptyenv()),
  .refs_checksum = new.env(parent = emptyenv()),
  .has_pull_refs = FALSE,
  .has_push_refs = FALSE,
  .must_push = FALSE,
  .has_pushed = FALSE,
  .is_running_push = FALSE,
  .func = NULL,
  
  ## Conditions and status messages //
  condition = NULL
  
) {
  if (!missing(.x)) {
    class(.x) <- c("ReactiveObject.S3", class(.x))
    this <- .x
  } else {
    this <- new.env()
    
    ##--------------------------------------------------------------------------
    ## Fields and initialization //
    ##--------------------------------------------------------------------------

    this$.cache <- .cache
    this$.checksum <- .checksum
    this$.refs_checksum <- .refs_checksum
    this$.class <- .class
    this$.condition <- condition
    
    this$.exists_visible <- .exists_visible
    
    this$.func <- .func
    this$.registry <- .registry
    this$.has_cached <- .has_cached
    
    this$.id <- .id
    this$.is_invalid <- .is_invalid
    
    this$.refs_pull <- .refs_pull
    this$.refs_push <- .refs_push
    this$.has_pull_refs <- .has_pull_refs
    this$.has_push_refs <- .has_push_refs
    this$.has_pushed <- .has_pushed
    this$.is_running_push <- .is_running_push 
    this$.must_push <- .must_push
    this$.value <- .value
    this$.where <- .where
    
    ##--------------------------------------------------------------------------
    ## Methods //
    ##--------------------------------------------------------------------------
    
    this$.checkClass <- function(self = this, v) {
      if (!inherits(v, self$.class)) {
        num_clss <- c("integer", "numeric")
        if (all(c(class(v), self$.class) %in% num_clss)) {
          
        } else {
          conditionr::signalCondition(
            call = substitute(
              assign(x= ID, value = VALUE, envir = WHERE, inherits = FALSE),
              list(ID = self$.id, VALUE = v, WHERE = self$.where)
            ),
            condition = "AbortedWithClassError",
            msg = c(
              Reason = "class of assignment value does not inherit from initial class",
              ID = self$.id,
              UID = self$.uid,
              Location = capture.output(self$.where),
              "Class expected" = self$.class,
              "Class provided" = class(v)
            ),
            ns = "reactr",
            type = "error"
          )
        }
      }
    }
    this$.compareChecksums = function(self = this, ref_uid, strict_get = 0,
                                      verbose = FALSE) {
      do_update <- FALSE
      ## Get last-known reference checksum //
      ref_chk_own <- self$.refs_checksum[[ref_uid]]
      if (!is.null(self$.refs_pull[[ref_uid]]$.checksum)) {
      ## --> due to invalidation
        ref_chk <- self$.refs_pull[[ref_uid]]$.checksum                 
        if (is.null(ref_chk_own) || ref_chk != ref_chk_own) {
        ## --> checksum missing or reference has changed 
        ## --> update                    
          if (verbose) {
            message(paste0("Modified reference: ", ref_uid))
          }
          self$.updateReferenceChecksum(
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
                list(ID = self$.id, WHERE = self$.where)
              ),
              condition = "BrokenReactiveReference",
              msg = c(
                Reason = "broken reactive reference",
                ID = self$.id,
                UID = self$.uid,
                Location = capture.output(self$.where),
                "Reference UID" = ref_uid
              ),
              ns = "reactr",
              type = "warning"
            )
            self$.value <- NULL
          } else if (strict_get == 2) {
            cond <- conditionr::signalCondition(
              call = substitute(
                get(x= ID, envir = WHERE, inherits = FALSE),
                list(ID = self$.id, WHERE = self$.where)
              ),
              condition = "BrokenReactiveReference",
              msg = c(
                Reason = "broken reactive reference",
                ID = self$.id,
                UID = self$.uid,
                Location = capture.output(self$.where),
                "Reference UID" = ref_uid
              ),
              ns = "reactr",
              type = "error",
              signal = FALSE
            )
            
            ## Transfer condition //
            self$.condition <- cond
          }
        }
      }
      return(do_update)
    }
    this$.computeChecksum <- function(self = this) {
      (self$.checksum <- digest::digest(self$.value))
    }
    this$.computeUid <- function(self = this) {
      out <- if (length(self$.id)) {
        self$.uid <- eval(substitute(digest::digest(list(id = ID, where = WHERE)), 
          list(ID = self$.id, WHERE = capture.output(eval(self$.where)))))
      } else {
        character()
      }
      out
    }
    this$.copy <- function(self = this, id, where = parent.frame()) {
      if (!is.null(self$.func)) {
        setReactiveS3(id = id, value = self$.func, where = .where)
      } else {
        setReactiveS3(id = id, value = self$.value, where = .where)
      }
    }
    this$.ensurePullReferencesIntegrity = function(ref_uid) {
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
    }
    this$.ensureIntegrity <- function(self = this, ref_uid) {
      if (exists(ref_uid, envir = self$.registry, inherits = FALSE)) {     
        assign(ref_uid, 
          get(ref_uid, envir = self$.registry, inherits = FALSE), 
          envir = self$.refs_pull
        )      
      }
      TRUE
    }
    this$.getVisible <- function(self = this) {
      get(self$.id, self$.where, inherits = FALSE)
    }
    this$.hasPullReferences = function(self = this) {
      (self$.has_pull_refs <- length(ls(self$.refs_pull, all.names = TRUE)) > 0)
    }
    this$.hasPushReferences = function(self = this) {
      (self$.has_push_refs <- length(ls(self$.refs_push, all.names = TRUE)) > 0)
    }
    this$.pushToReferences <- function(self = this, verbose = FALSE) {
      self$.has_pushed <- FALSE
      self$.is_running_push <- TRUE
      push_refs_env <- self$.refs_push
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
      self$.is_running_push <- FALSE
      self$.has_pushed <- TRUE
      out
    }
    this$.register <- function(self = this, overwrite = FALSE) {
      out <- if (!exists(self$.uid, envir = self$.registry) || overwrite) {     
        assign(self$.uid, self, envir = self$.registry)      
        TRUE
      } else {
        FALSE
      }
      out
    }
    this$.registerPullReferences = function(self = this, refs = list(), 
                                            where = .where) {
      out <- if (length(refs)) {
        self$.has_pull_refs <- TRUE
        sapply(refs, function(ref) {
          ref_uid <- computeObjectUid(id = ref$id, where = eval(ref$where))
          if (!exists(ref_uid, envir = self$.registry)) {
          ## Ensure a valid ref instance exists in registry    
            ref_inst <- ReactiveObject.S3(.id = ref$id, .where = eval(ref$where))
            ref_inst$.computeChecksum()
            ref_inst$.register()
          }
          ## Pointer //
          assign(ref_uid, self$.registry[[ref_uid]], envir = self$.refs_pull)
          ## Cached checksums //
          self$.updateReferenceChecksum(
            ref = ref_uid, 
            checksum = self$.refs_pull[[ref_uid]]$.checksum
          )
        })
        TRUE
      } else {
        FALSE
      }
      out
    }
    this$.registerPushReferences = function(self = this) {
      out <- if (self$.hasPullReferences()) {
        sapply(ls(self$.refs_pull), function(ref_uid) {
          ## Pointer //
          assign(ref_uid, .registry[[ref_uid]], envir = self$.refs_push)
          if (!exists(self$.uid, envir = self$.refs_pull[[ref_uid]]$.refs_push)) {
          ## Ensure a push reference is created //
            assign(self$.uid, self, self$.refs_pull[[ref_uid]]$.refs_push)
            self$.refs_pull[[ref_uid]]$.has_push_refs <- TRUE
            self$.refs_pull[[ref_uid]]$.must_push <- TRUE
            TRUE
          } else {
            FALSE
          }
        })  
      }
    }
    this$.remove <- function(self = this) {
      out <- if (exists(self$.id, envir = self$.where)) {
        tryCatch({
            ## Propagate invalidity to dependees //
            self$.is_invalid <- TRUE
            self$.unregister()
            rm(list = self$.id, envir = self$.where, inherits = FALSE)        
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
    this$.unregister <- function(self = this) {
      out <- if (exists(self$.uid, envir = self$.registry)) {
        ## Propagate invalidity to dependees //
        self$.is_invalid <- TRUE
        rm(list = self$.uid, envir = self$.registry, inherits = FALSE)      
        TRUE
      } else {
        FALSE
      }
      out
    }
    this$.unset <- function(self = this) {
      id <- self$.id
      uid <- self$.uid
      where <- self$.where
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
          self$.is_invalid <- TRUE
          self$.unregister()
          TRUE
        }
      } else {
        FALSE
      }
    }
    this$.updateReferenceChecksum <- function(self = this, ref, checksum) {
      assign(ref, checksum, envir = self$.refs_checksum)
    }
    
    ##--------------------------------------------------------------------------
    ## Initialize rest //
    ##--------------------------------------------------------------------------

    this$.computeChecksum()
    this$.computeUid()
    if (this$.cache && length(.references)) {
      this$.registerPullReferences(refs = .references)
    }
    
    class(this) <- c("ReactiveObject.S3", class(this))
  }
  return(this)
}
