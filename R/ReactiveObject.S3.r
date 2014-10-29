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
#' @param pull_refs_list \code{\link{list}}. 
#' 		List of pull references as returned by functions that identify 
#' 		pull references (e.g. \code{\link[yamlr]{processYaml}}.
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
#'    Checksum of visible value.
#'    Initial: \code{character()}.
#' @field .class \code{\link{character}}.
#'    Class of visible object (\code{.value}). If strongly typed (argument \code{typed = TRUE} in
#'    \code{\link[reactr]{setReactiveS3}}, then this field is used to determine
#'    if an assignment value is valid or not.
#'    Initial: \code{character()}.
#' @field .exists_visible \code{\link{logical}}.
#'    Field for tracking if the visible object value actually exists already 
#'    or if this is a mere "empty container" in the registry. 
#'    It is set to \code{TRUE} when the visible object is actually set/created
#'    via \code{\link[reactr]{setReactiveS3}}.
#'    Initial: \code{FALSE}.
#' @field .has_cached \code{\link{logical}}.
#'    Field for tracking if the instance already has a cached value or not.
#'    If \code{FALSE}, the binding function (if there is any) is executed and 
#'    after that the field is set to \code{TRUE} to signal that a cached value
#'    exists.
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
#'    dependees. It is set to \code{TRUE} when an reactive object is unset or
#'    removed.
#'    Initial: \code{FALSE}.
#' @field .cache \code{\link{logical}}.
#'    \code{TRUE}: use caching mechanism and everything associated with it;
#'    \code{FALSE}: no caching.
#'    Initial: \code{TRUE}.
#' @field .registry \code{\link{environment}}.
#'    Reference to the registry environment 
#'    (see \code{\link[reactr]{getRegistry}}. 
#'    Important for retrieving and comparing checksum values, enabling push
#'    and other useful things (integrity checks etc.)
#'    Initial: \code{getRegistry()}.
#' @field .refs_pull \code{\link{environment}}.
#'    Environment for storing information of inbound/pull references.
#'    Initial: \code{new.env(parent = emptyenv())}.
#' @field .refs_push \code{\link{environment}}.
#'    Environment for storing information of outbound/push references.
#'    Initial: \code{new.env(parent = emptyenv())}.
#' @field .has_pull_refs \code{\link{logical}}.
#'    \code{TRUE}: object has inbound/pull references;
#'    \code{FALSE}: object has no inbound/pull references
#'    Initial: \code{FALSE}.
#' @field .has_push_refs \code{\link{logical}}.
#'    \code{TRUE}: object has outbound/push references;
#'    \code{FALSE}: object has no outbound/push references
#'    Initial: \code{FALSE}.
#' @field .must_push \code{\link{logical}}.
#'    Field that controls if push is enabled.
#'    \code{TRUE}: push changes to outbound references;
#'    \code{FALSE}: changes need to be pulled by references, no push.
#'    Initial: \code{FALSE}.
#' @field .has_pushed \code{\link{logical}}.
#'    \code{TRUE}: change has been pushed to all push references;
#'    \code{FALSE}: change has not been pushed to push references yet.
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
#'    If a condition has been signaled, this field is assigned a respective 
#'    custom condition object that is triggered when the visible object value
#'    (or \code{self$.value}) is requested.
#'    Also see \code{\link[base]{signalCondition}} and 
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
  pull_refs_list = character(),
  
  id = character(),
  uid = character(),
  value = NULL,
  where = parent.frame(),
  checksum = character(),
  cl = class(value),
  
  exists_visible = FALSE,
  cache = TRUE,
  has_cached = FALSE,
  is_invalid = FALSE,
  
  ## Registry //
  registry = getRegistry(),
  
  ## References //
  refs_pull = new.env(parent = emptyenv()),
  refs_push = new.env(parent = emptyenv()),
  refs_checksum = new.env(parent = emptyenv()),
  has_pull_refs = FALSE,
  has_push_refs = FALSE,
  must_push = FALSE,
  has_pushed = FALSE,
  is_running_push = FALSE,
  func = NULL,
  
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

    this$.cache <- cache
    this$.caller <- character()
    this$.checksum <- checksum
    this$.refs_checksum <- refs_checksum
    this$.class <- cl
    this$.condition <- condition
    
    this$.exists_visible <- exists_visible
    
    this$.func <- func
    this$.registry <- registry
    this$.has_cached <- has_cached
    
    this$.id <- id
    this$.is_invalid <- is_invalid
    
    this$.refs_pull <- refs_pull
    this$.refs_push <- refs_push
    this$.has_bidir <- FALSE
    this$.has_pull_refs <- has_pull_refs
    this$.has_push_refs <- has_push_refs
    this$.has_pushed <- has_pushed
    this$.is_modcycle_complete <- TRUE
    this$.is_running_push <- is_running_push 
    this$.must_push <- must_push
    this$.needs_update <- FALSE
    this$.value <- value
    this$.where <- where
    
    ##--------------------------------------------------------------------------
    ## Methods //
    ##--------------------------------------------------------------------------
    
    ## Should further updates be blocked as they would lead to inconsitencies
    ## with respect to the **expected** values of bi-directional references
    ## when at least one of them has been explicitly modified via `<-`
    this$.blockUpdate <- function(self = this, verbose = FALSE) {
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
    }
    this$.checkClass <- function(self = this, v) {
      if (self$.class != "NULL" && !inherits(v, self$.class)) {
      ## --> seems like enforcing class consistency for `NULL` situations
      ## does not make sense as `NULL` will probably only be used for initial
      ## values. Possible think about what should happen when `NULL` is the 
      ## **assignment** value instead of the initial value --> issue #22
## TODO: issue #22
        
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
      ref_chk_last <- self$.refs_checksum[[ref_uid]]    
      if (!is.null(self$.refs_pull[[ref_uid]]$.checksum)) {
      ## --> due to invalidation
        ref_chk_current <- self$.refs_pull[[ref_uid]]$.checksum                 
        if (is.null(ref_chk_last) || ref_chk_current != ref_chk_last) {
        ## --> checksum missing or reference has changed 
        ## --> update      
          self$.updateReferenceChecksum(
            ref = ref_uid, 
            checksum = ref_chk_current
          )
## TODO: issue #21

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
        setReactiveS3(id = id, value = self$.func, where = where)
      } else {
        setReactiveS3(id = id, value = self$.value, where = where)
      }
    }
    this$.ensurePullReferencesIntegrity = function(self = this, ref_uid) {
      if (exists(ref_uid, envir = self$.registry, inherits = FALSE)) {     
        assign(ref_uid, 
          get(ref_uid, envir = self$.registry, inherits = FALSE), 
          envir = self$.refs_pull
        )      
      }
      
      ## Handle invalidation of referenced objects //
      ## Ensures that "in-object" reference (as compared to the 
      ## "in-registry" reference) is updated once a reference has 
      ## become invalid
      if (  is.null(self$.refs_pull[[ref_uid]]) ||
            self$.refs_pull[[ref_uid]]$.is_invalid
      ) {                 
        self$.refs_pull[[ref_uid]] <- self$.registry[[ref_uid]]
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
    this$.hasBidirectional = function(self = this, system_wide = FALSE) {
      uid <- self$.uid
      refs_pull <- self$.refs_pull
      (self$.has_bidir <- any(sapply(ls(refs_pull), function(ref_uid) {
        true <-  uid %in% ls(refs_pull[[ref_uid]]$.refs_pull)
# message("bidirectional //")        
# message("uid:")
# print(uid)
# message("pull refs in ref:")
# print(ls(refs_pull[[ref_uid]]$.refs_pull))
# message("true:")
# print(true)
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
    }
    this$.hasPullReferences = function(self = this) {
      (self$.has_pull_refs <- length(ls(self$.refs_pull, all.names = TRUE)) > 0)
    }
    this$.hasPushReferences = function(self = this) {
      (self$.has_push_refs <- length(ls(self$.refs_push, all.names = TRUE)) > 0)
    }
    this$.isCallerModcycleComplete = function(self = this, caller_uid = self$.caller$.uid) {
      self$.registry[[caller_uid]]$.is_modcycle_complete
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
                                            where = self$.where) {
      out <- if (length(refs)) {
        self$.has_pull_refs <- TRUE
        sapply(refs, function(ref) {
          ref_uid <- computeObjectUid(id = ref$id, where = eval(ref$where))
          if (!exists(ref_uid, envir = self$.registry)) {
          ## Ensure a valid ref instance exists in registry    
            ref_inst <- ReactiveObject.S3(id = ref$id, where = eval(ref$where))
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
          assign(ref_uid, self$.registry[[ref_uid]], envir = self$.refs_push)
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
#           tmp <- get(id, envir = where, inherits = FALSE)
          tmp <- self$.value
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

    this$.caller <- this
    this$.class <- class(this$.value)
    this$.computeChecksum()
    this$.computeUid()
    
    if (this$.cache && length(pull_refs_list)) {
      this$.registerPullReferences(refs = pull_refs_list)
    }
    if (this$.cache && length(this$.uid)) {
      this$.register(overwrite = TRUE)
    }
    
    ## Catch self-reference situations //
    if (length(this$.uid) && this$.uid %in% ls(this$.refs_checksum)) {    
      conditionr::signalCondition(
        condition = "NoSelfReferenceAllowed",
        msg = c(
          Reason = "tried to set a self-reference",
          ID = this$.id,
          UID = this$.uid,
          Location = capture.output(this$.where)
        ),
        ns = "reactr",
        type = "error"
      )
    }
    ## Check for bi-directional references //
    this$.hasBidirectional(system_wide = TRUE)
    
    class(this) <- c("ReactiveObject.S3", class(this))
  }
  return(this)
}
