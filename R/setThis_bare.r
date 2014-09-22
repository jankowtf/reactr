#' @title
#' Set Value (bare)
#'
#' @description 
#' Sets a variable value in an environment.
#' 
#' @details
#' If \code{binding_type = 1} (the current default), then the 
#' actual binding is established via \code{\link[base]{makeActiveBinding}}.
#' The actual binding relationship is provided via a simple function of the 
#' form \code{function(x) {## Do something with 'x'}} via argument \code{binding}. 
#' For standard binding cases (simple monitoring and mutual bindings), you 
#' don't need to provided any explicit binding function via \code{binding}. 
#' The function takes care of setting it to \code{function(x) {x}}.
#' 
#' @note
#' There also exists an S4 method
#' \code{\link[reactr]{setThis-character-ANY-environment-character-environment-call-method}}.
#' It is about 10 - 15 % slower than this bare S3 function.
#'     
#' @param id \code{\link{character}}.
#'    Name of the variable to set.
#' @param value \code{\link{ANY}}.
#'    Variable value.
#' @param where \code{\link{environment}}.
#'    Environment to set the variable in.
#' @param watch \\code{\link{character}}.
#'    Name of a variable to be monitored
#' @param binding \code{\link{call}}  
#'    Function that defines the binding relationship between \code{id} and 
#'    \code{watch}. See details.
#' @param binding_type \code{\link{integer}} or \code{\link{numerical}}.
#' \itemize{
#'    \item{\code{1}:} {
#'    use \code{\link{makeActiveBinding}} to establish the binding
#'    }
#'    \item{\code{2}:} {
#'    use custom way of establishing the binding
#'    }
#' }
#' @param mutual \code{\link{logical}}.
#'    \code{TRUE}: mutual binding contract;
#'    \code{FALSE}: binding contract depends on \code{watch} being specified
#'    or not (specified: \code{monitoring} contract; 
#'    not specified: \emph{monitored} contract)
#' @param .hash_id \code{\link{character}}.
#'    Name of the auxiliary environment for caching hash values. 
#'    Default: \code{"._HASH"}. Keep it unless this name is already taken in 
#'    either \code{where} or \code{where_watch}.
#' @param .tracelevel \code{\link{numeric}}.
#'    Verbosity level for tracing purposes. Value of \code{0} means 
#'    \emph{no tracing} whereas values of \code{> 0} can be used to fine 
#'    control tracing. The trace level can also be set as a global option when
#'    using package \code{tracer} (\strong{not functional yet}).
#' @template threedot
#' @example inst/examples/setThis_bare.r
#' @seealso \code{
#'   	\link[reactr]{setThis-character-ANY-environment-character-call-method}
#' }
#' @template author
#' @template references
#' @export 
setThis_bare <- function(
    id,
    value = NULL,
    where = .GlobalEnv,
    watch = character(),
    binding = substitute(expression()),
    binding_type = 1,
    mutual = FALSE,
    where_watch = where,
    .hash_id = "._HASH",
    .tracelevel = 0,
#     .binding = NULL,
    ...
  ) {

  ## Validate binding type //
#   binding_type <- as.numeric(match.arg(as.character(binding_type), c("1", "2")))
    binding_type
  
  ## Check location identiy //
  identical_where <- identical(where, where_watch)  
    
  ## Explicit handling of specific binding contracts //
  .binding <- NULL
  if (is.function(binding)) {
    .binding <- binding
    if (!mutual) {
      binding <- getBoilerplateCode(ns = classr::createInstance(
        cl = "Reactr.BindingContractMonitoring.S3"))
    } else {
      binding <- getBoilerplateCode(ns = classr::createInstance(
        cl = "Reactr.BindingContractMutual.S3"))
    }
  }    
    
  specific_binding <- !deparse(binding)[1] %in% c("expression()", 
    "substitute(expression())")
    
  ## Binding interface //
  if (!specific_binding && binding_type == 1) {
  ## Default "set-only" binding contract //      
    if (!length(watch)) {
      binding <- getBoilerplateCode(ns = classr::createInstance(
        cl = "Reactr.BindingContractMonitored.S3"))
    } else {
      ## Variables that binding boilerplate needs to find //
      if (is.null(.binding)) {
        .binding <- function(x) {x}
      }
      if (length(value)) {
        warning(paste0("Variable has monitoring binding --> disregarding provided 'value'"))
      }
      
      if (mutual) {
      ## Mutual binding contract //          
        binding <- getBoilerplateCode(ns = classr::createInstance(
          cl = "Reactr.BindingContractMutual.S3"))
      } else {
      ## Monitoring binding contract //          
        binding <- getBoilerplateCode(ns = classr::createInstance(
          cl = "Reactr.BindingContractMonitoring.S3"))
      }
    }
  }

  ## Ensure environment that caches hash keys //
#   if (!exists("._HASH", envir = where, inherits = FALSE)) {
#     assign("._HASH", new.env(), envir = where)
#   }    
  ensureHashRegistryState(id = id, where = where, .hash_id = .hash_id)
  if (!identical_where) {
    ensureHashRegistryState(id = id, where = where_watch,
                            .hash_id = .hash_id)
  }

  if (binding_type == 1) {
    has_binding <- try(bindingIsActive(id, where), silent = TRUE)
    if (inherits(has_binding, "try-error") || specific_binding) {
      has_binding <- FALSE
    } else {
      has_binding <- has_binding
    }
    
    ensureHashRegistryState(id = id, watch = watch, where = where,
                            .hash_id = .hash_id)
    if (!identical_where) {
      ensureHashRegistryState(id = id, watch = watch, where = where_watch,
                              .hash_id = .hash_id)
    }
    
    if (!has_binding) {
    ## Register variable with binding for the first time //
      ## Note: this only takes care of "registering" the variable!    
      makeActiveBinding(id, eval(binding), where)  
    } else {
      out <- assign(id, value, envir = where)
    }

    if (!length(watch) || has_binding) {
      ## Actually set desired value or pdate previously set variable //
      out <- assign(id, value, envir = where)
    } else {
      out <- get(id, envir = where, inherits = FALSE)
    }
  } else if (binding_type == 2) {
    ## Ensure auxiliary environments used for caching //
    if (!exists(id, envir = where[[.hash_id]], inherits = FALSE)) {
      assign(id, new.env(), envir = where[[.hash_id]])  
    }
    if (!exists(".bindings", envir = where, inherits = FALSE)) {
      assign(".bindings", new.env(), envir = where)
    }    
    if (!exists(".watch", envir = where, inherits = FALSE)) {
      assign(".watch", new.env(), envir = where)
    }
    
    ## Find out what kind of variable should be set //
    if (!length(watch) && !specific_binding) {
      has_binding <- FALSE
    } else {
      has_binding <- TRUE
    }
    
    if (has_binding) {
    ## Variable monitors another variable //
      ensureHashRegistryState(id = id, watch = watch, where = where,
                              .hash_id = .hash_id)
      if (!identical_where) {
        ensureHashRegistryState(id = id, watch = watch, where = where_watch,
                                .hash_id = .hash_id)
      }
      ## Retrieve hash key for monitored variable and transfer:
      assign(
        id, 
        value = get(watch, envir = where[[.hash_id]][[watch]], inherits = FALSE), 
        envir = where[[.hash_id]][[watch]]
      )
      ## Compute value based on binding function:
      out <- eval(binding)(x = get(watch, envir = where, inherits = FALSE))
      ## Cache current binding value:
      assign(id, out, envir = where)
      ## Compute and cache own hash value:
      assign(id, digest::digest(out), envir = where[[.hash_id]][[id]])
      ## Cache binding function:
      assign(id, binding, envir = where$.bindings)    
      ## Cache name of monitored variable:
      assign(id, watch, envir = where$.watch)    
    } else {
    ## Variable does not monitor another variable //
      ## Set:
      out <- assign(id, value, envir = where)
      ## Compute and cache hash value:
      assign(id, digest::digest(value), envir = where[[.hash_id]][[id]])
    }
  } else {
    stop(paste0("Something went wrong with 'binding_type'"))
  }
  
  return(out)
  
}
