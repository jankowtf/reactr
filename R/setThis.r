#' @title
#' Set Value (generic)
#'
#' @description 
#' Sets a variable value in an environment.
#' See main method 
#' \code{\link{setThis-character-ANY-environment-character-environment-call-method}}
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
#' The main S4 method is 
#' \code{\link[reactr]{setThis-character-ANY-environment-character-environment-call-method}}.
#' There also exist a bare S3 function that is about 10 - 15 % faster than 
#' the S4 method: \code{\link[reactr]{setThis_bare}}.
#'     
#' @param id \strong{Signature argument}.
#'    Object containing path-like ID information.
#' @param value \strong{Signature argument}.
#'    Object containing value information.
#' @param where \strong{Signature argument}.
#'    Object containing location information.
#' @param watch \strong{Signature argument}.
#'    Object containing information about the variable to monitor.
#'    Typically, this is the name of a variable.
#' @param where_watch \strong{Signature argument}.
#'    Object containing location information with respect to the variable 
#'    to monitor. Typically, this is the same location as the one for 
#'    \code{id}.
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
#'    not specified: \emph{monitored} contract).
#' @param force \code{\link{logical}}.
#'    \code{TRUE}: force a binding reset even though there might already
#'    have been defined another one;
#'    \code{FALSE}: in case a binding has already been defined it is not
#'    overwritten. \strong{Note that for the following constellations this value is 
#'    automtically set to \code{TRUE}: \code{mutual = TRUE} and whenever an
#'    explicit binding definition is provided via \code{binding}}.
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
#' @example inst/examples/setThis.r
#' @seealso \code{
#'   	\link[reactr]{setThis-character-ANY-environment-character-environment-call-method}
#' }
#' @template author
#' @template references
#' @export 
setGeneric(
  name = "setThis",
  signature = c(
    "id",
    "value",
    "where",
    "watch",
    "where_watch",
    "binding"
  ),
  def = function(
    id,
    value = NULL,
    where = .GlobalEnv,
    watch = character(),
    where_watch = where,
    binding = substitute(expression()),
    binding_type = 1,
    mutual = FALSE,
    force = FALSE,
    .hash_id = "._HASH",
    .tracelevel = 0,
    ...
  ) {
    standardGeneric("setThis")       
  }
)

#' @title
#' Set Value (character,ANY,missing,missing,missing,missing)
#'
#' @description 
#' See generic: \code{\link[reactr]{setThis}}
#'      
#' @inheritParams setThis
#' @param id \code{\link{character}}.
#' @param value \code{\link{ANY}}.
#' @param where \code{\link{missing}}.
#' @param watch \code{\link{missing}}.
#' @param watch \code{\link{missing}}.
#' @param binding \code{\link{missing}}.
#' @return See method
#'    \code{\link[reactr]{setThis-character-ANY-missing-character-missing-call-method}}.
#' @example inst/examples/setThis.r
#' @seealso \code{
#'    Generic: \link[reactr]{setThis}
#' }
#' @template author
#' @template references
#' @export
setMethod(
  f = "setThis", 
  signature = signature(
    id = "character",
    value = "ANY",
    where = "missing",
    watch = "missing",
    where_watch = "missing",
    binding = "missing"
  ), 
  definition = function(
    id,
    value,
    where,
    watch,
    where_watch,
    binding,
    binding_type,
    mutual,
    force,
    .hash_id,
    .tracelevel,
    ...
  ) {
  
  return(setThis(
    id = id,
    value = value,
    where = where,
    watch = watch,
    where_watch = where_watch,
    binding = substitute(binding),
    binding_type = binding_type,
    mutual = mutual,
    force = force,
    .hash_id = .hash_id,
    .tracelevel = .tracelevel,
    ...
  ))
  
  }
)

#' @title
#' Set Value (character,ANY,environment,missing,missing,missing)
#'
#' @description 
#' See generic: \code{\link[reactr]{setThis}}
#'      
#' @inheritParams setThis
#' @param id \code{\link{character}}.
#' @param value \code{\link{ANY}}.
#' @param where \code{\link{environment}}.
#' @param watch \code{\link{missing}}.
#' @param watch \code{\link{missing}}.
#' @param binding \code{\link{missing}}.
#' @return See method
#'    \code{\link[reactr]{setThis-character-ANY-environment-character-missing-call-method}}.
#' @example inst/examples/setThis.r
#' @seealso \code{
#'    Generic: \link[reactr]{setThis}
#' }
#' @template author
#' @template references
#' @export
setMethod(
  f = "setThis", 
  signature = signature(
    id = "character",
    value = "ANY",
    where = "environment",
    watch = "missing",
    where_watch = "missing",
    binding = "missing"
  ), 
  definition = function(
    id,
    value,
    where,
    watch,
    where_watch,
    binding,
    binding_type,
    mutual,
    force,
    .hash_id,
    .tracelevel,
    ...
  ) {
  
  return(setThis(
    id = id,
    value = value,
    where = where,
    watch = watch,
    where_watch = where_watch,
    binding = substitute(binding),
    binding_type = binding_type,
    mutual = mutual,
    force = force,
    .hash_id = .hash_id,
    .tracelevel = .tracelevel,
    ...
  ))
  
  }
)

#' @title
#' Set Value (character,ANY,environment,missing,environment,missing)
#'
#' @description 
#' See generic: \code{\link[reactr]{setThis}}
#'      
#' @inheritParams setThis
#' @param id \code{\link{character}}.
#' @param value \code{\link{ANY}}.
#' @param where \code{\link{environment}}.
#' @param watch \code{\link{missing}}.
#' @param watch \code{\link{environment}}.
#' @param binding \code{\link{missing}}.
#' @return See method
#'    \code{\link[reactr]{setThis-character-ANY-environment-character-environment-call-method}}.
#' @example inst/examples/setThis.r
#' @seealso \code{
#'    Generic: \link[reactr]{setThis}
#' }
#' @template author
#' @template references
#' @export
setMethod(
  f = "setThis", 
  signature = signature(
    id = "character",
    value = "ANY",
    where = "environment",
    watch = "missing",
    where_watch = "environment",
    binding = "missing"
  ), 
  definition = function(
    id,
    value,
    where,
    watch,
    where_watch,
    binding,
    binding_type,
    mutual,
    force,
    .hash_id,
    .tracelevel,
    ...
  ) {
  
  return(setThis(
    id = id,
    value = value,
    where = where,
    watch = watch,
    where_watch = where_watch,
    binding = substitute(binding),
    binding_type = binding_type,
    mutual = mutual,
    force = force,
    .hash_id = .hash_id,
    .tracelevel = .tracelevel,
    ...
  ))
  
  }
)

#' @title
#' Set Value (character,ANY,missing,character,missing,call)
#'
#' @description 
#' See generic: \code{\link[reactr]{setThis}}
#'      
#' @inheritParams setThis
#' @param id \code{\link{character}}.
#' @param value \code{\link{ANY}}.
#' @param where \code{\link{missing}}.
#' @param watch \code{\link{character}}.
#' @param where_watch \code{\link{missing}}.
#' @param binding \code{\link{call}}.
#' @return See method
#'    \code{\link[reactr]{setThis-character-ANY-missing-character-missing-call-method}}.
#' @example inst/examples/setThis.r
#' @seealso \code{
#'    Generic: \link[reactr]{setThis}
#' }
#' @template author
#' @template references
#' @export
setMethod(
  f = "setThis", 
  signature = signature(
    id = "character",
    value = "ANY",
    where = "missing",
    watch = "character",
    where_watch = "missing",
    binding = "call"
  ), 
  definition = function(
    id,
    value,
    where,
    watch,
    where_watch,
    binding,
    binding_type,
    mutual,
    force,
    .hash_id,
    .tracelevel,
    ...
  ) {

  return(setThis(
    id = id,
    value = value,
    where = where,
    watch = watch,
    where_watch = where_watch,
    binding = binding,
    binding_type = binding_type,
    mutual = mutual,
    force = force,
    .hash_id = .hash_id,
    .tracelevel = .tracelevel,
    ...
  ))
  
  }
)

#' @title
#' Set Value (character,ANY,environment,missing,environment,call)
#'
#' @description 
#' See generic: \code{\link[reactr]{setThis}}
#'      
#' @inheritParams setThis
#' @param id \code{\link{character}}.
#' @param value \code{\link{ANY}}.
#' @param where \code{\link{environment}}.
#' @param watch \code{\link{missing}}.
#' @param where_watch \code{\link{environment}}.
#' @param binding \code{\link{call}}.
#' @return See method
#'    \code{\link[reactr]{setThis-character-ANY-environment-character-environment-call-method}}.
#' @example inst/examples/setThis.r
#' @seealso \code{
#'    Generic: \link[reactr]{setThis}
#' }
#' @template author
#' @template references
#' @export
setMethod(
  f = "setThis", 
  signature = signature(
    id = "character",
    value = "ANY",
    where = "environment",
    watch = "missing",
    where_watch = "environment",
    binding = "call"
  ), 
  definition = function(
    id,
    value,
    where,
    watch,
    where_watch,
    binding,
    binding_type,
    mutual,
    force,
    .hash_id,
    .tracelevel,
    ...
  ) {

  return(setThis(
    id = id,
    value = value,
    where = where,
    watch = watch,
    where_watch = where_watch,
    binding = binding,
    binding_type = binding_type,
    mutual = mutual,
    force = force,
    .hash_id = .hash_id,
    .tracelevel = .tracelevel,
    ...
  ))
  
  }
)

#' @title
#' Set Value (character,ANY,missing,character,missing,missing)
#'
#' @description 
#' See generic: \code{\link[reactr]{setThis}}
#'      
#' @inheritParams setThis
#' @param id \code{\link{character}}.
#' @param value \code{\link{ANY}}.
#' @param where \code{\link{missing}}.
#' @param watch \code{\link{character}}.
#' @param where_watch \code{\link{missing}}.
#' @param binding \code{\link{missing}}.
#' @return See method
#'    \code{\link[reactr]{setThis-character-ANY-missing-character-missing-call-method}}.
#' @seealso \code{
#'    Generic: \link[reactr]{setThis}
#' }
#' @template author
#' @template references
#' @export
setMethod(
  f = "setThis", 
  signature = signature(
    id = "character",
    value = "ANY",
    where = "missing",
    watch = "character",
    where_watch = "missing",
    binding = "missing"
  ), 
  definition = function(
    id,
    value,
    where,
    watch,
    where_watch,
    binding,
    binding_type,
    mutual,
    force,
    .hash_id,
    .tracelevel,
    ...
  ) {

  return(setThis(
    id = id,
    value = value,
    where = where,
    watch = watch,
    where_watch = where_watch,
    binding = binding,
    binding_type = binding_type,
    mutual = mutual,
    force = force,
    .hash_id = .hash_id,
    .tracelevel = .tracelevel,
    ...
  ))
  
  }
)

#' @title
#' Set Value (character,ANY,environment,character,missing,missing)
#'
#' @description 
#' See generic: \code{\link[reactr]{setThis}}
#'      
#' @inheritParams setThis
#' @param id \code{\link{character}}.
#' @param value \code{\link{ANY}}.
#' @param where \code{\link{environment}}.
#' @param watch \code{\link{character}}.
#' @param where_watch \code{\link{missing}}.
#' @param binding \code{\link{missing}}.
#' @return See method
#'    \code{\link[reactr]{setThis-character-ANY-environment-character-missing-call-method}}.
#' @seealso \code{
#'    Generic: \link[reactr]{setThis}
#' }
#' @template author
#' @template references
#' @export
setMethod(
  f = "setThis", 
  signature = signature(
    id = "character",
    value = "ANY",
    where = "environment",
    watch = "character",
    where_watch = "missing",
    binding = "missing"
  ), 
  definition = function(
    id,
    value,
    where,
    watch,
    where_watch,
    binding,
    binding_type,
    mutual,
    force,
    .hash_id,
    .tracelevel,
    ...
  ) {

  return(setThis(
    id = id,
    value = value,
    where = where,
    watch = watch,
    where_watch = where_watch,
    binding = binding,
    binding_type = binding_type,
    mutual = mutual,
    force = force,
    .hash_id = .hash_id,
    .tracelevel = .tracelevel,
    ...
  ))
  
  }
)

#' @title
#' Set Value (character,ANY,environment,character,environment,missing)
#'
#' @description 
#' See generic: \code{\link[reactr]{setThis}}
#'      
#' @inheritParams setThis
#' @param id \code{\link{character}}.
#' @param value \code{\link{ANY}}.
#' @param where \code{\link{environment}}.
#' @param watch \code{\link{character}}.
#' @param where_watch \code{\link{environment}}.
#' @param binding \code{\link{missing}}.
#' @return See method
#'    \code{\link[reactr]{setThis-character-ANY-environment-character-environment-call-method}}.
#' @seealso \code{
#'    Generic: \link[reactr]{setThis}
#' }
#' @template author
#' @template references
#' @export
setMethod(
  f = "setThis", 
  signature = signature(
    id = "character",
    value = "ANY",
    where = "environment",
    watch = "character",
    where_watch = "environment",
    binding = "missing"
  ), 
  definition = function(
    id,
    value,
    where,
    watch,
    where_watch,
    binding,
    binding_type,
    mutual,
    force,
    .hash_id,
    .tracelevel,
    ...
  ) {

  return(setThis(
    id = id,
    value = value,
    where = where,
    watch = watch,
    where_watch = where_watch,
    binding = binding,
    binding_type = binding_type,
    mutual = mutual,
    .hash_id = .hash_id,
    .tracelevel = .tracelevel,
    ...
  ))
  
  }
)

#' @title
#' Set Value (character,ANY,missing,character,missing,function)
#'
#' @description 
#' See generic: \code{\link[reactr]{setThis}}
#'      
#' @inheritParams setThis
#' @param id \code{\link{character}}.
#' @param value \code{\link{ANY}}.
#' @param where \code{\link{missing}}.
#' @param watch \code{\link{character}}.
#' @param binding \code{\link{function}}.
#' @return See method
#'    \code{\link[reactr]{setThis-character-ANY-missing-character-missing-call-method}}.
#' @example inst/examples/setThis.r
#' @seealso \code{
#'    Generic: \link[reactr]{setThis}
#' }
#' @template author
#' @template references
#' @export
#' @import classr
setMethod(
  f = "setThis", 
  signature = signature(
    id = "character",
    value = "ANY",
    where = "missing",
    watch = "character",
    where_watch = "missing",
    binding = "function"
  ), 
  definition = function(
    id,
    value,
    where,
    watch,
    where_watch,
    binding,
    binding_type,
    mutual,
    force,
    .hash_id,
    .tracelevel,
    ...
  ) {

  return(setThis(
    id = id,
    value = value,
    where = where,
    watch = watch,
    where_watch = where_watch,
    binding = if (!mutual) {
      getBoilerplateCode(ns = classr::createInstance(
        cl = "Reactr.BindingContractMonitoring.S3"))
    } else {
      getBoilerplateCode(ns = classr::createInstance(
        cl = "Reactr.BindingContractMutual.S3"))
    },
    binding_type = binding_type,
    .binding = binding,
    mutual = mutual,
    force = force,
    .tracelevel = .tracelevel,
    ...
  ))
  
  }
)

#' @title
#' Set Value (character,ANY,environment,character,missing,function)
#'
#' @description 
#' See generic: \code{\link[reactr]{setThis}}
#'      
#' @inheritParams setThis
#' @param id \code{\link{character}}.
#' @param value \code{\link{ANY}}.
#' @param where \code{\link{environment}}.
#' @param watch \code{\link{character}}.
#' @param binding \code{\link{function}}.
#' @return See method
#'    \code{\link[reactr]{setThis-character-ANY-environment-character-missing-call-method}}.
#' @example inst/examples/setThis.r
#' @seealso \code{
#'    Generic: \link[reactr]{setThis}
#' }
#' @template author
#' @template references
#' @export
#' @import classr
setMethod(
  f = "setThis", 
  signature = signature(
    id = "character",
    value = "ANY",
    where = "environment",
    watch = "character",
    where_watch = "missing",
    binding = "function"
  ), 
  definition = function(
    id,
    value,
    where,
    watch,
    where_watch,
    binding,
    binding_type,
    mutual,
    force,
    .hash_id,
    .tracelevel,
    ...
  ) {

  return(setThis(
    id = id,
    value = value,
    where = where,
    watch = watch,
    where_watch = where_watch,
    binding = if (!mutual) {
      getBoilerplateCode(ns = classr::createInstance(
        cl = "Reactr.BindingContractMonitoring.S3"))
    } else {
      getBoilerplateCode(ns = classr::createInstance(
        cl = "Reactr.BindingContractMutual.S3"))
    },
    binding_type = binding_type,
    .binding = binding,
    mutual = mutual,
    force = force,
    .tracelevel = .tracelevel,
    ...
  ))
  
  }
)

#' @title
#' Set Value (character,ANY,environment,character,environment,function)
#'
#' @description 
#' See generic: \code{\link[reactr]{setThis}}
#'      
#' @inheritParams setThis
#' @param id \code{\link{character}}.
#' @param value \code{\link{ANY}}.
#' @param where \code{\link{environment}}.
#' @param watch \code{\link{character}}.
#' @param binding \code{\link{function}}.
#' @return See method
#'    \code{\link[reactr]{setThis-character-ANY-environment-character-environment-call-method}}.
#' @example inst/examples/setThis.r
#' @seealso \code{
#'    Generic: \link[reactr]{setThis}
#' }
#' @template author
#' @template references
#' @export
#' @import classr
setMethod(
  f = "setThis", 
  signature = signature(
    id = "character",
    value = "ANY",
    where = "environment",
    watch = "character",
    where_watch = "environment",
    binding = "function"
  ), 
  definition = function(
    id,
    value,
    where,
    watch,
    where_watch,
    binding,
    binding_type,
    mutual,
    force,
    .hash_id,
    .tracelevel,
    ...
  ) {

  return(setThis(
    id = id,
    value = value,
    where = where,
    watch = watch,
    where_watch = where_watch,
    binding = if (!mutual) {
      getBoilerplateCode(ns = classr::createInstance(
        cl = "Reactr.BindingContractMonitoring.S3"))
    } else {
      getBoilerplateCode(ns = classr::createInstance(
        cl = "Reactr.BindingContractMutual.S3"))
    },
    binding_type = binding_type,
    .binding = binding,
    mutual = mutual,
    force = force,
    .tracelevel = .tracelevel,
    ...
  ))
  
  }
)

#' @title
#' Set Value (character,ANY,environment,missing,missing,call)
#'
#' @description 
#' See generic: \code{\link[reactr]{setThis}}
#'      
#' @inheritParams setThis
#' @param id \code{\link{character}}.
#' @param value \code{\link{ANY}}.
#' @param where \code{\link{environment}}.
#' @param watch \code{\link{missing}}.
#' @param where_watch \code{\link{missing}}.
#' @param binding \code{\link{call}}.
#' @return See method
#'    \code{\link[reactr]{setThis-character-ANY-environment-character-missing-call-method}}.
#' @example inst/examples/setThis.r
#' @seealso \code{
#'    Generic: \link[reactr]{setThis}
#' }
#' @template author
#' @template references
#' @export
setMethod(
  f = "setThis", 
  signature = signature(
    id = "character",
    value = "ANY",
    where = "environment",
    watch = "character",
    where_watch = "missing",
    binding = "call"
  ), 
  definition = function(
    id,
    value,
    where,
    watch,
    where_watch,
    binding,
    binding_type,
    mutual,
    force,
    .hash_id,
    .tracelevel,
    ...
  ) {

  return(setThis(
    id = id,
    value = value,
    where = where,
    watch = watch,
    where_watch = where_watch,
    binding = binding,
    binding_type = binding_type,
    mutual = mutual,
    force = force,
    .tracelevel = .tracelevel,
    ...
  ))
  
  }
)

#' @title
#' Set Value (character,ANY,environment,character,environment,call)
#'
#' @description 
#' See generic: \code{\link[reactr]{setThis}}
#'   	 
#' @inheritParams setThis
#' @param id \code{\link{character}}.
#' @param value \code{\link{ANY}}.
#' @param where \code{\link{environment}}.
#' @param watch \code{\link{character}}.
#' @param where_watch \code{\link{environment}}.
#' @param binding \code{\link{call}}.
#' @param .binding \code{\link{function}}. 
#'    Internal argument that should not be set explicitly.
#'    The value at runtime will correspond to the function that has been 
#'    provided via argument \code{binding}.
#' @return \code{\link{ANY}}. Value of \code{value} or the return value 
#'    of the binding function.
#' @example inst/examples/setThis.r
#' @seealso \code{
#'    Generic: \link[reactr]{setThis}
#' }
#' @template author
#' @template references
#' @aliases setThis-method_main 
#' @export
#' @import digest
#' @import classr
setMethod(
  f = "setThis", 
  signature = signature(
    id = "character",
    value = "ANY",
    where = "environment",
    watch = "character",
    where_watch = "environment",
    binding = "call"
  ), 
  definition = function(
    id,
    value,
    where,
    watch,
    where_watch,
    binding,
    binding_type,
    mutual,
    force,
    .hash_id,
    .tracelevel,
    .binding = NULL,
    ...
  ) {

  ## Validate binding type //
#   binding_type <- as.numeric(match.arg(as.character(binding_type), c("1", "2")))
    binding_type
    
  ## Check location identiy //
  identical_where <- identical(where, where_watch)
  
  ## Check if specific binding or not //
  specific_binding <- !deparse(binding)[1] %in% c("expression()", 
    "substitute(expression())")
  
  ## Force //
  if (mutual || specific_binding) {
    force <- TRUE
  }

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
      if (mutual) {
      ## Mutual binding contract //          
        binding <- getBoilerplateCode(ns = classr::createInstance(
          cl = "Reactr.BindingContractMutual.S3"))
      } else {
      ## Monitoring binding contract //          
        if (length(value)) {
          warning(paste0("Variable has monitoring binding --> disregarding provided 'value'"))
        }
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
    ensureHashRegistryState(id = id, where = where_watch, .hash_id = .hash_id)
  }

  if (binding_type == 1) {
    has_binding <- try(bindingIsActive(id, where), silent = TRUE)
    if (inherits(has_binding, "try-error") || force) {
      has_binding <- FALSE
    } else {
      has_binding <- has_binding
    }
    
    ensureHashRegistryState(id = id, watch = watch, where = where, .hash_id = .hash_id)
    if (!identical_where) {
      ensureHashRegistryState(id = id, watch = watch, where = where_watch, 
                              .hash_id = .hash_id)
    }
    
    if (!has_binding) {
    ## Register variable with binding for the first time //
      ## Make sure to really remove prior variables before setting or 
      ## resetting them //
      if (exists(id, envir = where, inherits = FALSE)) {
        rm(list = id, envir = where, inherits = TRUE)
      }
      
      ## Note: this only takes care of "registering" the variable!    
      makeActiveBinding(id, eval(binding), where)  
    } else {
    ## Assign standard value      
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
)
