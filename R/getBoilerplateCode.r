#' @title
#' Get Boilerplate of Binding Contract
#'
#' @description 
#' Get boilerplate code for binding contract.
#'   	
#' @param ns \strong{Signature argument}.
#'    Object containing boilerplate namespace information.
#'    This usually corresponds to an instance of a class used for 
#'    distinguishing boilerplate code. See \code{\link[classr]{asClassInstance}}
#' @template threedot
#' @example inst/examples/getBoilerplateCode.r
#' @seealso \code{
#'   	\link[reactr]{getBoilerplateCode-missing-method}
#' }
#' @template author
#' @template references
#' @export 
setGeneric(
  name = "getBoilerplateCode",
  signature = c(
    "ns"
  ),
  def = function(
    ns,
    ...
  ) {
    standardGeneric("getBoilerplateCode")       
  }
)

#' @title
#' Get Boilerplate of Binding Contract
#'
#' @description 
#' See generic: \code{\link[reactr]{getBoilerplateCode}}
#'   	 
#' @inheritParams getBoilerplateCode
#' @param ns \code{\link{Reactr.BindingContractSet.S3}}.
#' @return \code{\link{call}}. Implemented binding interface.
#' @example inst/examples/getBoilerplateCode.r
#' @seealso \code{
#'    \link[reactr]{getBoilerplateCode}
#' }
#' @template author
#' @template references
#' @export
#' @import rapp.core.condition
setMethod(
  f = "getBoilerplateCode", 
  signature = signature(
    ns = "Reactr.BindingContractSet.S3"
  ), 
  definition = function(
    ns,
    ...
  ) {

  out <- substitute(
    local({
      VALUE <- NULL
      function(v) {
#         if (!exists(id, where$.hash, inherits = FALSE)) {
#           assign(id, new.env(), envir = where$.hash)
#         }
        if (!missing(v)) {
#           where$.hash[[id]][[id]] <- digest::digest(v)
          VALUE <<- v
        }
        ## Ensure hash value //
        assign(id, digest::digest(VALUE), where$.hash[[id]])
        VALUE
      }
    }),
    list(
      VALUE = as.name("value")
    )
  )
  
  return(out)
    
  }
)

#' @title
#' Get Boilerplate of Binding Contract
#'
#' @description 
#' See generic: \code{\link[reactr]{getBoilerplateCode}}
#'      
#' @inheritParams getBoilerplateCode
#' @param ns \code{\link{Reactr.BindingContractGet.S3}}.
#' @return \code{\link{call}}. Implemented binding interface.
#' @example inst/examples/getBoilerplateCode.r
#' @seealso \code{
#'    \link[reactr]{getBoilerplateCode}
#' }
#' @template author
#' @template references
#' @export
#' @import rapp.core.condition
setMethod(
  f = "getBoilerplateCode", 
  signature = signature(
    ns = "Reactr.BindingContractGet.S3"
  ), 
  definition = function(
    ns,
    ...
  ) {

  out <- substitute(
    local({
      ##------------------------------------------------------------------------
      ## Initialization //
      ##------------------------------------------------------------------------
      
      if (  exists(watch, envir = where, inherits = FALSE) &&
            !is.null(get(watch, envir = where, inherits = FALSE))
      ) {
        VALUE <- BINDING_IFACE
      } else {
        VALUE <- NULL
      }
      
      ## Ensure hash value transfer //
      hash_0 <- as.character(where$.hash[[watch]][[watch]])
      hash_1 <- as.character(where$.hash[[watch]][[id]])
#       if (  exists(watch, envir = where$.hash[[watch]], inherits = FALSE) &&
#             !exists(id, envir = where$.hash[[watch]], inherits = FALSE)
#       ) {
      if (!length(hash_1)) {
        assign(
          id, 
#           get(watch, envir = where$.hash[[watch]]),
          hash_0,
          where$.hash[[watch]]
        )
      }
print("DEBUG")
print(hash_0)
print(hash_1)

      function(v) {
        if (  exists(watch, envir = where, inherits = FALSE) &&
              !is.null(get(watch, envir = where, inherits = FALSE))
        ) {
          
        ##----------------------------------------------------------------------
        ## Get //
        ##----------------------------------------------------------------------
        
          if (missing(v)) {
            hash_0 <- where$.hash[[watch]][[watch]]
            hash_1 <- where$.hash[[watch]][[id]]
            if (!length(hash_0)) {
              stop(paste0("[", Sys.getpid(), "] ", gsub("-|:| ", "", Sys.time()), 
                "/reactr/binding> ID: ", id, " --> empty hash for: ", watch))
            }
            message(hash_0)
            message(hash_1)
            if (hash_0 != hash_1) {
#               message("monitored variable has changed:")
#               message("updating")
              VALUE <<- BINDING_IFACE
              where$.hash[[watch]][[id]] <- hash_0
            }
          }
        }
        VALUE
      }
    }),
    list(
      VALUE = as.name("value"), 
      BINDING_IFACE = substitute(.binding(x = where[[watch]]))
    )
  )    
  
  return(out)
    
  }
)

#' @title
#' Get Boilerplate of Binding Contract
#'
#' @description 
#' See generic: \code{\link[reactr]{getBoilerplateCode}}
#'      
#' @inheritParams getBoilerplateCode
#' @param ns \code{\link{Reactr.BindingContractCombined.S3}}.
#' @return \code{\link{call}}. Implemented binding interface.
#' @example inst/examples/getBoilerplateCode.r
#' @seealso \code{
#'    \link[reactr]{getBoilerplateCode}
#' }
#' @template author
#' @template references
#' @export
#' @import rapp.core.condition
setMethod(
  f = "getBoilerplateCode", 
  signature = signature(
    ns = "Reactr.BindingContractCombined.S3"
  ), 
  definition = function(
    ns,
    ...
  ) {

  out <- substitute(
    local({
      
      ##------------------------------------------------------------------------
      ## Initialization //
      ##------------------------------------------------------------------------
      
message("Force (before):")
print(FORCE_VALUE)
# print(VALUE_FORCE)      
      
# force_value <- FALSE      
      if (FORCE_VALUE) {
        if (FALSE) {
          VALUE <- VALUE_FORCE
          
          ## Try aligning hash values //
          assign(id, digest::digest(NULL), envir = where$.hash[[watch]])
          assign(watch, digest::digest(NULL), envir = where$.hash[[watch]])
#           assign(id, digest::digest(NULL), where$.hash[[id]])
  #         print("asldkfjasldjkf")
        }
        value <- NULL
      } else {
        if (  exists(watch, envir = where, inherits = FALSE) &&
              !is.null(get(watch, envir = where, inherits = FALSE))
        ) {
          VALUE <- BINDING_IFACE
        } else {
          VALUE <- NULL
        }
      }

      ## Ensure hash value transfer //
      hash_0 <- where$.hash[[watch]][[watch]]
      if (is.null(hash_0) || !length(hash_0)) {
        hash_0 <- digest::digest(hash_0)
        assign(watch, hash_0, envir = where$.hash[[watch]])
      }
      hash_1 <- where$.hash[[watch]][[id]]
      if (is.null(hash_1) || !length(hash_1)) {
        hash_1 <- digest::digest(hash_1)
        assign(id, hash_1, envir = where$.hash[[watch]])
      }
#       if (  exists(watch, envir = where$.hash[[watch]], inherits = FALSE) &&
#             !exists(id, envir = where$.hash[[watch]], inherits = FALSE)
#       ) {
      if (is.null(hash_1)) {
        assign(
          id, 
#           get(watch, envir = where$.hash[[watch]]),
          hash_0,
          where$.hash[[watch]]
        )
      }

print(hash_0)
print(hash_1)
message("----------")
      function(v) {
        
        ##----------------------------------------------------------------------
        ## Set //
        ##----------------------------------------------------------------------
        
        if (!missing(v)) {
#           where$.hash[[id]][[id]] <- digest::digest(v)
          VALUE <<- v
        }
        ## Ensure hash value //
        ## PATCH-TRY: aling initial hash values
#         if (!FORCE_VALUE) {
          assign(id, digest::digest(VALUE), where$.hash[[id]])
#         } else {
#           assign(id, digest::digest(NULL), where$.hash[[id]])
#         }

message("id:")
message(id)
message("watch:")
message(watch)
message("hash of 'id':")
print(where$.hash[[id]][[id]])   
message("hash of 'watch':")
print(where$.hash[[watch]][[watch]])   
message("hash envir of 'watch':")
print(ls(where$.hash[[watch]]))   

        ##----------------------------------------------------------------------
        ## Get //
        ##----------------------------------------------------------------------

message("Force (middle):")
print(FORCE_VALUE)

        if (  exists(watch, envir = where, inherits = FALSE) &&
              !FORCE_VALUE
        ) {
          if (missing(v)) {
            hash_0 <- where$.hash[[watch]][[watch]]
            hash_1 <- where$.hash[[watch]][[id]]
            if (!length(hash_0)) {
              stop(paste0("[", Sys.getpid(), "] ", gsub("-|:| ", "", Sys.time()), 
                "/reactr/binding> ID: ", id, " --> empty hash for: ", watch))
            }
            message(hash_0)
            message(hash_1)
            if (hash_0 != hash_1) {
#               message("monitored variable has changed:")
              message("updating")
#               stop("planned stop")
              VALUE <<- BINDING_IFACE
              where$.hash[[watch]][[id]] <- hash_0
            }
#             if (FORCE_VALUE) {
#               VALUE <<- VALUE_FORCE
#             }
          }
        }
        force_value <- FALSE
message("Force (after):")
print(FORCE_VALUE)
        VALUE
      }

#       force_value <- FALSE


#       VALUE

    }),
    list(
      VALUE = as.name("value"), 
      BINDING_IFACE = substitute(.binding(x = where[[watch]])),
      FORCE_VALUE = as.name("force_value"),
      VALUE_FORCE = as.name("value_force")
    )
  )    
  
  return(out)
    
  }
)
