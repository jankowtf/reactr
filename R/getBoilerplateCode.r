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
#' @param ns \code{\link{Reactr.BindingContractMonitored.S3}}.
#' @return \code{\link{call}}. Implemented binding interface.
#' @example inst/examples/getBoilerplateCode.r
#' @seealso \code{
#'    \link[reactr]{getBoilerplateCode}
#' }
#' @template author
#' @template references
#' @export
setMethod(
  f = "getBoilerplateCode", 
  signature = signature(
    ns = "Reactr.BindingContractMonitored.S3"
  ), 
  definition = function(
    ns,
    ...
  ) {

  out <- substitute(
    local({
      VALUE <- NULL
      function(v) {
        if (!missing(v)) {
          VALUE <<- v
          ## Ensure hash value //
          assign(id, digest::digest(VALUE), where[[HASH]][[id]])
        }   
        VALUE
      }
    }),
    list(
      VALUE = as.name("value"),
      HASH = as.name(".hash_id")
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
#' @param ns \code{\link{Reactr.BindingContractMonitoring.S3}}.
#' @return \code{\link{call}}. Implemented binding interface.
#' @example inst/examples/getBoilerplateCode.r
#' @seealso \code{
#'    \link[reactr]{getBoilerplateCode}
#' }
#' @template author
#' @template references
#' @export
setMethod(
  f = "getBoilerplateCode", 
  signature = signature(
    ns = "Reactr.BindingContractMonitoring.S3"
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
      
      if (.tracelevel == 1) {
        message("----- INIT START -----")
        message("id:")
        message(id)
        message("watch:")
        message(watch)      
      }
      
      if (  exists(watch, envir = where_watch, inherits = FALSE) &&
            !is.null(get(watch, envir = where_watch, inherits = FALSE))
      ) {
        VALUE <- BINDING_CONTRACT
      } else {
        VALUE <- NULL
      }
  
      if (.tracelevel == 1) {
        message("----- INIT END -----")
      }
      
      function(v) {
        
        if (.tracelevel == 1) {
          message("----- BINDING CONTRACT START -----")
          message("id:")
          message(id)
          message("watch:")
          message(watch)
          message("hash id/id:")
          print(where[[HASH]][[id]][[id]])   
          message("hash id/watch:")
          print(where[[HASH]][[id]][[watch]])   
          message("hash watch/watch:")
          print(where_watch[[HASH]][[watch]][[watch]])   
          message("hash watch/id:")
          print(where_watch[[HASH]][[watch]][[id]])   
        }
        
        if (exists(watch, envir = where_watch, inherits = FALSE)) {  
          
        ##----------------------------------------------------------------------
        ## Get //
        ##----------------------------------------------------------------------
        
          if (missing(v)) {
            if (.tracelevel == 1) {
              message(paste0("retrieve (", id, " watching ", watch, ")"))
            }
            ## Control for reactives that have been unset in the meantime //
            if (exists(watch, envir = where_watch[[HASH]], inherits = FALSE)) {          
              hash_0 <- where_watch[[HASH]][[watch]][[watch]]
              hash_1 <- where_watch[[HASH]][[watch]][[id]]
              if (hash_0 != hash_1) {
                if (.tracelevel == 1) {
                  message(paste0("update based on contract (", 
                    id, " watching ", watch, ")"))
                  message(paste0("hash watch/watch old: ", hash_0))
                  message(paste0("hash watch/id old: ", hash_1))
                }
                VALUE <<- BINDING_CONTRACT
                hash_1 <- where[[HASH]][[watch]][[id]] <- hash_0
                where[[HASH]][[id]][[id]] <- hash_0
                where[[HASH]][[id]][[watch]] <- hash_0
                if (.tracelevel == 1) {
                  message(paste0("hash watch/watch new: ", hash_0))
                  message(paste0("hash watch/id new: ", hash_1))
                }
              } else {
                if (.tracelevel == 1) {
                  message(paste0("in sync (", 
                    id, " watching: ", watch, ")"))
                }
              }
            } else {                      
              suppressWarnings(rm(list = watch, envir = where[[HASH]][[id]], 
                inherits = FALSE))
            }
          }
        } else {
          if (STRICT) {
            VALUE <<- NULL
          }
        }
  
        if (.tracelevel == 1) {
          message("----- BINDING CONTRACT END -----")
        }
  
        VALUE
      }
    }),
    list(
      VALUE = as.name("value"), 
      BINDING_CONTRACT = substitute(.binding(x = where_watch[[watch]])),
      HASH = as.name(".hash_id"),
      STRICT = as.name("strict")
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
#' @param ns \code{\link{Reactr.BindingContractMutual.S3}}.
#' @return \code{\link{call}}. Implemented binding interface.
#' @example inst/examples/getBoilerplateCode.r
#' @seealso \code{
#'    \link[reactr]{getBoilerplateCode}
#' }
#' @template author
#' @template references
#' @export
#' @import digest
setMethod(
  f = "getBoilerplateCode", 
  signature = signature(
    ns = "Reactr.BindingContractMutual.S3"
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
      
      if (.tracelevel == 1) {
        message("----- INIT START -----")
        message("id:")
        message(id)
        message("watch:")
        message(watch)      
      }
      
      if (  exists(watch, envir = where, inherits = FALSE) &&
            !is.null(get(watch, envir = where, inherits = FALSE))
      ) {
  #       print(BINDING_CONTRACT)
        VALUE <- BINDING_CONTRACT
      } else {
        VALUE <- NULL
      }
  
      if (.tracelevel == 1) {
        message("----- INIT END -----")
      }
  
      function(v) {
  
      if (.tracelevel == 1) {
        message("----- BINDING CONTRACT START -----")
        message("id:")
        message(id)
        message("watch:")
        message(watch)
        message("hash id/id:")
        print(where[[HASH]][[id]][[id]])   
        message("hash id/watch:")
        print(where[[HASH]][[id]][[watch]])   
        message("hash watch/watch:")
        print(where[[HASH]][[watch]][[watch]])   
        message("hash watch/id:")
        print(where[[HASH]][[watch]][[id]])   
      }
        
        ##----------------------------------------------------------------------
        ## Set //
        ##----------------------------------------------------------------------
        
        if (!missing(v)) {
          VALUE <<- v
          ## Update hash value //
          assign(id, digest::digest(VALUE), where[[HASH]][[id]])
          if (.tracelevel == 1) {
            message(paste0("setting ", id))
            message("new hash id/id:")
            print(where[[HASH]][[id]][[id]])             
          }
        }
      
        ##----------------------------------------------------------------------
        ## Get //
        ##----------------------------------------------------------------------
  
        if (exists(watch, envir = where, inherits = FALSE)) {
          if (missing(v)) {
            ## Control for reactives that have been unset in the meantime //
            if (exists(watch, envir = where_watch[[HASH]], inherits = FALSE)) {
              if (.tracelevel == 1) {
                message(paste0("retrieve (", id, " watching ", watch, ")"))
              }
              hash_0 <- where[[HASH]][[watch]][[watch]]
              hash_1 <- where[[HASH]][[watch]][[id]]
              if (hash_0 != hash_1) {
                if (.tracelevel == 1) {
                  message(paste0("update based on contract (", 
                    id, " watching ", watch, ")"))
                  message(paste0("hash watch/watch old: ", hash_0))
                  message(paste0("hash watch/id old: ", hash_1))
                }
                VALUE <<- BINDING_CONTRACT
                hash_1 <- where[[HASH]][[watch]][[id]] <- hash_0
                where[[HASH]][[id]][[id]] <- hash_0
                where[[HASH]][[id]][[watch]] <- hash_0
                if (.tracelevel == 1) {
                  message(paste0("hash watch/watch new: ", hash_0))
                  message(paste0("hash watch/id new: ", hash_1))
                }
              } else {
                if (.tracelevel == 1) {
                  message(paste0("in sync (", 
                    id, " watching: ", watch, ")"))
                }
              }
            } else {
              suppressWarnings(rm(list = watch, envir = where[[HASH]][[id]], 
                  inherits = FALSE))
              if (!exists(watch, envir = where_watch, inherits = FALSE)) {
                warning(paste0("Observed object has been removed: ", watch))
                unsetReactive(id = id, where = where, .hash_id = .hash_id)
                assign(id, NULL, where)
              }
            }
          }
        } else {
            if (STRICT) {
              VALUE <<- NULL
            }
          }
      
        if (.tracelevel == 1) {
          message("----- BINDING CONTRACT END -----")
        }
      
        VALUE
      }
    }),
    list(
      VALUE = as.name("value"), 
      BINDING_CONTRACT = substitute(.binding(x = where[[watch]])),
      HASH = as.name(".hash_id"),
      STRICT = as.name("strict")
    )
  )    
  
  return(out)
    
  }
)
