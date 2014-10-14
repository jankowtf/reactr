#' @title
#' Prepare Reactive Instance (generic)
#'
#' @description
#' Prepares a suitable instance of a class governing the establishment of 
#' reactive behavior for the use inside \code{\link[reactr]{setReactiveS3}}.
#' 
#' @details
#' 
#' Currently, this means that an instance of class 
#' \code{\link[reactr]{Reactive.S3}} taken and certain objects/variables/values 
#' are transferred to the respective class fields. Furthermore, the function 
#' makes sure that all associated/necessary elements in the hash registry 
#' exist (see \code{\link[reactr]{getHashRegistry()}}).
#' 
#' @param input \strong{Signature argument}.
#'    Object containing a suitable object for preparation. 
#'    Typically, this corresponds to the instance of class 
#'    \code{\link[reactr]{Reactive.S3}}.
#' @param input \code{\link{character}}.
#'    UIDs of references if there exist any.
#' @template threedot
#' @example inst/examples/prepareReactiveInstance.r
#' @seealso \code{
#'     \link[reactr]{prepareReactiveInstance-Reactive.S3-method},
#'     \link[reactr]{Reactive.S3},
#'     \link[reactr]{setReactiveS3}
#' }
#' @template author
#' @template references
setGeneric(
  name = "prepareReactiveInstance",
  signature = c(
    "input"
  ),
  def = function(
    input = NULL,
    id = character(),
    value = NULL,
    where = parent.frame(), ## TODO: verify this!!!
    references = character(),
    ...
  ) {
    standardGeneric("prepareReactiveInstance")       
  }
)

#' @title
#' Prepare Reactive Instance (Reactive.S3-method) 
#'
#' @description 
#' See generic: \code{\link[reactr]{prepareReactiveInstance}}
#'      
#' @inheritParams prepareReactiveInstance
#' @param input \code{\link{Reactive.S3}}.
#' @return \code{\link{logical}}. \code{TRUE}: check passed; 
#'    \code{FALSE}: check not passed.
#' @example inst/examples/prepareReactiveInstance.r
#' @seealso \code{
#'    \link[reactr]{prepareReactiveInstance},
#'     \link[reactr]{setReactiveS3}
#' }
#' @template author
#' @template references
#' @export
#' @import digest
setMethod(
  f = "prepareReactiveInstance", 
  signature = signature(
    input = "Reactive.S3"
  ), 
  definition = function(
    input,
    value,
    where,
    references,
    ...
  ) {
    
  input$value <- value
  input$id <- id
  input$uid <- getReactiveUid(id = id, where = where)
#     expr <- substitute(digest::digest(list(id = ID, where = WHERE)), 
#     list(ID = input$id, WHERE = eval(where)))
  input$where <- where
  
  ## Ensure subenvironment in hash registry //
  if (!exists(input$uid, envir = input$hash)) {
    assign(input$uid, new.env(parent = emptyenv()), envir = input$hash)      
  }

  ## Ensure 'id' entry in hash registry //
  assign("id", id, envir = input$hash[[input$uid]])      
  ## Ensure 'uid' entry in hash registry //
  assign("uid", input$uid, envir = input$hash[[input$uid]])      
  ## Ensure 'where' entry in hash registry //
  assign("where", where, envir = input$hash[[input$uid]])     

  ## References //
  if (length(references)) {
    sapply(references, function(ref) {
      if (!exists(ref, envir = input$hash)) {
        assign(ref, new.env(parent = emptyenv()), envir = input$hash)      
        assign("checksum", digest::digest(NULL), envir = input$hash[[ref]])      
      }
      assign(ref, input$hash[[ref]], envir = input$references)
    })
  }
  
  return(input)
  
  }
)
