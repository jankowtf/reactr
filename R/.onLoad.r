#' On Load Hook
#'
#' @description 
#' On load hook.
#' 
#' @param libname 
#' @param pkgname
#' @template author
#' @template references
#' @export .onLoad
.onLoad <- function(libname, pkgname) {
#   setOldClass("BindingContractObserved.S3")
#   setOldClass("BindingContractObserving.S3")
#   setOldClass("BindingContractMutual.S3")
  
  envir <- new.env()
  envir$.hash <- new.env()
  options("reactr" = envir)
}
