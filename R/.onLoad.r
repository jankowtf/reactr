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
  setOldClass("Reactr.BindingContractMonitored.S3")
  setOldClass("Reactr.BindingContractMonitoring.S3")
  setOldClass("Reactr.BindingContractMutual.S3")
}
