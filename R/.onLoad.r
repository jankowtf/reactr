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
  setOldClass("Reactr_BindingContractSet.S3")
  setOldClass("Reactr.BindingContractGet.S3")
  setOldClass("Reactr.BindingContractCombined.S3")
}
