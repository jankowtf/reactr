\dontrun{

options("reactr" = NULL)
getOption("reactr")
container <- initializeOptionContainer("reactr")
identical(getOption("reactr")$options, container$options)
identical(getOption("reactr")$.registry, container$.registry)  

}
