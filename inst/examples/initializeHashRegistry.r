\dontrun{

## Make sure you remove the options that might have been loaded 
## at package startup //
options("reactr" = NULL)
getOption("reactr")

## Initialize hash registry //
initializeHashRegistry()
getOption("reactr")
getOption("reactr")$.hash
ls(getHashRegistry())

}
