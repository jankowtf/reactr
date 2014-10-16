\dontrun{

initializeRegistry()
getOption("reactr")$.registry
removeHashRegistry()
getOption("reactr")$.registry

## NOTE
## 'getRegistry()' calls 'initializeRegistry()' in order to ensure
## it, so it will never return 'NULL' as 'getOption("reactr")$.registry' would
getRegistry()

}
