\dontrun{

initializeHashRegistry()
getOption("reactr")$.hash
removeHashRegistry()
getOption("reactr")$.hash

## NOTE
## 'getHashRegistry()' calls 'initializeHashRegistry()' in order to ensure
## it, so it will never return 'NULL' as 'getOption("reactr")$.hash' would
getHashRegistry()

}
