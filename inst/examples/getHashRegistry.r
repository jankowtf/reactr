\dontrun{

## Set reactive so that there are elements in hash registry //  
resetHashRegistry()
setReactiveS3(id = "x_1", value = Sys.time())  
  
hash <- getHashRegistry()
hash
ls(hash)
identical(hash, getOption("reactr")$.hash)

## Clean up //
resetHashRegistry()

}
