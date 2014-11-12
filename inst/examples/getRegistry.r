\dontrun{

## Set reactive so that there are elements in registry //  
resetRegistry()
setReactive(id = "x_1", value = Sys.time())  
  
registry <- getRegistry()
registry
ls(registry)
identical(registry, getOption("reactr")$.registry)

## Clean up //
resetRegistry()

}
