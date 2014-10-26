\dontrun{

## Set reactives so registry has entries //  
setReactiveS3(id = "x_1", value = 10)
setReactiveS3(id = "x_2", value = function() {
  .ref_1 <- get("x_1")
})

## Inspect current state of registry //
registry <- getRegistry()
showRegistry()
## --> two entries corresponding to invisible objects associated to `x_1` 
## and `x_2`

## Reset //
resetRegistry()
showRegistry()
## --> empty 

}
