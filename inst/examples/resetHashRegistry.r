\dontrun{

## Set reactives so hash registry has entries //  
setReactiveS3(id = "x_1", value = 10)
setReactiveS3(id = "x_2", value = function() {
  .ref_1 <- get("x_1", envir = where)
})

## Inspect current state of hash registry //
hash <- getHashRegistry()
ls(hash)

## Reset //
resetHashRegistry()
ls(hash)
## --> empty 

}
