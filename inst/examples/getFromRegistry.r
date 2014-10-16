\dontrun{

## Example object //
resetRegistry()
setReactiveS3(id = "x_1", value = 10)
x_1

## Get from registry //
ls(getRegistry())
obj <- getFromRegistry(id = "x_1")
obj$id
obj$uid
obj$where

## Clean up //
obj$remove()
try(x_1)
## --> removes itself from 'where' and from registry
ls(getRegistry())

}
