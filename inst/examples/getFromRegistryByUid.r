\dontrun{

## Example object //
resetRegistry()
setReactiveS3(id = "x_1", value = 10)
x_1

## Get from registry //
uid <- computeObjectUid(id = "x_1")
showRegistry()
obj <- getFromRegistryByUid(uid = uid)
obj$.id
obj$.uid
obj$.where

## Clean up //
obj$.remove()
try(x_1)
## --> removes itself from 'where' and from registry
showRegistry()

}
