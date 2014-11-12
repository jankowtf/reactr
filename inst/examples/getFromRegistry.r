\dontrun{

## Example object //
resetRegistry()
setReactive(id = "x_1", value = 10)
x_1

## Get from registry //
showRegistry()
obj <- getFromRegistry(id = "x_1")
obj$.id
obj$.uid
obj$.where

## Clean up //
obj$.remove()
try(x_1)
## --> removes itself from 'where' and from registry
showRegistry()

}
