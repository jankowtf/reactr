\dontrun{

## NOTE //
## This function should typically only be called inside 'unsetReactive()'
## as it manages the internal registry!

## Start with a clean registry //
resetRegistry()

where <- new.env()
setReactiveS3(id = "x_1", value = 10, where = where)
setReactiveS3(id = "x_2", 
  value = function() .ref_1 <- get("x_1"),
  where = where
)

## Insepct registry before removal //
showRegistry()

removeFromRegistry(id = "x_1", where = where)
showRegistry()
removeFromRegistry(id = "x_2", where = where)
showRegistry()

## Sanity of actual cached values is not affected by this unless other values
## for `strict_get` are chosen
where$x_1
where$x_2

}
