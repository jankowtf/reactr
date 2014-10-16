\dontrun{

## NOTE //
## This function should typically only be called inside 'unsetReactive()'
## as it manages the internal registry!

## Start with a clean registry //
resetRegistry()

where <- new.env()
setReactiveS3(id = id, value = 10, where = where)
setReactiveS3(id = id_2, 
  value = function() .ref_1 <- get("x_1", envir = where),
  where = where
)

## Insepct registry before removal //
ls(getRegistry())

removeFromRegistry(id = "x_1", where = where)
ls(getRegistry())
removeFromRegistry(id = "x_2", where = where)
ls(getRegistry())

## Sanity of actual cached values is not affected by this //
where$x_1
where$x_2

}
