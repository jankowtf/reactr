\dontrun{

## NOTE //
## This function should typically only be called inside 'unsetReactive()'
## as it manages the internal hash registry!

## Start with a clean hash registry //
resetHashRegistry()

where <- new.env()
setReactiveS3(id = id, value = 10, where = where)
setReactiveS3(id = id_2, 
  value = function() .ref_1 <- get("x_1", envir = where),
  where = where
)

## Insepct hash registry before removal //
ls(getHashRegistry())

removeFromHashRegistry(id = "x_1", where = where)
ls(getHashRegistry())
removeFromHashRegistry(id = "x_2", where = where)
ls(getHashRegistry())

## Sanity of actual cached values is not affected by this //
where$x_1
where$x_2

}
