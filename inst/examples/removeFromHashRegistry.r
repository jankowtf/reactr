\dontrun{

## NOTE //
## This function should typically only be called inside 'unsetReactive()'
## as it manages the internal hash registry!

where <- new.env()
setReactive(id = "x_1", value = 10, where = where)
setReactive(id = "x_2", watch = "x_1", where = where)

## Insepct hash registry before removal //
ls(where[[.hash_id]])

removeFromHashRegistry(id = "x_1", where = where)
removeFromHashRegistry(id = "x_1", where = where)

## Insepct hash registry before removal //
ls(where$._HASH)

## Sanity of actual values is not affected by this //
where$x_1
where$x_2


}
