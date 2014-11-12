\dontrun{

## Start with fresh registry //
resetRegistry()

## Reactive object w/o references //
setReactive(id = "x_1", value = 10)
getChecksum(id = "x_1")
## --> equivalent to:
digest::digest(10)

## Reactive object with references //
setReactive(id = "x_2", value = function() "object-ref: {id: x_1}")
getChecksum(id = "x_2")
## --> equivalent to:
digest::digest(x_1)

## Clean up //
rmReactive("x_1")
rmReactive("x_2")

}
