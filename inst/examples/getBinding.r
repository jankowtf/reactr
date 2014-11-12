\dontrun{

## Start with fresh registry //
resetRegistry()

## Reactive object w/o references //
setReactive(id = "x_1", value = 10)
(binding <- getBinding(id = "x_1"))
identical(binding, getFromRegistry("x_1")$.func)

## Reactive object with references //
setReactive(id = "x_2", value = function() "object-ref: {id: x_1}")
(binding <- getBinding(id = "x_2"))
identical(binding, getFromRegistry("x_2")$.func)

## Clean up //
rmReactive("x_1")
rmReactive("x_2")

}
