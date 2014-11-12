\dontrun{

setReactive(id = "x_1", value = 10)
setReactive(id = "x_2", value = function() "object-ref: {id: x_1}")

showPullRefs(id = "x_1")
showPullRefs(id = "x_2")

## Clean up //
rmReactive("x_1")
rmReactive("x_2")

}
