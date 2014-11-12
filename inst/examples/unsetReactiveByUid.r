\dontrun{

setReactive(id = "x_1", value = 10)
unsetReactiveByUid(uid = computeObjectUid("x_1"))
isReactive("x_1")

## Clean up //
rmReactive("x_1")

}
