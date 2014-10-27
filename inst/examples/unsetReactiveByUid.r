\dontrun{

setReactiveS3(id = "x_1", value = 10)
unsetReactiveByUid(uid = computeObjectUid("x_1"))
isReactive("x_1")

## Clean up //
removeReactive("x_1")

}
