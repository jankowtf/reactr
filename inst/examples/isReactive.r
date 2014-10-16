\dontrun{

## Reactive objects //
setReactiveS3(id = "x_1", value = 10)
isReactive(id = "x_1")
unsetReactive("x_1")
isReactive(id = "x_1")

## Non-reactive/regular objects //
x_2 <- 10
isReactive(id = "x_2")
obj <- ReactiveObject.S3()
isReactive("obj")

}
