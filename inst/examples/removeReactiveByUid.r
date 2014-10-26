\dontrun{

setReactiveS3(id = "x_1", value = 10)
isReactive("x_1")
setReactiveS3(id = "x_2", value = function() "object-ref: {id: x_1}")

unsetReactiveByUid(uid = computeObjectUid("x_1"))
isReactive("x_1")
## --> `x_1` is now a regular/non-reactive object again

## Actual values still identical //
x_1
identical(x_2, x_1)

## Implications for objects referencing `x_1` //
x_1 <- 20
x_1
x_2
## --> no reactive binding to `x_1` anymore

setReactiveS3(id = "x_2", value = function() "object-ref: {id: x_1}",
              strict_get = 1)
x_1 <- 10
x_2

}
