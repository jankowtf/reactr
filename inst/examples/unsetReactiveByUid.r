\dontrun{

setReactiveS3(id = "x_1", value = 10)
setReactiveS3(
  id = "x_2", 
  value = function(refs = list(x_1 = .GlobalEnv)) {x_1}
)
unsetReactiveByUid(uid = getReactiveUid("x_1", .GlobalEnv))
x_1
identical(x_2, x_1)
x_1 <- 20
x_1
x_2
## --> no reactive binding to 'x_1' anymore; last-cached value is returned

## Reset again //
setReactiveS3(id = "x_1", value = 10)
x_1
x_2
## --> cached value still at '10' --> reactivity is revealed only after 
## changing 'x_1'
x_1 <- 20
x_1
x_2

}
