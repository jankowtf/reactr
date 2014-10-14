\dontrun{

## Start with a fresh hash registry //
resetHashRegistry()

setReactiveS3(id = "x_1", value = 10)
setReactiveS3(id = "x_2", 
  value = function(refs = list(x_1 = parent.frame())) x_1 * 2
)

## Illustrate reactiveness //
x_1
x_2
x_1 <- 50
x_1 
x_2

## Unset reactive --> turn it into a regular object again //
unsetReactive(id = "x_1", where = where)

## Illustrate removed reactiveness //
x_1
x_2
x_1 <- 10
x_1
x_2
## --> 'x_1' is not a reactive object anymore; from now on, 'x_2' simply returns
## the last value that has been cached

## Implications with respect to hash registry //
## Entry corresponding to 'x_1' set in 'environment()' has been removed
exists(getReactiveUid(id = "x_1", where = where, getHashRegistry()))
## [1] FALSE

}
