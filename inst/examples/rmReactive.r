\dontrun{

## Start with fresh registry //
resetRegistry()

## Non-strict //
setReactiveS3(id = "x_1", value = 10)
setReactiveS3(id = "x_2", 
  value = function() {
    "object-ref: {id: x_1}"
    x_1 * 2
  }
)

rmReactive(id = "x_1")
exists("x_1", envir = environment(), inherits = FALSE)
x_2
## --> `x_2` not affected but only because `strict_get = 0`

## Strict: level 1 //
setReactiveS3(id = "x_1", value = 10)
setReactiveS3(id = "x_2", 
  value = function() {
    "object-ref: {id: x_1}"
    x_1 * 2
  },
  strict_get = 1
)
rmReactive(id = "x_1")
try(x_2)
## --> warning as referenced object `x_1` does not exist anymore

## Strict: level 2 //
setReactiveS3(id = "x_1", value = 10)
setReactiveS3(id = "x_2", 
  value = function() {
    "object-ref: {id: x_1}"
    x_1 * 2
  },
  strict_get = 2
)
rmReactive(id = "x_1")
try(x_2)
## --> error as referenced object `x_1` does not exist anymore

}
