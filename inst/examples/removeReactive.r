\dontrun{

## Start with fresh registry //
resetRegistry()

## Non-strict //
where <- new.env()  
setReactiveS3(id = "x_1", value = 10, where = where)
setReactiveS3(id = "x_2", 
  value = function(refs = list(x_1 = where)) {x_1 * 2}, 
  where = where)

removeReactive(id = "x_1", where = where)
exists("x_1", envir = where, inherits = FALSE)
where$x_2

## Strict //
where <- new.env()  
setReactiveS3(id = "x_2", 
  value = function(refs = list(x_1 = where)) {x_1 * 2}, 
  where = where, 
  strict = TRUE
)
removeReactive(id = "x_1", where = where)
try(where$x_2)
## --> as referenced object 'x_1' does not exist anymore

}
