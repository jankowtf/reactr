\dontrun{
  
where <- new.env()  
  
setValue(id = "x_1", value = Sys.time(), where = where, binding_type = 2)
getValue(id = "x_1", where = where)

binding <- substitute(function(x) {
  x + 60*60*24
})
setValue(id = "x_2", where = where, binding = binding, watch = "x_1", 
         binding_type = 2)
getValue(id = "x_2", where = where)  
  
## Change value of monitored variable //
setValue(id = "x_1", value = Sys.time(), where = where, binding_type = 2)
getValue(id = "x_1", where = where)  
getValue(id = "x_2", where = where)    
 
##----------------------------------------------------------------------------
## Setting via 'makeActiveBinding' //
##----------------------------------------------------------------------------

where <- new.env()

## Set variable that can be monitored by others //
setValue(
  id = "x_1", 
  value = 10, 
  where = where
)

## Set with explicit binding (not needed; just for illustration)
setValue(
  id = "x_1", 
  value = 20, 
  where = where, 
  binding = getBoilerplateCode()
)

## Get current variable value //
where$x_1

## Set variable that monitors 'x_1' //
## Type of binding: identical
setValue(
  id = "x_2", 
  where = where, 
  watch = "x_1", 
  binding = function(x) {x}
)

## When 'x_1' changes, 'x_2' changes accordingly:
## NOTE:
## When retrieving the value of 'x_2', you always retrieve a *cached* value 
## unless for the first retrieval after the observed variable 'x_1' has changed.
## That way, the function that binds 'x_1' to 'x_2' does not need to 
## be executed each time, but only when it's actually required
where$x_1
where$x_2  ## cached value
where$x_1 <- 10
where$x_1  
where$x_2  ## value after executing binding function
where$x_2  ## cached value
where$x_2  ## cached value
where$x_1 <- 20
where$x_2  ## value after executing binding function
where$x_2  ## cached value
where$x_2  ## cached value

## NOTE:
## It does not matter if you set (get) values via 'setValue()' ('getValue()')
## or via '<-'/'assign()' ('$'/'get()')
setValue(id = "x_1", value = 100, where = where)
where$x_1
where$x_2  ## value after executing binding function
where$x_2  ## cached value
getValue("x_2", where = where)

## Set variable that monitors 'x_1' //
## Type of binding: arbitrary modification
setValue(
  id = "x_3", 
  where = where, 
  watch = "x_1", 
  binding = function(x) {x + 100}
)
where$x_1
where$x_3
where$x_1 <- 10
where$x_3 

## Set variable that can both be monitored and also monitors 'x_2' //
## Type of binding: arbitrary modification
## TODO

##----------------------------------------------------------------------------
## Profiling //
##----------------------------------------------------------------------------

require("microbenchmark")

where <- new.env()  
res_1 <- microbenchmark(
  "1" = setValue(id = "x_1", value = Sys.time(), where = where, binding_type = 2),
  "2" = getValue(id = "x_1", where = where),
  "3" = setValue(id = "x_2", where = where,
    binding = substitute(function(x) {
        x + 60*60*24
      }), watch = "x_1", binding_type = 2),
  "4" = getValue(id = "x_2", where = where),
  "5" = setValue(id = "x_1", value = Sys.time(), where = where,
                 binding_type = 2),
  "6" = getValue(id = "x_2", where = where),
  control = list(order = "inorder", warmup = 10)
)

##-----------

where <- new.env()
res_2 <- microbenchmark(
  "1" = setValue(id = "x_1", value = 10, where = where),
  "2" = getValue(id = "x_1", where = where),
  "3" = setValue(id = "x_2", where = where, watch = "x_1", 
                 binding = function(x) x + 100),
  "4" = getValue(id = "x_2", where = where),
  "5" = setValue(id = "x_1", value = 100, where = where),
  "6" = getValue(id = "x_2", where = where),
  control = list(order = "inorder", warmup = 10)
)

res_1
res_2

}
