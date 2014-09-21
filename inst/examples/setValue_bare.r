\dontrun{

################################################################################
## Binding type 1 //
################################################################################
 
## This is based on 'makeActiveBinding()' and respective boilerplate code 

where <- new.env()

## Set variable that can be monitored by others //
setValue_bare(id = "x_1", value = 10, where = where)

## Get current variable value //
where$x_1

##------------------------------------------------------------------------------
## Binding scenario: identical
##------------------------------------------------------------------------------

## Set variable that monitors 'x_1' //
## Binding contract: identical
setValue_bare(id = "x_2", where = where, watch = "x_1", binding = function(x) {x})

## When 'x_1' changes, 'x_2' changes accordingly:
## NOTE:
## When retrieving the value of 'x_2', you always retrieve a *cached* value 
## unless for the first retrieval after the observed variable 'x_1' has changed.
## That way, the function that binds 'x_1' to 'x_2' (the binding contract) 
## does not need to be executed each time, but only when it's actually required
where$x_1
where$x_2  ## cached value
where$x_1 <- 100
where$x_1  
where$x_2  ## value after executing binding function
where$x_2  ## cached value
where$x_2  ## cached value
where$x_1 <- 10
where$x_2  ## value after executing binding function
where$x_2  ## cached value
where$x_2  ## cached value

## NOTE:
## It does not matter if you set (or get) values via 'setValue_bare()' 
## (or 'getValue()') or via '<-'/'assign()' (or '$'/'get()')
setValue_bare(id = "x_1", value = 100, where = where)

where$x_1
where$x_2  ## value after executing binding function
where$x_2  ## cached value
getValue("x_2", where = where) ## cached value

##------------------------------------------------------------------------------
## Binding scenario: arbitrary functional relationship
##------------------------------------------------------------------------------

## Set variable that monitors 'x_1' //
setValue_bare(id = "x_3", where = where, watch = "x_1", binding = function(x) {x + 100})

where$x_1
where$x_3
where$x_1 <- 10
where$x_3 

##------------------------------------------------------------------------------
## Binding scenario: mutual
##------------------------------------------------------------------------------

## Set variables that are mutually bound //
where <- new.env()  

## Set '.tracelevel = 1' if you'd like to be able to understand what's actually
## going on
.tracelevel <- 0
    
setValue_bare(id = "x_1", where = where, watch = "x_2", 
  mutual = TRUE, .tracelevel = .tracelevel)
setValue_bare(id = "x_2", where = where, watch = "x_1", 
  mutual = TRUE, .tracelevel = .tracelevel
)

## Initial default values //
where$x_1
where$x_2

## Change any one of the mutually bound variables //
where$x_1 <- 300
where$x_1
where$x_2
where$x_2 <- 500
where$x_2
where$x_1

##------------------------------------------------------------------------------
## Binding scenario: multi-way
##------------------------------------------------------------------------------

## Set '.tracelevel = 1' if you'd like to be able to understand what's actually
## going on
.tracelevel <- 0

where <- new.env()      

## Set variables that are mutually bound //
setValue_bare(id = "x_1", where = where, watch = "x_2", 
  mutual = TRUE, .tracelevel = .tracelevel)
setValue_bare(id = "x_2", where = where, watch = "x_1", 
  mutual = TRUE, .tracelevel = .tracelevel
)
setValue_bare(id = "x_3", where = where, watch = "x_2", 
  binding = function(x) {x + 100}, .tracelevel = .tracelevel
)

where$x_1 <- 100
where$x_1
where$x_2
where$x_3

where$x_2 <- 200
where$x_1
where$x_2
where$x_3

## Disregarded:
where$x_3 <- 500
where$x_1
where$x_2
where$x_3

##------------------------------------------------------------------------------
## Binding scenario: multi-way with non-standard binding
##------------------------------------------------------------------------------

## Set '.tracelevel = 1' if you'd like to be able to understand what's actually
## going on
.tracelevel <- 0

where <- new.env()  

## Set variables that are mutually bound //
setValue_bare(id = "x_1", where = where, watch = "x_2", 
  mutual = TRUE, binding = function(x) {x/2}, .tracelevel = .tracelevel)
setValue_bare(id = "x_2", where = where, watch = "x_1", 
  mutual = TRUE, binding = function(x) {x * 2}, .tracelevel = .tracelevel
)

where$x_1 <- 100
where$x_1
where$x_2

where$x_2 <- 500
where$x_1
where$x_2

##------------------------------------------------------------------------------
## Binding scenario: complex data structure (kind of like Reference Classes)
##------------------------------------------------------------------------------

x_1 <- new.env()  
x_2 <- new.env()  

## Set regular "complex" variable 'x_1' //
setValue_bare(id = "field_1", value = TRUE, where = x_1)
setValue_bare(id = "field_2", value = data.frame(x_1 = 1:5, x_2 = letters[1:5]), 
  where = x_1)

## Set variable with bindings //
setValue_bare(id = "field_1", where = x_2, watch = "field_1", where_watch = x_1, 
         binding = function(x) {!x})
setValue_bare(id = "field_2", where = x_2, watch = "field_2", where_watch = x_1, 
         binding = function(x) {x[,-1,drop = FALSE]})

x_1$field_1
x_1$field_2
x_2$field_1
x_2$field_2

################################################################################
## Binding type 2 //
################################################################################

## This is the LEGACY (!) way of defining reactive bindings and is subject
## to being deprecated in furture package versions.

where <- new.env()  
  
setValue_bare(id = "x_1", value = Sys.time(), where = where, binding_type = 2)
getValue(id = "x_1", where = where)

binding <- substitute(function(x) {
  x + 60*60*24
})
setValue_bare(id = "x_2", where = where, binding = binding, watch = "x_1", 
         binding_type = 2)
getValue(id = "x_2", where = where)  
  
## Change value of monitored variable //
setValue_bare(id = "x_1", value = Sys.time(), where = where, binding_type = 2)
getValue(id = "x_1", where = where)  
getValue(id = "x_2", where = where) 

##------------------------------------------------------------------------------
## Profiling //
##------------------------------------------------------------------------------

require("microbenchmark")
    
## Binding type 1 //
where <- new.env()

res_bt_1 <- microbenchmark(
  "1" = setValue_bare(id = "x_1", value = 10, where = where),
  "2" = getValue(id = "x_1", where = where),
  "3" = setValue_bare(id = "x_2", where = where, watch = "x_1",
    binding = function(x) {x + 100}),
  "4" = getValue(id = "x_2", where = where),
  "5" = setValue_bare(id = "x_1", value = 100, where = where),
  "6" = getValue(id = "x_2", where = where),
  control = list(order = "inorder")
)
res_bt_1

## Binding type 2 //
where <- new.env()  

res_bt_2 <- microbenchmark(
  "1" = setValue_bare(id = "x_1", value = Sys.time(), where = where,
                 binding_type = 2),
  "2" = getValue(id = "x_1", where = where),
  "3" = setValue_bare(id = "x_2", where = where,
    binding = substitute(function(x) {
        x + 60*60*24
      }), watch = "x_1", binding_type = 2),
  "4" = getValue(id = "x_2", where = where),
  "5" = setValue_bare(id = "x_1", value = Sys.time(), where = where,
                 binding_type = 2),
  "6" = getValue(id = "x_2", where = where),
  control = list(order = "inorder")
)
res_bt_2

}
