\dontrun{
  
################################################################################
## Basics
################################################################################

##------------------------------------------------------------------------------  
## In parent environment (parent.frame()) //
##------------------------------------------------------------------------------

## NOTE
## BE CAREFUL WHAT YOU DO AS THIS ALTERS OBJECT IN .GlobalEnv!

setReactiveS3(id = "x_1", value = 10)
setReactiveS3(id = "x_2", 
  value = function() {
    ## object-ref: {id: x_1}
    x_1 * 2
  }
)
x_1
x_2
(x_1 <- 50)
x_2
## --> update
x_2
## --> cached value

## Explicit setting of object that actually "only" has a reactive
## binding to another variable (no mutually-bound object)
x_2 <- 500
x_2
## --> currently this passes the prerequisite check which might lead to 
## system ambiguity.
## This is already addressed in GitHub issue #5 and subject to change in 
## future releases

x_1 <- 100
## --> once the referenced variable is updated, the reactive binding
## kicks in again
x_2
## --> update

## Clean up //
rm(x_1)
rm(x_2)
resetRegistry()

##------------------------------------------------------------------------------  
## In custom environment //
##------------------------------------------------------------------------------

where <- new.env()

## Set variable that others can have reactive bindings to //
setReactiveS3(id = "x_1", value = 10, where = where)

## Set variable that has reactive binding to 'x_1'
setReactiveS3(id = "x_2", 
  value = function() {
    ############################
    ## Reference specification #
    ############################
    
    ## object-ref: {id: x_1, where: where, as: ref_1}
    
    ## NOTE
    ## All these are valid ways to specify the references after the part
    ## 'object-ref:' (obmitting the closing bracket)
    ##
    ##    {id: {id}}
    ##    --> default 'where' is used, i.e. 'parent.frame()' is used
    ##    Example: object-ref: {id: x_1}
    ##
    ##    {id: {id}}, where: {where}}
    ##    --> explicit 'where'. Can be the name of any environment object
    ##    that is accessible, i.e. that exists under this name when calling
    ##    'setReactive()'
    ##    Example: {id: x_1, where: where_1}
    ##
    ##    {id: {id}, where: {where}, as: {ref-id}}
    ##    --> additional specification of the name/id to use inside *this* 
    ##    function if it should differ from {id}.
    ##    Example: {id: x_1, where: where_1, as: my_ref}
    ##    --> you would then use objec 'my_ref' in the remainder of this 
    ##    function
    
    #############################
    ## Using referenced objects #
    #############################
    
    ## As we used the markup 
    ##
    ##                {id: x_1, where: where, as: ref_1}
    ##
    ## `setReactiveS3()` expects us to use 'ref_1' in the remainder 
    
    ref_1 * 2
    
  }, 
  where = where
)

## Get current variable value //
where$x_1
where$x_2
## --> value cached at initialization is used; no update as 'x_1' in 'where'
## has not changed yet
(where$x_1 <- 100)
## --> 'x_1' in 'where' is updated
where$x_2
## --> referenced value for 'x_1' in 'where' changed --> update and re-cache
where$x_2
## --> cached value is used until reference changes again
where$x_2
where$x_2
(where$x_1 <- 50)
where$x_2
## --> referenced value for 'x_1' in 'where' changed --> update and re-cache

## Clean up //
suppressWarnings(rm(where))
resetRegistry()

################################################################################
## Reactive scenarios
################################################################################

##------------------------------------------------------------------------------
## Scenario 1: one-directional (1)
##------------------------------------------------------------------------------

## Explanation //
## - Type/Direction: 
##   `A` references `B` 
## - Binding/Relationship: 
##   `A` uses value of `B` "as is", i.e. value of `A` identical to value of `B`

setReactiveS3(id = "x_1", value = 10)
setReactiveS3(id = "x_2", value = function() {
  ## object-ref: {id: x_1}
  x_1
})

x_1
x_2
(x_1 <- 50)
x_2
## --> as 'x_1' has changed 'x_2' changes according to its binding function

## NOTE
## After an initial call to `setReactiveS3()`, it does not matter if you set 
## (or get) values via 'setReactiveS3()' (or 'getReactive()') or 
## via '<-'/'assign()' (or '$'/'get()'):
setReactiveS3(id = "x_1", value = 100)
x_1
x_2  ## value after executing binding function
getReactive("x_2") ## cached value

##------------------------------------------------------------------------------
## Scenario: B observes A, B uses A in an arbitrary functional way 
## (functional relationship)
##------------------------------------------------------------------------------

setReactiveS3(
  id = "x_3", 
  value = function() {
    ## object-ref: {id: x_1}
    x_1 * 2
  }
)

x_1
x_3
(x_1 <- 10)
x_3 

## Clean up //
rm(x_1)
rm(x_2)
rm(x_3)
resetRegistry()

##------------------------------------------------------------------------------
## Scenario: B observes A, C observes B and A 
## (arbitrary functional relationship)
##------------------------------------------------------------------------------

setReactiveS3(id = "x_1", value = 10)
setReactiveS3(
  id = "x_2", 
  value = function() {
    ## object-ref: {id: x_1}
    x_1 * 2
  }
)
setReactiveS3(
  id = "x_3", 
  value = function() {
    ## object-ref: {id: x_1}
    ## object-ref: {id: x_2}
    x_1 + x_2 + 100
  }
)

x_1
x_2
x_3
(x_1 <- 50)
x_3 
## --> change of 'x_1' affects both 'x_2' and 'x_3' --> update
x_3
x_2

(x_2 <- 500)
x_1
## --> not affected as no binding to either 'x_2' or 'x_3'
x_3
## --> affected by change of 'x_2' --> update

## Clean up //
rm(x_1)
rm(x_2)
rm(x_3)
resetRegistry()

##------------------------------------------------------------------------------
## Scenario: B observes A and A observers B
## - mutual
## - value identity
##------------------------------------------------------------------------------

setReactiveS3(id = "x_1", value = function() {
  ## object-ref: {id: x_2}
  x_2
  }
)
setReactiveS3(id = "x_2", value = function() {
  ## object-ref: {id: x_1}
  x_1
  }
)

## Note that mutually bound objects are initialized to 'NULL'
x_1
x_2

## Thus you need to set a specific value to *either one* of them
## (they both accept "set values")
## Setting 'x_1':
x_1 <- 10
x_1
x_2
x_1
x_2

## Setting 'x_2':
x_2 <- 100
x_2
x_1
x_2
x_1

## Clean up //
rm(x_1)
rm(x_2)
resetRegistry()

##------------------------------------------------------------------------------
## Scenario: B observes A and A observers B 
## - mutual
## - functional relationship
## - steady state
##------------------------------------------------------------------------------

setReactiveS3(id = "x_1", value = function() {
  ## object-ref: {id: x_2}
  x_2 * 2
  }
)
setReactiveS3(id = "x_2", value = function() {
  ## object-ref: {id: x_1}
  x_1 / 2
  }
)

## Setting 'x_1':
x_1 <- 10
x_1
x_2
x_1
x_2

## Setting 'x_2':
x_2 <- 100
x_2
x_1
x_2
x_1

## Clean up //
rm(x_1)
rm(x_2)
resetRegistry()

##------------------------------------------------------------------------------
## Scenario: B observes A and A observers B 
## - mutual
## - functional relationship
## - no steady state
##------------------------------------------------------------------------------

setReactiveS3(id = "x_1", value = function() {
  ## object-ref: {id: x_2}
  x_2
  }
)
setReactiveS3(id = "x_2", value = function() {
  ## object-ref: {id: x_1}
  x_1 * 2
  }
)

## Setting 'x_1':
x_1 <- 10
x_1
x_2
x_1
x_2
x_1
x_2

## Setting 'x_2':
x_2 <- 100
x_2
x_1
x_2
x_1
x_2
x_1

## Clean up //
rm(x_1)
rm(x_2)
resetRegistry()

##------------------------------------------------------------------------------
## Scenario: complex data structure (kind of like Reference Classes)
##------------------------------------------------------------------------------

x_1 <- new.env()  
x_2 <- new.env()  

## Set regular "complex" variable 'x_1' //
setReactiveS3(id = "field_1", value = TRUE, where = x_1)
setReactiveS3(
  id = "field_2", 
  value = data.frame(x_1 = 1:5, x_2 = letters[1:5]), 
  where = x_1
)

## Set variable with bindings //
setReactiveS3(id = "field_1", 
  value = function() {
    ## object-ref: {id: field_1,  where: x_1}
    !field_1
  },
  where = x_2
)
setReactiveS3(id = "field_2", 
  value = function() {
    ## object-ref: {id: field_2}, where: x_1}
    field_2[,-1,drop = FALSE]
  },
  where = x_2
)
         
x_1$field_1
x_1$field_2
x_2$field_1
x_2$field_2
(x_1$field_1 <- FALSE)
(x_1$field_2 <- data.frame(x_1 = letters[1:3], x_2 = 1:3))
x_2$field_1
x_2$field_2

## Clean up //
rm(x_1)
rm(x_2)
resetRegistry()

##------------------------------------------------------------------------------
## On a sidenote: cachiong mechanism
##------------------------------------------------------------------------------

## The caching mechanism implemented by this function relies on keeping
## a registry that stores certain information that are either required
## or useful in deciding whether an update should be triggered or not.
##
## The rule of thumb is as follows:
##    If B depends on A and A has not changed:
##    --> use the last cache value if B is requested
##    If B depends on A and A has changed
##    --> execute the reactive binding function and thus also update 
##        the cached value
##
## The decision is based on a comparison of checksum values as computed by 
## 'digest::digest()'. These are stored in option environment
##                       'getOption(".reactr")$.registry'
## which is accessible via the convenience function 
##                          'getRegistry()'
## Besides the actual checksum values, each entry - corresponding to a reactive 
## object - also contains some additional information:
## - id:    object ID as specified in call to 'setReactiveS3()'
## - uid:   object UID computed as follows:
##          'digest::digest(list(id = id, where = {where}))'
##          where '{where}' stands for the location provided via argument 
##          'where' in the call to 'setReactiveS3()'
## - {uid}: subenvironment corresponding to the object's UID. This contains
##          the object's own checksum 
## - {ref-uid} subenvironments for each referenced object should there exist
##             any. These in turn contain the referenced object's checksum that is
##             used to determine if an update is necessary or not.

## Hash registry //
registry <- getRegistry()
ls(registry)

setReactiveS3(id = "x_1", value = 10)
ls(registry)
uid_x_1 <- getObjectUid(id = "x_1", where = environment())
registry_x_1 <- registry[[uid_x_1]]
ls(registry_x_1)
registry_x_1$id
registry_x_1$uid
registry_x_1$where
registry_x_1[[uid_x_1]]
## --> contains own registry value

setReactiveS3(
  id = "x_2", 
  value = function() {
    ## object-ref: {id: x_1}
    x_1 * 2
  }
)
ls(registry)

uid_x_2 <- getObjectUid(id = "x_2", where = environment())
registry_x_2 <- registry[[uid_x_2]]
ls(registry_x_2)
registry_x_2$id
registry_x_2$uid
registry_x_2$where
registry_x_2[[uid_x_2]]
## --> contains own registry value
registry_x_2[[uid_x_1]]
## --> contains registry value of 'x_1' in '.GlobalEnv'

## Clean up //
rm(x_1)
rm(x_2)
resetRegistry()

##------------------------------------------------------------------------------
## Profiling //
##------------------------------------------------------------------------------

require("microbenchmark")
require("shiny")    
where <- environment()
res <- microbenchmark(
  "set x_1" = setReactiveS3(id = "x_1", value = 10, where = where),
  "get x_1" = get("x_1"),
  "set x_2" = setReactiveS3(
    id = "x_2", 
    value = function() {
      ## object-ref: {id: x_1}
      x_1 * 2
    }
  ),
  "get x_2" = get("x_2"),
  "change x_1" = assign("x_1", 100),
  "get x_2 (2)" = get("x_2"),
  "set x_3" = setShinyReactive(id = "x_3", value = 10, where = where),
  "get x_3" = get("x_3"),
  "set x_4" = setShinyReactive(id = "x_4", value = reactive(x_3 * 2), where = where),
  "get x_4" = get("x_4"),
  "set x_5" = assign("x_3", 10),
  "get x_5" = get("x_3"),
  control = list(order = "inorder")
)
res

# Unit: microseconds
#         expr      min       lq       mean   median        uq      max neval
#      set x_1 1025.184 1163.771 1304.73738 1286.662 1406.5925 2784.756   100
#      get x_1   18.952   20.729   25.82828   21.322   27.8360  130.887   100
#      set x_2 3474.727 3891.671 4149.34659 4093.332 4244.9475 5124.734   100
#      get x_2   92.392  101.275  117.46108  104.828  123.4845  261.775   100
#   change x_1  145.101  159.908  183.49688  174.122  196.0350  294.349   100
#  get x_2 (2)  840.402  937.531 1039.03075 1024.888 1082.6320 2476.787   100
#      set x_3 1216.481 1358.324 1561.13406 1432.651 1574.4950 6664.582   100
#      get x_3    4.738    5.330    5.87540    5.331    5.9230   13.622   100
#      set x_4 1849.003 2086.198 2279.68636 2211.163 2308.5880 5186.327   100
#      get x_4  944.046 1066.938 1202.07100 1155.478 1236.3210 2681.113   100
#      set x_5    5.330    6.515   20.75265    6.515    7.4035 1305.318   100
#      get x_5    2.962    3.554    5.70957    4.146    4.7380   85.284   100

##------------------------------------------------------------------------------
## References to environments //
##------------------------------------------------------------------------------

## Illustration that references to environments are not removed if the 
## referenced environment is removed:

env_1 <- new.env()
env_1$x <- 10
env_2 <- new.env()
env_2$env_1 <- env_1

## See if they are really the same //
env_2$env_1
env_1
identical(env_2$env_1, env_1)

env_2$env_1$x
env_1$x <- 100
env_2$env_1$x

## Removing 'env_1'
rm(env_1)
env_2$env_1
## --> still there

## Reassigning 'env_1'
env_1 <- new.env()
identical(env_2$env_1, env_1)
## --> not identical anymore

## This is the reason why method 'ensureIntegrity()` of class `ReactiveObject.S3`
## exists and why in certain situations the re-sync of registry references 
## must/should be ensured

}
