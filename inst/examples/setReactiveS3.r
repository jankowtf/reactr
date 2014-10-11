\dontrun{
  
################################################################################
## Basics
################################################################################

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
    
    ## [@reactive-ref: x_1 in where as ref_1]
    
    ## NOTE
    ## All these are valid ways to specify the references after the part
    ## '@reactive-ref:' (obmitting the closing bracket)
    ##
    ##    {id}
    ##    --> default 'where' is used, i.e. 'parent.frame()' is used
    ##    Example: [@reactive-ref: x_1]
    ##
    ##    {id} in {where}
    ##    --> explicit 'where'. Can be the name of any environment object
    ##    that is accessible, i.e. that exists under this name when calling
    ##    'setReactive()'
    ##    Example: x_1 in where_1
    ##
    ##    {id} in {where} as {ref-name}
    ##    --> additional specification of the name/id to use inside *this* 
    ##    function if it should differ from {id}.
    ##    Example: x_1 in where_1 as my_ref
    ##    --> you would then use objec 'my_ref' in the remainder of this 
    ##    function
    
    #############################
    ## Using referenced objects #
    #############################
    
    ## As we used the markup 
    ##
    ##                      x_1 in where as ref_1
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
resetHashRegistry()

##------------------------------------------------------------------------------  
## In custom parent environment (parent.frame()) //
##------------------------------------------------------------------------------

## NOTE
## BE CAREFUL WHAT YOU DO AS THIS ALTERS OBJECT IN .GlobalEnv!

setReactiveS3(id = "x_1", value = 10)
setReactiveS3(id = "x_2", 
  value = function() {
    ## [@reactive-ref: x_1]
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
resetHashRegistry()

################################################################################
## Reactive scenarios
################################################################################

##------------------------------------------------------------------------------
## Scenario: B observes A and B = A (value identity)
##------------------------------------------------------------------------------

setReactiveS3(id = "x_1", value = 10)
setReactiveS3(
  id = "x_2", 
  value = function() {
    ## [@reactive-ref: x_1]
    x_1
  }
)
x_1
x_2
(x_1 <- 50)
x_2

## NOTE
## It does not matter if you set (or get) values via 'setReactiveS3()' 
## (or 'getReactive()') or via '<-'/'assign()' (or '$'/'get()')
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
    ## [@reactive-ref: x_1]
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
resetHashRegistry()

##------------------------------------------------------------------------------
## Scenario: B observes A, C observes B and A 
## (arbitrary functional relationship)
##------------------------------------------------------------------------------

setReactiveS3(id = "x_1", value = 10)
setReactiveS3(
  id = "x_2", 
  value = function() {
    ## [@reactive-ref: x_1]
    x_1 * 2
  }
)
setReactiveS3(
  id = "x_3", 
  value = function() {
    ## [@reactive-ref: x_1]
    ## [@reactive-ref: x_2]
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
resetHashRegistry()

##------------------------------------------------------------------------------
## Scenario: B observes A and A observers B
## - mutual
## - value identity
##------------------------------------------------------------------------------

setReactiveS3(id = "x_1", value = function() {
  ## [@reactive-ref: x_2]
  x_2
  }
)
setReactiveS3(id = "x_2", value = function() {
  ## [@reactive-ref: x_1]
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
resetHashRegistry()

##------------------------------------------------------------------------------
## Scenario: B observes A and A observers B 
## - mutual
## - functional relationship
## - steady state
##------------------------------------------------------------------------------

setReactiveS3(id = "x_1", value = function() {
  ## [@reactive-ref: x_2]
  x_2 * 2
  }
)
setReactiveS3(id = "x_2", value = function() {
  ## [@reactive-ref: x_1]
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
resetHashRegistry()

##------------------------------------------------------------------------------
## Scenario: B observes A and A observers B 
## - mutual
## - functional relationship
## - no steady state
##------------------------------------------------------------------------------

setReactiveS3(id = "x_1", value = function() {
  ## [@reactive-ref: x_2]
  x_2
  }
)
setReactiveS3(id = "x_2", value = function() {
  ## [@reactive-ref: x_1]
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
resetHashRegistry()

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
    ## [@reactive-ref: field_1 in x_1]
    !field_1
  },
  where = x_2
)
setReactiveS3(id = "field_2", 
  value = function() {
    ## [@reactive-ref: field_2 in x_1]
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
resetHashRegistry()

##------------------------------------------------------------------------------
## On a sidenote: cachiong mechanism
##------------------------------------------------------------------------------

## The caching mechanism implemented by this function relies on keeping
## a hash registry that stores certain information that are either required
## or useful in deciding whether an update should be triggered or not.
##
## The rule of thumb is as follows:
##    If B depends on A and A has not changed:
##    --> use the last cache value if B is requested
##    If B depends on A and A has changed
##    --> execute the reactive binding function and thus also update 
##        the cached value
##
## The decision is based on a comparison of hash values as computed by 
## 'digest::digest()'. These are stored in option environment
##                       'getOption(".reactr")$.hash'
## which is accessible via the convenience function 
##                          'getHashRegistry()'
## Besides the actual hash values, each entry - corresponding to a reactive 
## object - also contains some additional information:
## - id:    object ID as specified in call to 'setReactiveS3()'
## - uid:   object UID computed as follows:
##          'digest::digest(list(id = id, where = {where}))'
##          where '{where}' stands for the location provided via argument 
##          'where' in the call to 'setReactiveS3()'
## - {uid}: subenvironment corresponding to the object's UID. This contains
##          the object's own hash 
## - {ref-uid} subenvironments for each referenced object should there exist
##             any. These in turn contain the referenced object's hash that is
##             used to determine if an update is necessary or not.

## Hash registry //
hash <- getHashRegistry()
ls(hash)

setReactiveS3(id = "x_1", value = 10)
ls(hash)
uid_x_1 <- getReactiveUid(id = "x_1", where = environment())
hash_x_1 <- hash[[uid_x_1]]
ls(hash_x_1)
hash_x_1$id
hash_x_1$uid
hash_x_1$where
hash_x_1[[uid_x_1]]
## --> contains own hash value

setReactiveS3(
  id = "x_2", 
  value = function() {
    ## [@reactive-ref: x_1]
    x_1 * 2
  }
)
ls(hash)

uid_x_2 <- getReactiveUid(id = "x_2", where = environment())
hash_x_2 <- hash[[uid_x_2]]
ls(hash_x_2)
hash_x_2$id
hash_x_2$uid
hash_x_2$where
hash_x_2[[uid_x_2]]
## --> contains own hash value
hash_x_2[[uid_x_1]]
## --> contains hash value of 'x_1' in '.GlobalEnv'

## Clean up //
rm(x_1)
rm(x_2)
resetHashRegistry()

##------------------------------------------------------------------------------
## Profiling //
##------------------------------------------------------------------------------

require("microbenchmark")
    
res_bt_1 <- microbenchmark(
  "set x_1" = setReactiveS3(id = "x_1", value = 10, where = where),
  "set x_3" = assign("x_3", 10),
  "get x_1" = get("x_1"),
  "get x_3" = get("x_3"),
  "set x_2" = setReactiveS3(
    id = "x_2", 
    value = function() {
      ## [@reactive-ref: x_1]
      x_1 * 2
    }
  ),
  "get x_2" = get("x_2"),
  "change x_1" = assign("x_1", 100),
  "get x_2 (2)" = get("x_2"),
  control = list(order = "inorder")
)
res_bt_1

}
