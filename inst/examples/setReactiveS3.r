\dontrun{
  
################################################################################
## Basics
################################################################################

##------------------------------------------------------------------------------  
## In parent environment (parent.frame()) //
##------------------------------------------------------------------------------

## NOTE
## Be careful what you do as this alters objects in .GlobalEnv due to 
## the default value of `where` being equal to  `parent.frame()`!

setReactiveS3(id = "x_1", value = 10)
setReactiveS3(id = "x_2", 
  value = function() {
    ########################################
    ## Unambiguously specifying references #
    ########################################
    
    ## Via YAML markup:
    "object-ref: {id: x_1, as: ref_1}"
    
    ## NOTE
    ## See vignette `Specifying reactive references` for details and alternative
    ## ways to specify references
    ## 
    ## All these are valid ways to specify the references after the part
    ## `object-ref:` (omitting the closing bracket)
    ##
    ##    {id: {id}}
    ##    --> default `where` is used, i.e. `parent.frame()` is used
    ##    Example: object-ref: {id: x_1}
    ##
    ##    {id: {id}}, where: {where}}
    ##    --> explicit `where`. Can be the name of any environment object
    ##    that is accessible, i.e. that exists under this name when calling
    ##    `setReactive()`
    ##    Example: {id: x_1, where: where_1}
    ##
    ##    {id: {id}, where: {where}, as: {ref-id}}
    ##    --> additional specification of the name/id to use inside *this* 
    ##    function if it should differ from {id}.
    ##    Example: {id: x_1, where: where_1, as: my_ref}
    ##    --> you would then use objec `my_ref` in the remainder of this 
    ##    function
    
    #############################
    ## Using referenced objects #
    #############################
    
    ## As we used the markup 
    ##
    ##                         {id: x_1, as: ref_1}
    ##
    ## `setReactiveS3()` expects us to use `ref_1` in the following 
    
    ref_1 * 2
  }
)
x_1
x_2
(x_1 <- 50)
x_2
## --> update
x_2
## --> cached value, no update

## Explicit setting of object that actually "only" has a reactive
## binding to another variable (no bi-directionality)
x_2 <- 500
x_2
## --> currently this passes the prerequisite check which might lead to 
## system ambiguities.
## This is already addressed in GitHub issue #5 and subject to change in 
## future releases

x_1 <- 100
## --> once the referenced variable is updated, the reactive binding
## kicks in again
x_2
## --> update

## Clean up //
removeReactive("x_1")
removeReactive("x_2")

##------------------------------------------------------------------------------  
## Typed //
##------------------------------------------------------------------------------

setReactiveS3(id = "x_1", value = 10, typed = TRUE)
setReactiveS3(id = "x_2", value = function() "object-ref: {id: x_1}")

x_1 <- 20
x_2
try(x_1 <- "hello world")

## Clean up //
removeReactive("x_1")
removeReactive("x_2")

##------------------------------------------------------------------------------  
## Verbose //
##------------------------------------------------------------------------------

setReactiveS3(id = "x_1", value = 10)
setReactiveS3(id = "x_2", value = function() "object-ref: {id: x_1}", verbose = TRUE)

x_1 <- 20
x_2
x_2
x_1 <- 30
x_2

## Clean up //
removeReactive("x_1")
removeReactive("x_2")

##------------------------------------------------------------------------------  
## No cache //
##------------------------------------------------------------------------------

## As no caching is required, there is no need to keep a registry either
## --> more lightweight and thus faster, but features like "bi-directional 
## bindings" and "push updates" are not available that way.

setReactiveS3(id = "x_1", value = 10, cache = FALSE)
setReactiveS3(id = "x_2", value = function() "object-ref: {id: x_1}", cache = FALSE)

showRegistry()
## --> empty

x_1 <- 20
x_2

## Clean up //
removeReactive("x_1")
removeReactive("x_2")

## No bi-directional bindings possible //
## --> results in infinite recursion
try(setReactiveS3(id = "x_1", value = function() "object-ref: {id: x_2}", 
                  cache = FALSE))
try(setReactiveS3(id = "x_2", value = function() "object-ref: {id: x_1}", 
                  cache = FALSE))

##------------------------------------------------------------------------------  
## In custom environment //
##------------------------------------------------------------------------------

where <- new.env()

## Set variable that others can have reactive bindings to //
setReactiveS3(id = "x_1", value = 10, where = where)

## Set variable that has reactive binding to `x_1`
setReactiveS3(id = "x_2", 
  value = function() {
    "object-ref: {id: x_1, as: ref_1}"
    ref_1 * 2
  }, 
  where = where
)

## Get current variable value //
where$x_1
where$x_2
## --> value cached at initialization is used; no update as `x_1` in `where`
## has not changed yet
(where$x_1 <- 100)
## --> `x_1` in `where` is updated
where$x_2
## --> referenced value for `x_1` in `where` changed --> update and re-cache
where$x_2
## --> cached value is used until reference changes again
where$x_2
where$x_2
(where$x_1 <- 50)
where$x_2
## --> referenced value for `x_1` in `where` changed --> update and re-cache

## Clean up //
removeReactive("x_1", where)
removeReactive("x_2", where)
suppressWarnings(rm(where))

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
  "object-ref: {id: x_1}"
  x_1
})

x_1
x_2
(x_1 <- 50)
x_2
## --> as `x_1` has changed `x_2` changes according to its binding function

## NOTE
## After an initial call to `setReactiveS3()`, it does not matter if you set 
## (or get) values via `setReactiveS3()` (or `getReactive()`) or 
## via `<-`/`assign()` (or `$`/`get()`):
setReactiveS3(id = "x_1", value = 100)
x_1
x_2  
## --> value after executing binding function
getReactive("x_2") 
## --> cached value
x_2 
## --> cached value


##------------------------------------------------------------------------------
## Scenario 1: one-directional (2)
##------------------------------------------------------------------------------

## Explanation //
## - Type/Direction: 
##   `A` references `B`   
## - Binding/Relationship:
##   `A` transforms value of `B` , i.e. value of `A` is the result of 
##   applying a function on the value of `B`

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
removeReactive("x_1")
removeReactive("x_2")
removeReactive("x_3")

##------------------------------------------------------------------------------
## Scenario 1: one-directional (3)
##------------------------------------------------------------------------------

## Explanation //
## - Type/Direction: 
##   `A` references `B` and `C`, `B` references `C`
## - Binding/Relationship: 
##   `A` transforms value of `B` , i.e. value of `A` is the result of 
##   applying a function on the value of `B`

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
    x_1 + x_2 * 2
  }
)

x_1
x_2
x_3
(x_1 <- 50)
x_3 
## --> change of `x_1` affects both `x_2` and `x_3` --> update
x_3
x_2

(x_2 <- 500)
x_1
## --> not affected as no binding to either `x_2` or `x_3`
x_3
## --> affected by change of `x_2` --> update

## Clean up //
removeReactive("x_1")
removeReactive("x_2")
removeReactive("x_3")

##------------------------------------------------------------------------------
## Scenario 4: bi-directional (1)
##------------------------------------------------------------------------------

## Explanation //
## - Type/Direction: 
##   `A` references `B` and `B` references `A` --> bidirectional binding type
## - Binding/Relationship: 
##   `A` uses value of `B` "as is" and `B` uses value of `A` "as is". 
##   This results in a steady state. 

setReactiveS3(id = "x_1", value = function() {
  ## object-ref: {id: x_2}
  x_2
})
setReactiveS3(id = "x_2", value = function() {
  ## object-ref: {id: x_1}
  x_1
  }
)

## Note that mutually bound objects are initialized to `NULL`
x_1
x_2

## Thus you need to set a specific value to *either one* of them
## (they both accept "set values")
## Setting `x_1`:
x_1 <- 10
x_1
x_2
x_1
## --> update cycle complete; from now own cached values can be used
x_2
x_1

## Setting `x_2`:
x_2 <- 100
x_2
x_1
x_2
## --> update cycle complete; from now own cached values can be used
x_1
x_2

## Clean up //
removeReactive("x_1")
removeReactive("x_2")

##------------------------------------------------------------------------------
## Scenario 5: bi-directional (2)
##------------------------------------------------------------------------------

## Explanation //
## - Type/Direction: 
##   `A` references `B` and `B` references `A` --> bidirectional binding type
## - Binding/Relationship: 
##   `A` uses transformed value of `B` and `B` uses transformed value of `A`. 
##   The binding functions used result in a steady state.

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

## NOTE
## Still a minor inconsistency with respect to initial values 
## (`numeric()` instead of `NULL`) depending on the structure of the binding
## function
x_1
x_2

## Setting `x_1`:
x_1 <- 10
x_1
x_2
x_1
x_2
x_1

## Setting `x_2`:
x_2 <- 100
x_2
x_1
x_2
x_1
x_2

## Clean up //
removeReactive("x_1")
removeReactive("x_2")

##------------------------------------------------------------------------------
## Scenario 6: bi-directional (3)
##------------------------------------------------------------------------------

## Explanation //
## - Type/Direction: 
##   `A` references `B` and `B` references `A` --> bidirectional binding type
## - Binding/Relationship: 
##   `A` uses transformed value of `B` and `B` uses transformed value of `A`. 
##   The binding functions used result in a **non-steady state**.

setReactiveS3(id = "x_1", value = function() {
  ## object-ref: {id: x_2}
  x_2 * 2
})
setReactiveS3(id = "x_2", value = function() {
  ## object-ref: {id: x_1}
  x_1 * 4
})

## Setting `x_1`:
x_1 <- 10
x_1
## --> 10 * 4 * 2 = 80
x_2
## --> 80 * 4 = 320
x_1
## --> 320 * 2 = 640
x_2
## --> 640 * 4 = 2560
x_1
## --> 2560 * 2 = 5120
## --> we never reach a steady state

## Setting `x_2`:
x_2 <- 1
x_2
x_1
x_2
x_1
x_2
x_1

## Clean up //
removeReactive("x_1")
removeReactive("x_2")

##------------------------------------------------------------------------------
## Pushing
##------------------------------------------------------------------------------

## The caching mechanism in combination with the registry mechanism used in
## this package allows "push updates", i.e. actively propagating system state
## changes to all objects that are referencing a certain system state.

setReactiveS3(id = "x_1", value = 10)
setReactiveS3(
  id = "x_2", 
  value = function() {
    ## object-ref: {id: x_1}
    tmp <- x_1 * 2
    message(paste0("Reference `x_1` changed: ", tmp))
    tmp
  },
  push = TRUE
)

x_1
x_2
## --> so far, this is no different from what we specified before

## The difference lies in the way changes of `x_1` are propagated:
## Up until now, objects that reference other objects would only be notified 
## of changes in their references in a "pull manner": 
## they would not be updated until explicitly requested.
## However, when using `pull = TRUE`, whenever an object that is referenced in
## other objects (i.e. `x_1`) changes, it actually calls all of its push 
## references (i.e. `x_2`), i.e. it is "pushing" the change throughout the 
## system. 

x_1 <- 100
## --> note that we **did not** request `x_2` explicitly, yet its binding
## function was executed by `x_1` as we've registered `x_2` to be an object
## that changes can/should be actively pushed to.

x_2
## --> already the cached value

## Clean up //
removeReactive("x_1")
removeReactive("x_2")

##------------------------------------------------------------------------------
## Using reactive bindings in more complex data structure //
##------------------------------------------------------------------------------

## This resembles what is already possibly via the use of Reference Classes
## or R6 Classes (see argument `active` in `R6Class()`)
x_1 <- new.env()  
x_2 <- new.env()  

setReactiveS3(id = "field_1", value = 1:5, where = x_1, typed = TRUE)
setReactiveS3(id = "field_2", value = function() { 
  "object-ref: {id: field_1, where: x_1}"
  field_1 * 2
}, where = x_1, typed = TRUE)

setReactiveS3(id = "field_1", value = function() { 
  "object-ref: {id: field_1, where: x_1}"
  "object-ref: {id: field_2, where: x_1}"
  data.frame(field_1, field_2)
}, where = x_2, typed = TRUE)

setReactiveS3(id = "x_3", value = function() { 
  "object-ref: {id: field_1, where: x_1, as: x_1_f_1}"
  "object-ref: {id: field_2, where: x_1, as: x_1_f_2}"
  "object-ref: {id: field_1, where: x_2, as: x_2_f_1}"
  list(
    x_1_f_1 = summary(x_1_f_1), 
    x_1_f_2 = summary(x_1_f_2), 
    x_2_f_1 = x_2_f_1[,1] * x_2_f_1[,2],
    files = paste0("file_", x_2_f_1[,1])
  )
}, x_1 = x_1, x_2 = x_2)

## Inspect //
x_1$field_1
x_1$field_2
x_2$field_1
x_3

## Change values //
(x_1$field_1 <- 1:10)
x_1$field_2
x_2$field_1
x_3

(x_1$field_1 <- 1)
x_1$field_2
x_2$field_1
x_3


## Clean up //
removeReactive("x_1")
removeReactive("x_2")
removeReactive("x_3")

##------------------------------------------------------------------------------
## Profiling //
##------------------------------------------------------------------------------

require("microbenchmark")

resetRegistry()
object.size(getRegistry())
rm(list = ls(environment(), all.names = TRUE))
memory.size(max = FALSE)
where <- environment()
res <- microbenchmark(
  "set/x_1/setReactiveS3" = setReactiveS3(id = "x_1", value = 10, where = where),
  "set/x_2/assign" = assign("x_2", value = 10, envir = where),
  "get x_1" = get("x_1"),
  "get x_2" = get("x_2"),
  "set/x_3/setReactiveS3" = setReactiveS3(
    id = "x_3", 
    value = function() {
      ## object-ref: {id: x_1}
      x_1 * 2
    }
  ),
  "get x_3" = get("x_3"),
  "change x_1" = assign("x_1", 100),
  "change x_2" = assign("x_2", 100),
  "get x_3 (2)" = get("x_3")
)
memory.size(max = FALSE)

## Object sizes //
object.size(getRegistry())
object.size(x_1)
object.size(x_2)
object.size(x_3)

res
# Unit: microseconds
#                   expr      min        lq       mean    median        uq      max neval
#  set/x_1/setReactiveS3 1069.604 1220.6270 1347.94280 1281.6285 1411.9235 2926.306   100
#         set/x_2/assign    1.184    2.3690    2.81935    2.9610    3.5540    6.515   100
#                get x_1   55.672   66.3320   80.83052   71.9590   91.2070  179.452   100
#                get x_2    1.184    2.3695   18.85741    2.9620    4.1460 1528.004   100
#  set/x_3/setReactiveS3 5121.775 5922.7935 6320.13374 6164.4315 6563.6075 9162.103   100
#                get x_3  175.898  201.0690  301.58565  228.3125  291.9795  784.138   100
#             change x_1  194.258  213.2100  264.50488  235.4195  275.6930 1605.589   100
#            get x_3 (2)  175.306  206.6950  333.85734  241.3425  366.6030  990.834   100

rm(list = ls(environment(), all.names = TRUE))
memory.size(max = FALSE)
where <- environment()
res <- microbenchmark(
  "set/x_1/setReactiveS3" = setReactiveS3(id = "x_1", value = 10, where = where,
                                          cache = FALSE),
  "set/x_2/assign" = assign("x_2", value = 10, envir = where),
  "get x_1" = get("x_1"),
  "get x_2" = get("x_2"),
  "set/x_3/setReactiveS3" = setReactiveS3(
    id = "x_3", 
    value = function() {
      ## object-ref: {id: x_1}
      x_1 * 2
    },
    cache = FALSE,
    where = where
  ),
  "get x_3" = get("x_3"),
  "change x_1" = assign("x_1", 100),
  "change x_2" = assign("x_2", 100),
  "get x_3 (2)" = get("x_3")
)
memory.size(max = FALSE)
res

# Unit: microseconds
#                   expr      min        lq       mean    median        uq      max neval
#  set/x_1/setReactiveS3 1097.439 1146.0040 1257.89191 1199.8990 1309.7610 2812.004   100
#         set/x_2/assign    1.184    2.3690    2.97928    2.9610    3.5540   17.176   100
#                get x_1   15.991   19.5445   22.79001   21.3215   24.5785   42.050   100
#                get x_2    1.184    2.3690    3.06213    2.9610    2.9620   18.952   100
#  set/x_3/setReactiveS3 4514.130 4701.8735 5092.67555 5012.2125 5332.9160 7096.932   100
#                get x_3  309.747  334.3250  364.77293  349.7240  383.1855  512.296   100
#             change x_1  194.850  212.9140  247.34757  220.9095  245.1915 1711.603   100
#            get x_3 (2)  310.931  331.6610  373.98843  347.3550  370.1570 1894.608   100


#   "set x_3" = setShinyReactive(id = "x_3", value = 10, where = where),
#   "get x_3" = get("x_3"),
#   "set x_4" = setShinyReactive(id = "x_4", value = reactive(x_3 * 2), where = where),
#   "get x_4" = get("x_4"),
#   "set x_5" = assign("x_3", 10),
#   "get x_5" = get("x_3"),
#   control = list(order = "inorder")
# )



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

## Removing `env_1`
rm(env_1)
env_2$env_1
## --> still there

## Reassigning `env_1`
env_1 <- new.env()
identical(env_2$env_1, env_1)
## --> not identical anymore

## This is the reason why method `ensureIntegrity()` of class `ReactiveObject.S3`
## exists and why in certain situations the re-sync of registry references 
## must/should be ensured

##------------------------------------------------------------------------------
## On a sidenote: caching mechanism
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
## `digest::digest()`. These are stored in option environment
##                       `getOption(".reactr")$.registry`
## which is accessible via the convenience function 
##                          `getRegistry()`
## Besides the actual checksum values, each entry - corresponding to a reactive 
## object - also contains some additional information:
## - id:    object ID as specified in call to `setReactiveS3()`
## - uid:   object UID computed as follows:
##          `digest::digest(list(id = id, where = {where}))`
##          where `{where}` stands for the location provided via argument 
##          `where` in the call to `setReactiveS3()`
## - {uid}: subenvironment corresponding to the object`s UID. This contains
##          the object`s own checksum 
## - {ref-uid} subenvironments for each referenced object should there exist
##             any. These in turn contain the referenced object`s checksum that is
##             used to determine if an update is necessary or not.

## Hash registry //
registry <- getRegistry()
ls(registry)

setReactiveS3(id = "x_1", value = 10)
ls(registry)
uid_x_1 <- computeObjectUid(id = "x_1", where = environment())
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

uid_x_2 <- computeObjectUid(id = "x_2", where = environment())
registry_x_2 <- registry[[uid_x_2]]
ls(registry_x_2)
registry_x_2$id
registry_x_2$uid
registry_x_2$where
registry_x_2[[uid_x_2]]
## --> contains own registry value
registry_x_2[[uid_x_1]]
## --> contains registry value of `x_1` in `.GlobalEnv`

## Clean up //
removeReactive(x_1)
removeReactive(x_2)
resetRegistry()

}
