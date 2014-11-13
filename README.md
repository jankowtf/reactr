reactr
======

Reactive object bindings with built-in caching and push functionality

## Installation 

```
require("devtools")
devtools::install_github("Rappster/conditionr")
devtools::install_github("Rappster/yamlr")
devtools::install_github("Rappster/reactr")
require("reactr")
```
## Purpose 

The package aims at contributing to *Reactive Programming* or *Reactivity* in R. 

It allows the specification of **reactive objects** based on reactive expressions that dynamically bind them to other objects. 
    
That way, an object `x` can be dynamically observed by `n` other objects. Whenever `x` changes, the `n` variables observing `x` change according to their reactive expressions that defines the actual binding relationship. This is an approach to ensure consistent states of objects and the entire system. You can choose how changes should be propagated throughout the system: following a *pull* or a *push*  principle (see section **Highlighting selected features**).

## Implementations

Two different reactivity implementations are provided: 

1. Implementation that builds on top of [`shiny`](https://github.com/rstudio/shiny) (recommended for **regular reactive scenarios**)

  I tried to build as much on top of existing functionality in order to make this package as compatible as possible with shiny apps. However, I was not able to implement all of my "wish-list features" yet that way - so any help is greatly appreciated.
  
  Branch `legacy-shinyOld` contains a legacy version on the road to the current implementation that might or might not be of interest to other developers.

2. Custom implementation (legacy but recommended for **bi-directional reactive scenarios**)

  This implementation is older and is much more "pedestrian" as it stems from the time where I did not understand very well yet how reactivity is implemented in [`shiny`](https://github.com/rstudio/shiny). However, while it is quite "custom-made" in comparision, it does work and on top reveals some of the interesting details that shiny solves in a similar, yet much more elegantly way. Thus I decided to keep it as a reference for myself and other programmers for the time being.

### Aknowledgements

The package is greatly inspired by reactivity as implemented by the [shiny](http://shiny.rstudio.com) framework. 

### Vignettes (not refactored yet!)

- [Bi-Directional Bindings](https://github.com/Rappster/reactr/blob/master/vignettes/bidirectional_bindings.pdf)
- [Caching](https://github.com/Rappster/reactr/blob/master/vignettes/caching.pdf)
- [Convenience Functions](https://github.com/Rappster/reactr/blob/master/vignettes/convenience_functions.pdf)
- [Pushing](https://github.com/Rappster/reactr/blob/master/vignettes/pushing.Rmd)
- [Reactive References](https://github.com/Rappster/reactr/blob/master/vignettes/reactive_references.pdf)
- [Relations to Shiny](https://github.com/Rappster/reactr/blob/master/vignettes/relations_to_shiny.Rmd)
- [Registry](https://github.com/Rappster/reactr/blob/master/vignettes/registry.pdf)
- [Strictness](https://github.com/Rappster/reactr/blob/master/vignettes/strictness.Rmd)

----------

### Basics

```
setShinyReactive(id = "x_1", value = 10)
setShinyReactive(id = "x_2", value = reactiveExpression(x_1 * 2))

x_1
x_2
## --> x_1 * 2 = 20

(x_1 <- 20)
x_2
## --> x_1 * 2= 40

setShinyReactive(id = "root_dir", value = getwd())
setShinyReactive(id = "sub_dir", value = reactiveExpression(
  file.path(root_dir, "doc")
))

root_dir
sub_dir
## --> "Q:/home/wsp/rapp2/reactr/doc"

root_dir <- tempdir()
sub_dir
## --> "C:\\Users\\jat\\AppData\\Local\\Temp\\RtmpyUOCMk/doc"
```

Clean up 

```
rmReactive("x_1")
rmReactive("x_2")
```

### Highlighting selected features

1. Strictness levels can be defined for 

  - the *creation process* itself in `setReactive()` and`setShinyReactive()`: see argument `strict`
  - *getting* the visible value of a reactive object: see argument `strict_get`
  - *setting* the visible value of a reactive object: see argument `strict_set`
  
  See vignette [Strictness](https://github.com/Rappster/reactr/blob/master/vignettes/strictness.Rmd) for details.
    
2. **Propagation of changes**: you can choose between a **pull** and a **push** paradigm with respect to how changes are propagated throughout the system. 

  When using a *pull* paradigm (the default), objects referencing other objects that have changed are not informed of these change until they are explicitly requested (by `get()` or its syntactical sugars).
  
  When using a *push* paradigm, an object that changed informs all objects that have a reference to it about the change by implicitly calling the `$getVisible()` method of all of their registered push references. 
  
  See vignette [Pushing](https://github.com/Rappster/reactr/blob/master/vignettes/pushing.Rmd) for details on this.

3. **Relations to shiny**: as already mentioned, the package has a lot of relations to the [shiny framework](http://shiny.rstudio.com) and thus the actual [shiny](http://cran.r-project.org/web/packages/shiny/index.html) package

  Summary of the added functionality compared to what is currently offered by existing shiny functionality (shiny's limitations should always be read "AFAIK" ;-)):
  
  1. The same function can be used for setting both reative sources and observers.
  
  2. Reactive expressions/binding functions are **hidden** from the user.
  
    To the user, all reactive objects appear and behave as if they are actual *non-reactive*/*non-functional* values. This eliminates the need to distinguish (mentally and by code) if a certain value is a *non-functional* value or a *function* that needs to be executed via `()`. 
    
    The latter is what is necessary when using current shiny functionality based on `shiny::makeReactiveBinding()` and `shiny::reactive()`).
    
    However, you can mimick the default shiny behavior by setting `lazy = TRUE`.
  
  3. Push updates
  
    While shiny implements reactivity following a **pull paradigm** with respect to the way that changes are propagated throughout the system (resembles *lazy evaluation*), `reactr` also offers the alternative use of a **push paradigm** where changes are *actively* propagated via the argument `push = TRUE`.
    
  See vignette [Relations to Shiny](https://github.com/Rappster/reactr/blob/master/vignettes/relations_to_shiny.Rmd) for more details.

5. **Reference specification** (only relevant for `setReactive()`):

  The preferred way to specify the reference is via [YAML](http://www.yaml.org/) markup as in the example above. However, there also exist two other ways to specify references.: 

  1. Via a function argument `refs`.
  2. Via explicit `get()` calls in the body of form 
    
    ```
    .ref_{number} <- get({id}, {where})
    ```
  
    with `{number}` being an arbitrary number or other symbol, `{id}` being the referenced object's name/ID and `{where}` being the environment where the value belonging to `{id}` was assigned to (e.g. `.ref_1 <- get{"x_1", where_1}`).

  See vignette [Reactive References](https://github.com/Rappster/reactr/blob/master/vignettes/reactive_references.Rmd) for details.

6. **Caching mechanism** (only relevant for `setReactive()`): 

  Binding functions are only executed if they need to be, i.e. only if one of the referenced objects has actually changed. 

  Otherwise a cached value that has been stored from the last update run is returned.

  While this may cost more than it actually helps in scenarios where the binding functions are quite simple and thus don't take long to run, caching *may* reduce runtimes/computation times in case of either more complex and long-running binding functions or when greater amounts of data comes into play (needs to be tested yet). 
  
  See vignette [Caching](https://github.com/Rappster/reactr/blob/master/vignettes/caching.Rmd) for details.

-----

### Feature showcase 1: pushing

Such a construct could be used for logging or ensuring that certain database operations are triggered right away after the system state has changed:

```
setShinyReactive(id = "x_1", value = 10)
setShinyReactive(
  id = "x_2", 
  value = reactiveExpression({
    message(paste0("[", Sys.time(), "] I'm x_2 and the value of x_1 is: ", x_1))
    x_1 * 2
  }), 
  push = TRUE
)
## --> [2014-11-13 17:35:11] I'm x_2 and the value of x_1 is: 10

x_1
## --> 10

x_2
## --> 20
```

Note that we never request the value of `x_2` explicitly yet changes in `x_1` are actively pushed to `x_2` thus executing its reactive binding function:

```
(x_1 <- 11)
## --> [2014-11-13 17:35:47] I'm x_2 and the value of x_1 is: 11
## --> 11

(x_1 <- 12)
## --> [2014-11-13 17:36:14] I'm x_2 and the value of x_1 is: 12
## --> 12

(x_1 <- 13)
## --> [2014-11-13 17:36:32] I'm x_2 and the value of x_1 is: 13
## --> 13

x_2
## --> 26
```

Clean up 

```
rmReactive("x_1")
rmReactive("x_2")
```
### Feature showcase 2: getting closer to an actual use case

Specify reactive objects:

```
setShinyReactive(id = "x_1", value = 1:5, typed = TRUE)
setShinyReactive(id = "x_2", value = reactiveExpression(x_1 * 2), typed = TRUE)

setShinyReactive(id = "x_3", value = reactiveExpression(
  data.frame(x_1 = x_1, x_2 = x_2)), typed = TRUE)

setShinyReactive(id = "x_4", value = reactiveExpression(
  list(
    x_1 = summary(x_1), 
    x_2 = summary(x_2), 
    x_3_new = data.frame(x_3, prod = x_3$x_1 * x_3$x_2),
    filenames = paste0("file_", x_1)
  )
))
```

Inspect:

```
x_1
# [1] 1 2 3 4 5

x_2
# [1]  2  4  6  8 10

x_3
#   x_1 x_2
# 1   1   2
# 2   2   4
# 3   3   6
# 4   4   8
# 5   5  10

x_4
# $x_1
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#       1       2       3       3       4       5 
# 
# $x_2
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#       2       4       6       6       8      10 
# 
# $x_3_new
#   x_1 x_2 prod
# 1   1   2    2
# 2   2   4    8
# 3   3   6   18
# 4   4   8   32
# 5   5  10   50
# 
# $filenames
# [1] "file_1" "file_2" "file_3" "file_4" "file_5"
```

Change values and inspect implications:

```
(x_1 <- 100:102)
# [1] 100 101 102

x_2
# [1] 200 202 204

x_3
#   x_1 x_2
# 1 100 200
# 2 101 202
# 3 102 204

x_4
# $x_1
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   100.0   100.5   101.0   101.0   101.5   102.0 
# 
# $x_2
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#     200     201     202     202     203     204 
# 
# $x_3_new
#   x_1 x_2  prod
# 1 100 200 20000
# 2 101 202 20402
# 3 102 204 20808
# 
# $filenames
# [1] "file_100" "file_101" "file_102"

(x_1 <- 1)
# [1] 1

x_2
# [1] 2

x_3
#   x_1 x_2
# 1   1   2

x_4
# $x_1
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#       1       1       1       1       1       1 
# 
# $x_2
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#       2       2       2       2       2       2 
# 
# $x_3_new
#   x_1 x_2 prod
# 1   1   2    2
# 
# $filenames
# [1] "file_1"

try((x_1 <- "hello world!"))
x_1
## --> still `1:2` --> overwrite has been ignored (due to `typed = TRUE`)
```

Clean up:

```
rmReactive("x_1")
rmReactive("x_2")
rmReactive("x_3")
rmReactive("x_4")
```

### Feature showcase 3: setReactive() (legacy)

Note that we set `verbose = TRUE` to enable the display of status messages that help understand what's going on.

Set reactive object `x_1` that others can reference:

```
setReactive(id = "x_1", value = 10, verbose = TRUE)
```

Set reactive object that references `x_1` and has a reactive binding of form `x_1 * 2` to it:

```
setReactive(id = "x_2", value = function() {
  "object-ref: {id: x_1}"
  x_1 * 2
}, verbose = TRUE)
# Initializing ...

x_1 
# [1] 10

x_2
# [1] 20
```

Whenever `x_1` changes, `x_2` changes accordingly:


```
(x_1 <- 100)
# [1] 100

x_2
# Object: ab22808532ff42c87198461640612405
# Called by: ab22808532ff42c87198461640612405
# Modified reference: 2fc2e352f72008b90a112f096cd2d029
#   - Checksum last: 2522027d230e3dfe02d8b6eba1fd73e1
# 	- Checksum current: d344558826c683dbadec305ed64365f1
# Updating ...
# [1] 200
```

See the examples of `setReactive()` for a short description of the information contained in the status messages

Note that for subsequent requests and as long as `x_1` does not change, the value that has been cached during the last update cycle is used instead of re-running the binding function each time:

```
x_2
# [1] 200
## --> cached value, no update

x_2
# [1] 200
## --> cached value, no update

(x_1 <- 1)
x_2
# Object: ab22808532ff42c87198461640612405
# Called by: ab22808532ff42c87198461640612405
# Modified reference: 2fc2e352f72008b90a112f096cd2d029
#   - Checksum last: d344558826c683dbadec305ed64365f1
# 	- Checksum current: 6717f2823d3202449301145073ab8719
# Updating ...
# [1] 2
## --> update according to binding function

x_2
# [1] 2
## --> cached value, no update
```

Clean up 

```
rmReactive("x_1")
rmReactive("x_2")
```

-----

## Unsetting reactive objects

This turns reactive objects (that are, even though hidden from the user, instances of class `ReactiveObject.S3`) into regular or non-reactive objects again. 

**Note that it does not mean the a reactive object is removed alltogether! See `rmReactive()` for that purpose**

```
setReactive(id = "x_1", value = 10)
setReactive(id = "x_2", value = function() "object-ref: {id: x_1}")

## Illustrate reactiveness //
x_1
x_2
(x_1 <- 50)
x_2

## Unset reactive --> turn it into a regular object again //
unsetReactive(id = "x_1")
```
Illustration of removed reactiveness: 

```
x_1
x_2
(x_1 <- 10)
x_2
## --> `x_1` is not a reactive object anymore; from now on, `x_2` simply returns
## the last value that has been cached
```

### NOTE
What happens when a reactive relationship is broken or removed depends on how you set argument `strictness_get` in the call to `setReactive()` or `setShinyReactive()`. 

Also refer to vignette [Strictness](https://github.com/Rappster/reactr/blob/master/vignettes/strictness.Rmd) for more details.

## Removing reactive objects

This deletes the object alltogether. 

```
setReactive(id = "x_1", value = 10)
setReactive(id = "x_2", value = function() "object-ref: {id: x_1}")

## Remove reactive --> remove it from `where` //
rmReactive(id = "x_1")

exists("x_1", inherits = FALSE)
```

-----

# Reactivity scenarios in detail

The examples currently still use `setReactive()` instead of the recommended `setShinyReactive()` but should also work for `setShinyReactive()` given that you specify `value` via `reactiveExpression()` and that you do not want to use bi-directional bindings (as this is currently only possible when using `setReactive()`)

## Scenario 1: one-directional (1)

### Scenario explanation

- Type/Direction: 

  `A` references `B` 
  
- Binding/Relationship: 

  `A` uses value of `B` "as is", i.e. value of `A` identical to value of `B`

### Example

Set object `x_1` that others can reference:

```
setReactive(id = "x_1", value = 10)
```

Set object that references `x_1` and has a reactive binding to it:

```
setReactive(id = "x_2", value = function() "object-ref: {id: x_1}")

x_1 
x_2

```

Whenever `x_1` changes, `x_2` changes accordingly:


```
(x_1 <- 100)
# [1] 100

x_2
# [1] 100

x_2
# [1] 100
## --> cached value as `x_1` has not changed; no update until `x_1` 
## changes again

## Clean up //
rmReactive("x_1")
rmReactive("x_2")
```
-----

## Scenario 2: one-directional (2)

### Scenario explanation

- Type/Direction: 

  `A` references `B` 
  
- Binding/Relationship: 

  `A` transforms value of `B` , i.e. value of `A` is the result of applying a function on the value of `B`

### Example

```
setReactive(id = "x_1", value = 10)
setReactive(id = "x_2", value = function() "object-ref: {id: x_1}")
setReactive(id = "x_3", value = function() {
  "object-ref: {id: x_1, as: ref_1}"
  ref_1 * 2
})
```

Note how `x_3` changes according to its binding relationship `ref_1 * 2` (which is just a translation for `x_1 * 2`):

```
x_1 
# [1] 10

x_2
# [1] 10

x_3
# [1] 20
## --> x_1 * 2

(x_1 <- 500)
x_2
# [1] 500

x_3
# [1] 1000

## Clean up //
rmReactive("x_1")
rmReactive("x_2")
rmReactive("x_3")
```

-----

## Scenario 3: one-directional (3)

### Scenario explanation

- Type/Direction: 

  `A` references `B` and `C`, `B` references `C`
  
- Binding/Relationship: 

  `A` transforms value of `B` , i.e. value of `A` is the result of applying a function on the value of `B`

### Example

```
setReactive(id = "x_1", value = 10)
setReactive(id = "x_2", value = function() "object-ref: {id: x_1}")
setReactive(id = "x_3", value = function() {
  "object-ref: {id: x_1, as: ref_1}"
  "object-ref: {id: x_2, as: ref_2}"
  ref_1 + ref_2 * 2
})
```

Note how each object that is involved changes according to its binding relationships:

```
x_3
# [1] 30

(x_1 <- 100)

x_3
[1] 300

(x_2 <- 1)
x_2
## --> disregarded as `x_2` has a one-directional binding to `x_1`, hence does 
## not accept explicit assignment values

x_3
# [1] 300

(x_1 <- 50)
x_2
# [1] 50

x_3
# [1] 150

## Clean up //
rmReactive("x_1")
rmReactive("x_2")
rmReactive("x_3")
```

## Scenario 4: bi-directional (1)

### Scenario explanation

- Type/Direction: 

  `A` references `B` and `B` references `A` --> bidirectional binding type
  
- Binding/Relationship: 

  `A` uses value of `B` "as is" and `B` uses value of `A` "as is". This results in a **steady state**. 

### Example

A cool feature of this binding type is that you are free to alter the values of *both* objects and still keep everything "in sync"

```
setReactive(id = "x_1", function() "object-ref: {id: x_2}")
setReactive(id = "x_2", function() "object-ref: {id: x_1}")
```

Note that the call to `setReactive()` merely initializes objects with bidirectional bindings to the value `numeric(0)`:

```
x_1
# NULL

x_2
# NULL
```

You must actually assign a value to either one of them via `<-` **after** establishing the binding:

```
## Set actual initial value to either one of the objects //
(x_1 <- 100)
# [1] 100

x_2
# [1] 100

x_1
# [1] 100

## Changing the other one of the two objects //
(x_2 <- 1000)
# [1] 1000

x_1
# [1] 1000

## Clean up //
rmReactive("x_1")
rmReactive("x_2")
```

## Scenario 5: bi-directional (2)

### Scenario explanation

- Type/Direction: 

  `A` references `B` and `B` references `A` --> bidirectional binding type
  
- Binding/Relationship: 

  `A` uses transformed value of `B` and `B` uses transformed value of `A`. 
  
  The binding functions used result in a **steady state**.

### Example

As the binding functions are "inversions"" of each other, we still get to a steady state.

```
setReactive(id = "x_1", function() {
  "object-ref: {id: x_2}"
  x_2 * 2
})

setReactive(id = "x_2", function() {
  "object-ref: {id: x_1}"
  x_1 / 2
})
```

Note that due to the structure of the binding functions, the visible object values are initialized to `numeric()` instead of `NULL` now.

```
x_1
# numeric(0)

x_2
# numeric(0)
```

Here, we always reach a steady state, i.e. a state in which cached values can be used instead of the need to executed the binding functions.

```
## Set actual initial value to either one of the objects //
(x_1 <- 100)
# [1] 100

x_2
# [1] 50

x_1
# [1] 100

## Changing the other one of the two objects //
(x_2 <- 1000)
# [1] 1000

x_1
# [1] 2000

x_2
# [1] 1000

## Clean up //
rmReactive("x_1")
rmReactive("x_2")
```

## Scenario 6: bi-directional (3)

### Scenario explanation

- Type/Direction: 

  `A` references `B` and `B` references `A` --> bidirectional binding type
  
- Binding/Relationship: 

  `A` uses transformed value of `B` and `B` uses transformed value of `A`. 
  
  The binding functions used result in a **non-steady state**.

### Example

As the binding functions are **not** "inversions"" of each other, we never reach/stay at a steady state. Cached values are/can never be used as by the definition of the binding functions the two objects are constantly updating each other.

```
setReactive(id = "x_1", function() {
  "object-ref: {id: x_2}"
  x_2 * 2
})

setReactive(id = "x_2", function() {
  "object-ref: {id: x_1}"
  x_1 * 10
})
```

Here, we have "non-steady-state" behavior, i.e. we never reach a state were cached values can be used. We always need to execute the binding functions as each request of a visible object value results in changes. 

This is best verified when using `verbose = TRUE` and comparing it to the other scenarios (not done at this point).

```
x_1
# numeric(0)

x_2
# numeric(0)

## Set actual initial value to either one of the objects //
(x_1 <- 1)
# [1] 1

x_2
# [1] 10
## --> `x_1` * 10

x_1
# [1] 20
## --> x_2 * 2

x_2
# [1] 200
## --> `x_1` * 10

## Changing the other one of the two objects //
(x_2 <- 1)
# [1] 1

x_1
# [1] 2

x_2
# [1] 20

x_1
# [1] 40

## Clean up //
rmReactive("x_1")
rmReactive("x_2")
```

----

## Caching mechanism (overview)

### NOTE

This is only necessary/usefull when using `setReactive()` as `setShinyReactive()` as shiny takes care of registering and caching itself.

The package implements a caching mechanism that (hopefully) contributes to an efficient implementation of reactivity in R in the respect that binding functions are only executed when they actually need to.

As mentioned above, this *might* be unnecessary or even counter-productive in situations where the runtime of binding functions is negligible, but help in situations where unnecessary executions of binding functions is not desired due to their specific nature or long runtimes.

A second reason why the caching mechanism was implemented is to offer the possibility to specify *bi-directional* reactive bindings. AFAICT, you need some sort of caching mechanism in order to avoid infinite recursions.

See vignette [Caching](https://github.com/Rappster/reactr/blob/master/vignettes/caching.Rmd) for details on this.

### The registry

Caching is implemented by storing references of the "hidden parts" of an reactive object (the hidden instances of class `ReactiveObject.S3`) in a registry that is an `environment` and lives in `getOption("reactr")$.registry`.

### Convenience functions

Ensuring example content in registry:

```
resetRegistry()
setReactive(id = "x_1", value = 10)
setReactive(id = "x_2", value = function() "object-ref: {id: x_1}")
```

#### Get the registry object

```
registry <- getRegistry()
```

#### Show registry content

```
showRegistry()
```

The registry contains the UIDs of the reactive objects that have been set via `setReactive`. See `computeObjectUid()` for the details of the computation of object UIDs.

#### Retrieve from registry

```
x_1_hidden <- getFromRegistry(id = "x_1")
x_2_hidden <- getFromRegistry(id = "x_2")

## Via UID //
getFromRegistry(computeObjectUid("x_1"))
getFromRegistry(computeObjectUid("x_2"))

```

This object corresponds to the otherwise "hidden part"" of `x_1` that was implicitly created by the call to `setReactive()`.

```
class(x_1_hidden)
ls(x_1_hidden)

## Some interesting fields //
x_1_hidden$.id
x_1_hidden$.where
x_1_hidden$.uid
x_1_hidden$.value
x_1_hidden$.hasPullReferences()

x_2_hidden$.id
x_2_hidden$.where
x_2_hidden$.uid
x_2_hidden$.value
x_2_hidden$.has_cached
x_2_hidden$.hasPullReferences()
ls(x_2_hidden$.refs_pull)
x_2_hidden$.refs_pull[[x_1_hidden$.uid]]
```
#### Remove from registry

```
## Via ID (and `where`) //
rmFromRegistry(id = "x_1")
## --> notice that entry `2fc2e352f72008b90a112f096cd2d029` has been removed

## Via UID //
rmFromRegistry(computeObjectUid("x_2"))
## --> notice that entry `ab22808532ff42c87198461640612405` has been removed
```

#### Reset registry

```
showRegistry()
resetRegistry()
showRegistry()
```
