reactr
======

Reactive object bindings with built-in caching and push functionality

## Installation 

```
require("devtools")
devtools::install_github("Rappster/yamlr")
devtools::install_github("Rappster/reactr")
require("reactr")
```
## Overview 

The package aims at contributing to *Reactive Programming* or *Reactivity* in R. It allows to specify *reactive objects**, i.e. objects that are linked a way that if one object changes, all objects referencing that object are updated as well. 

The implementation is greatly inspired by and is to a large extend very similar to that implemented by the [shiny](http://shiny.rstudio.com) framework. It is a declared goal of this package to re-use as much of the existing functionality provided by shiny and to make its reactive objects as compatible as possible to those used by shiny.

### Quick Example 1: setReactiveS3()

Note that we set `verbose = TRUE` to enable the display of status messages that help understand what's going on.

Set reactive object `x_1` that others can reference:

```
setReactiveS3(id = "x_1", value = 10, verbose = TRUE)
```

Set reactive object that references `x_1` and has a reactive binding of form `x_1 * 2` to it:

```
setReactiveS3(id = "x_2", value = function() {
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

See examples of `setReactiveS3()` for a short description of the information contained in the status messages

```
x_2
# [1] 200
```

Note that for subsequent requests and as long as `x_1` does not change, the value that has been cached during the last update cycle is used instead of re-running the binding function each time.

Clean up 

```
removeReactive("x_1")
removeReactive("x_2")
```

### Quick Example 2: setShinyReactive()

```
setShinyReactive(id = "x_1", value = 10)
setShinyReactive(id = "x_2", value = function() {
  "object-ref: {id: x_1}"
  x_1 * 2
})
```

The main difference to using `setReactiveS3()` consists in the classes and instances used: instead of class `ReactiveObject.S3` class `ReactiveShinyObject` (and the classes that this class inherits from) is used:

```
reg_x_1 <- getFromRegistry("x_1")
reg_x_1
class(reg_x_1)

reg_x_2 <- getFromRegistry("x_2")
reg_x_2
class(reg_x_2)
```

Do the same for reactive objects set via `setReactiveS3()` and compare the objects/classes.

Clean up 

```
removeReactive("x_1")
removeReactive("x_2")
```

### Quick Example 3: Closer to an actual use case

```
setReactiveS3(id = "x_1", value = 1:5, typed = TRUE)
setReactiveS3(id = "x_2", value = function() { 
  "object-ref: {id: x_1}"
  x_1 * 2
}, typed = TRUE)

setReactiveS3(id = "x_3", value = function() { 
  "object-ref: {id: x_1}"
  "object-ref: {id: x_2}"
  data.frame(x_1 = x_1, x_2 = x_2)
}, typed = TRUE)

setReactiveS3(id = "x_4", value = function() { 
  "object-ref: {id: x_1}"
  "object-ref: {id: x_2}"
  "object-ref: {id: x_3}"
  list(
    x_1 = summary(x_1), 
    x_2 = summary(x_2), 
    x_3_new = data.frame(x_3, prod = x_3$x_1 * x_3$x_2),
    filenames = paste0("file_", x_1)
  )
})

## Inspect //
x_1
x_2
x_3
x_4

## Change values //
(x_1 <- 1:10)
x_2
x_3
x_4

(x_1 <- 1)
x_2
x_3
x_4

try((x_1 <- "hello world!"))

## Clean up //
removeReactive("x_1")
removeReactive("x_2")
removeReactive("x_3")
removeReactive("x_4")
```

### Highlighting selected features

1. The preferred way to specify the reference is via [YAML](http://www.yaml.org/) markup as in the example above. However, there also exist two other ways to specify references.: 

  1. Via a function argument `refs`.
  2. Via explicit `get()` calls in the body of form 
    
  ```
  .ref_{number} <- get({id}, {where})
  ```
  
  with `{number}` being an arbitrary number or other symbol, `{id}` being the referenced object's name/ID and `{where}` being the environment where the value belonging to `{id}` was assigned to.

  See vignette [Specifying Reactive References](https://github.com/Rappster/reactr/blob/master/vignettes/specifying_reactive_references.Rmd) for details.

2. The environment in which to set a reactive object can be chosen via argument `where`

3. Strictness levels can be defined for 

  - the creation process itself in `setReactiveS3()` and`setShinyReactive()`: see argument `strict`
  - *getting* the visible value of a reactive object: see argument `strict_get`
  - *setting* the visible value of a reactive object: see argument `strict_set`
  
  See vignette [Strictness](https://github.com/Rappster/reactr/blob/master/vignettes/strictness.Rmd) for details.
  
4. **Caching mechanism**: binding functions are only executed if they need to be, i.e. only if one of the referenced objects has actually changed. 

  Otherwise a cached value that has been stored from the last update run is returned.

  While this may cost more than it actually helps in scenarios where the binding functions are quite simple and thus don't take long to run, caching *may* reduce runtimes/computation times in case of either more complex and long-running binding functions or when greater amounts of data comes into play (needs to be tested yet). 
  
  See vignette [Caching](https://github.com/Rappster/reactr/blob/master/vignettes/caching.Rmd) for details.
  
5. **Propagating changes**: you can choose between a **pull** and a **push** paradigm with respect to how changes are propagated throughout the system. 

  When using a *pull* paradigm (the default), objects referencing other objects that have changed are not informed of these change until they are explicitly requested (by `get()` or its syntactical sugars).
  
  When using a *push* paradigm, an object that changed informs all objects that have a reference to it about the change by implicitly calling the `$getVisible()` method of all of their registered push references. 
  
  See vignette [Pushing](https://github.com/Rappster/reactr/blob/master/vignettes/pushing.Rmd) for details on this.

5. As already mentioned, the package has a lot of relations to the [shiny](http://shiny.rstudio.com) and thus the actual [shiny](http://cran.r-project.org/web/packages/shiny/index.html) package

  Added functionality with respect to existing shiny functionality (limitations of shiny should always be considered "AFAIK" ;-)):
  
  1. Binding functions are **hidden** from the user.
  
    To the user, all reactive objects behave as if they are actual *non-function* values. This eliminates the need to keep keep track if a certain value is a *non-function* value or a *function* that needs to be executed via `()` (that's what is necessary when using current shiny functionality)
    
  2. Caching
  
    While shiny implements reactivity in an *immediate* manner (i.e. binding functions are **always** executed), `reactr` implements a mechanism that keeps track if an update is actually needed or if it is valid to return a cached value instead. 
    
  3. Bi-directional bindings
  
  Due to the aspect mentioned in 2., it is not possible to define bi-directional bindings with current shiny functionality. Due to the caching mechanism, `reactr` allows to specify such bindings.
  
  4. Push updates
  
  While shiny implements reactivity following a **pull paradigm** with respect to the way that changes are propagated throughout the system (resembles *lazy evaluation*), `reactr` also offers the use of a **push paradigm** where changes are *actively* propagated.

-----

# Reactivity scenarios

## Scenario 1: one-directional (1)

### Scenario explanation

- Type/Direction: 

  `A` references `B` 
  
- Binding/Relationship: 

  `A` uses value of `B` "as is", i.e. value of `A` identical to value of `B`

### Example

Set object `x_1` that others can reference:

```
setReactiveS3(id = "x_1", value = 10)
```

Set object that references `x_1` and has a reactive binding to it:

```
setReactiveS3(id = "x_2", value = function() "object-ref: {id: x_1}")

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
removeReactive("x_1")
removeReactive("x_2")
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
setReactiveS3(id = "x_1", value = 10)
setReactiveS3(id = "x_2", value = function() "object-ref: {id: x_1}")
setReactiveS3(id = "x_3", value = function() {
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
removeReactive("x_1")
removeReactive("x_2")
removeReactive("x_3")
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
setReactiveS3(id = "x_1", value = 10)
setReactiveS3(id = "x_2", value = function() "object-ref: {id: x_1}")
setReactiveS3(id = "x_3", value = function() {
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
removeReactive("x_1")
removeReactive("x_2")
removeReactive("x_3")
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
setReactiveS3(id = "x_1", function() "object-ref: {id: x_2}")
setReactiveS3(id = "x_2", function() "object-ref: {id: x_1}")
```

Note that the call to `setReactiveS3()` merely initializes objects with bidirectional bindings to the value `numeric(0)`:

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
removeReactive("x_1")
removeReactive("x_2")
```

## Scenario 5: bi-directional (2)

### Scenario explanation

- Type/Direction: 

  `A` references `B` and `B` references `A` --> bidirectional binding type
  
- Binding/Relationship: 

  `A` uses transformed value of `B` and `B` uses transformed value of `A`. 
  
  The binding functions used result in a **steady state**.

### Example

As the binding functions are "inversions"" of each other, we still stay at a steady state.

```
setReactiveS3(id = "x_1", function() {
  "object-ref: {id: x_2}"
  x_2 * 2
})

setReactiveS3(id = "x_2", function() {
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
removeReactive("x_1")
removeReactive("x_2")
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
setReactiveS3(id = "x_1", function() {
  "object-ref: {id: x_2}"
  x_2 * 2
})

setReactiveS3(id = "x_2", function() {
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
removeReactive("x_1")
removeReactive("x_2")
```

----

## Unsetting reactive objects

This turns reactive objects (that are, even though hidden from the user, instances of class `ReactiveObject.S3`) into regular or non-reactive objects again. 

**Note that it does not mean the a reactive object is removed alltogether! See `removeReactive()` for that purpose**

```
setReactiveS3(id = "x_1", value = 10)
setReactiveS3(id = "x_2", value = function() "object-ref: {id: x_1}")

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
What happens when a reactive relationship is broken or removed depends on how you set argument `strictness_get` in the call to `setReactiveS3()` or `setShinyReactive()`. 

Also refer to vignette [Strictness](https://github.com/Rappster/reactr/blob/master/vignettes/strictness.Rmd) for more details.

## Removing reactive objects

This deletes the object alltogether. 

```
setReactiveS3(id = "x_1", value = 10)
setReactiveS3(id = "x_2", value = function() "object-ref: {id: x_1}")

## Remove reactive --> remove it from `where` //
removeReactive(id = "x_1")

exists("x_1", inherits = FALSE)
```

## Caching mechanism (overview)

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
setReactiveS3(id = "x_1", value = 10)
setReactiveS3(id = "x_2", value = function() "object-ref: {id: x_1}")
```

#### Get the registry object

```
registry <- getRegistry()
```

#### Show registry content

```
showRegistry()
`` 

The registry contains the UIDs of the reactive objects that have been set via `setReactiveS3`. See `computeObjectUid()` for the details of the computation of object UIDs.

#### Retrieve from registry

```
x_1_hidden <- getFromRegistry(id = "x_1")
x_2_hidden <- getFromRegistry(id = "x_2")

## Via UID //
getFromRegistry(computeObjectUid("x_1"))
getFromRegistry(computeObjectUid("x_2"))

```

This object corresponds to the otherwise "hidden part"" of `x_1` that was implicitly created by the call to `setReactiveS3()`.

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
removeFromRegistry(id = "x_1")
## --> notice that entry `2fc2e352f72008b90a112f096cd2d029` has been removed

## Via UID //
removeFromRegistry(computeObjectUid("x_2"))
## --> notice that entry `ab22808532ff42c87198461640612405` has been removed
```

#### Reset registry

```
showRegistry()
resetRegistry()
showRegistry()
```
