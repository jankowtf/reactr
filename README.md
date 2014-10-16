reactr
======

Reactive object bindings with built-in caching

## Installation 

```
require("devtools")
devtools::install_github("Rappster/yamlr")
devtools::install_github("Rappster/reactr")
require("reactr")
```
## Overview 

The package tries to make a contribution with respect to ways for *Reactive Programming* or *Reactivity* in R. It allows to dynamically link objects so that if one object changes, the objects referencing that object (in whatever way) are updated as well. 

The implementation was greatly inspired by and is very similar to that of [shiny](http://shiny.rstudio.com) framework. 

### Quick Example

Set object `x_1` that others can reference:

```
setReactiveS3(id = "x_1", value = 10)
# [1] 10
```

Set object that references `x_1` and has the reactive binding `x_1 * 2` to it:

```
setReactiveS3(id = "x_2", value = function() {
  "object-ref: {id: x_1}"
  x_1 * 2
})
# Initializing ...
# [1] 20

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
# Modified reference: 2fc2e352f72008b90a112f096cd2d029
# Updating ...
# [1] 200

x_2
# [1] 200
## --> cached value is used as `x_1` has not changed, so executing the 
## binding function would be unnecessary
```

### Things to notice at this point

1. The preferred way to specify the reference is via [YAML](http://www.yaml.org/) markup as in the example above. However, there also exist two other ways to specify references. See vignette [Specifying Reactive References](https://github.com/Rappster/reactr/blob/master/vignettes/specifying_reactive_references.Rmd) for details.

2. Strictness levels can be defined for 

  - the creation process itself in `setReactiveS3()`: argument `strict`
  - *getting* the value of a reactive object: argument `strict_get`
  - *setting* the value of a reactive object: argument `strict_set`
  
  See vignette [Strictness](https://github.com/Rappster/reactr/blob/master/vignettes/strictness.Rmd) for details.
  
3. The environment in which to set a reactive object can be chosen via argument `where`

4. The package implements a **caching mechanism**: the binding functions are only executed when they need to be, i.e. only if one of the referenced objects has actually changed. Otherwise a cached value that has been stored from the last update run is returned.

  While this may cost more than it actually helps in scenarios where the binding functions are quite simple and thus don't take long to run, such a mechanism *may* significantly reduce computation times in case of more complex binding functions that take very long to run. See vignette [Caching](https://github.com/Rappster/reactr/blob/master/vignettes/caching.Rmd) for details.
  
5. You can choose between a **pull** and a **push** paradigm with respect to how changes are propagated through the system. 

  When using *pull* paradigm (the default), objects referencing an object that has changed are not informed of this change until they are explicitly requested (by `get()` or its syntactical sugars).
  
  When using a *push* paradigm, an object that changed informs all objects that have a reference to it about the change by implicitly calling the `$get()` method of their `ReactiveObject.S3` class instance which translates to an actual `get()` of the respective reactive objects. 
  
  See vignette [Pushing](https://github.com/Rappster/reactr/blob/master/vignettes/pushing.Rmd) for details on this.

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

# [1] 10
```

Set object that references `x_1` and has a reactive binding to it:

```
setReactiveS3(id = "x_2", value = function() "object-ref: {id: x_1}")
# Initializing ...
# [1] 10

x_1 
# [1] 10

x_2
# [1] 10
```

Whenever `x_1` changes, `x_2` changes accordingly:


```
(x_1 <- 100)
# [1] 100

x_2
# Modified reference: 2fc2e352f72008b90a112f096cd2d029
# Updating ...
# [1] 100

x_2
# [1] 100
## --> cached value as `x_1` has not changed; no update until `x_1` 
## changes again
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
setReactiveS3(id = "x_3", value = function() {
  "object-ref: {id: x_1, as: ref_1}"
  ref_1 * 2
})
# Initializing ...
# [1] 200
```

Note how `x_3` changes according to its binding relationship `ref_1 * 2` (which is just a translation for `x_1 * 2`):

```
x_1 
# [1] 100

x_2
# [1] 100

x_3
# [1] 200
## --> x_1 * 2

(x_1 <- 500)
x_2
# Modified reference: 2fc2e352f72008b90a112f096cd2d029
# Updating ...
# [1] 500

x_3
# Modified reference: 2fc2e352f72008b90a112f096cd2d029
# Updating ...
# [1] 1000
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
setReactiveS3(id = "x_4", value = function() {
  "object-ref: {id: x_1, as: ref_1}"
  "object-ref: {id: x_2, as: ref_2}"
  ref_1 + ref_2 * 2
})
# Initializing ...
# [1] 1500
```

Note how each object that is involved changes according to its binding relationships:

```
x_4
# [1] 1500

(x_1 <- 10)

x_4
# Modified reference: 2fc2e352f72008b90a112f096cd2d029
# Updating ...
## --> corresponds to the update of `x_2` due to a change of `x_1`

# Modified reference: 2fc2e352f72008b90a112f096cd2d029
# Updating ...
## --> corresponds to the update of `x_4` due to a change of `x_1`
[1] 30

(x_2 <- 100)
## --> NOTE
## this is only allowed due to `strict_set = 0`! By doing so, we temporariliy 
## break/disable or pause the reactive binding of `x_2` on `x_2`.
## As long as `x_1` is not updated, `x_2` is "out of sync" with respect to `x_1`

x_4
# Modified reference: ab22808532ff42c87198461640612405
# Updating ...
# [1] 210
## --> corresponds to the update of `x_4` due to a change of `x_2` 

(x_1 <- 50)
x_2
# Modified reference: 2fc2e352f72008b90a112f096cd2d029
# Updating ...
# [1] 50
## --> reactive binding reestablished again; `x_2` is now in-sync again 
## with respect to `x_1`

x_4
# Modified reference: 2fc2e352f72008b90a112f096cd2d029
# Modified reference: ab22808532ff42c87198461640612405
# Updating ...
# [1] 150
## --> update as both `x_1` and `x_2` have changed
```

## Scenario 4: bi-directional (1)

### Scenario explanation

- Type/Direction: 

  `A` references `B` and `B` references `A` --> bidirectional binding type
  
- Binding/Relationship: 

  `A` uses value of `B` "as is" and `B` uses value of `A` "as is". This results in a steady state. 

### Example

A cool feature of this binding type is that you are free to alter the values of *both* objects and still keep everything "in sync"

```
setReactiveS3(id = "x_5", function() "object-ref: {id: x_6}")
# Initializing ...
# numeric(0)

setReactiveS3(id = "x_6", function() "object-ref: {id: x_5}")
# Initializing ...
# Modified reference: 9032d7c46f03dd5177f6f16eb729baf9
# Updating ...
# Initializing ...
# numeric(0)
```

Note that the call to `setReactiveS3()` merely initializes objects with bidirectional bindings to the value `numeric(0)`.

You must actually assign a value to either one of them via `<-` **after** establishing the binding:

```
## Default values //
x_5
# Modified reference: 9032d7c46f03dd5177f6f16eb729baf9
# Updating ...
# numeric(0)

x_6
# numeric(0)

## Set actual initial value to either one of the objects //
(x_5 <- 100)
# [1] 100

x_6
# Modified reference: 25165b8d029c31f694793c3c13fbbee1
# Updating ...
# [1] 100

x_5
# Modified reference: 9032d7c46f03dd5177f6f16eb729baf9
# Updating ...
# [1] 100

## Changing the other one of the two objects //
(x_6 <- 1000)
# [1] 1000

x_5
# Modified reference: 9032d7c46f03dd5177f6f16eb729baf9
# Updating ...
# [1] 1000
```

## Scenario 4: bi-directional (2)

### Scenario explanation

- Type/Direction: 

  `A` references `B` and `B` references `A` --> bidirectional binding type
  
- Binding/Relationship: 

  `A` uses transformed value of `B` and `B` uses transformed value of `A`. 
  
  The binding functions used result in a **steady** state.

### Example

As the binding functions are "inversions"" of each other, we still stay at a steady state.

```
setReactiveS3(id = "x_6", function() {
  "object-ref: {id: x_7}"
  x_7 * 2
})
# Initializing ...
# numeric(0)

setReactiveS3(id = "x_7", function() {
  "object-ref: {id: x_6}"
  x_6 / 2
})
# Initializing ...
# Modified reference: d02321209550bf005cbade3bf09fdd85
# Updating ...
# Initializing ...
# numeric(0)

x_6
# Modified reference: d02321209550bf005cbade3bf09fdd85
# Updating ...
# numeric(0)

x_7
# numeric(0)

## Set actual initial value to either one of the objects //
(x_6 <- 100)
# [1] 100

x_7
# Modified reference: 9032d7c46f03dd5177f6f16eb729baf9
# Updating ...
# [1] 50

x_6
# Modified reference: d02321209550bf005cbade3bf09fdd85
# Updating ...
# [1] 100

## Changing the other one of the two objects //
(x_7 <- 1000)
# [1] 1000

x_6
# Modified reference: d02321209550bf005cbade3bf09fdd85
# Updating ...
# [1] 2000

x_7
# Modified reference: 9032d7c46f03dd5177f6f16eb729baf9
# Updating ...
# [1] 1000
```

## Scenario 4: bi-directional (3)

### Scenario explanation

- Type/Direction: 

  `A` references `B` and `B` references `A` --> bidirectional binding type
  
- Binding/Relationship: 

  `A` uses transformed value of `B` and `B` uses transformed value of `A`. 
  
  The binding functions used result in a **non-steady** state.

### Example

As the binding functions are **not** "inversions"" of each other, we never reach/stay at a steady state. Cached values are/can never be used as by the definition of the binding functions the two objects are constantly updating each other.

```
setReactiveS3(id = "x_8", function() {
  "object-ref: {id: x_9}"
  x_9 * 2
})
# Initializing ...
# NULL

setReactiveS3(id = "x_9", function() {
  "object-ref: {id: x_8}"
  x_8 * 10
})
# Initializing ...
# Modified reference: 794daff29ee5d00144e3dc00ef18107b
# Updating ...
# Initializing ...
# numeric(0)
```

Note that `x_8` is initialized to `NULL` and `x_9` `numeric()`.

This is a minor inconsistency due to the actual structure of these specific binding functions that will be removed in future releases.

Illustration of "non-steady-state" behavior:

```
x_8
# Modified reference: 794daff29ee5d00144e3dc00ef18107b
# Updating ...
# Modified reference: 49f6edc9dcf9dc2d4afa65bc5a008fdd
# Updating ...
# numeric(0)

x_9
# numeric(0)

## Set actual initial value to either one of the objects //
(x_8 <- 1)
# [1] 1

x_9
# Modified reference: 49f6edc9dcf9dc2d4afa65bc5a008fdd
# Updating ...
# [1] 10
## --> `x_8` * 10

x_8
# Modified reference: 794daff29ee5d00144e3dc00ef18107b
# Updating ...
# [1] 20
## --> x_9 * 2

x_9
# Modified reference: 49f6edc9dcf9dc2d4afa65bc5a008fdd
# Updating ...
# [1] 200
## --> `x_8` * 10

## Changing the other one of the two objects //
(x_9 <- 1)
# [1] 1

x_8
# Modified reference: 794daff29ee5d00144e3dc00ef18107b
# Updating ...
# [1] 2

x_9
# Modified reference: 49f6edc9dcf9dc2d4afa65bc5a008fdd
# Updating ...
# [1] 20

x_8
# Modified reference: 794daff29ee5d00144e3dc00ef18107b
# Updating ...
# [1] 40
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
What happens when a reactive relationship is broken or removed depends on how you set argument `strictness_get` in the call to `setReactiveS3()`. Also refer to vignette [Strictness](https://github.com/Rappster/reactr/blob/master/vignettes/strictness.Rmd) for more details.

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

### Convenience function

Ensuring example content in registry:

```
setReactiveS3(id = "x_1", value = 10)
setReactiveS3(id = "x_2", value = function() "object-ref: {id: x_1}")
```

#### Get the registry object

```
registry <- getRegistry()
```

#### Show registry content

```
showRegistryContent()
`` 

The registry contains the UIDs of the reactive objects that have been set via `setReactiveS3`. See `getObjectUid()` for the details of the computation of object UIDs.

#### Retrieve from registry

```
x_1_hidden <- getFromRegistry(id = "x_1")
x_2_hidden <- getFromRegistry(id = "x_2")

## Via UID //
getFromRegistry(getObjectUid("x_1"))
getFromRegistry(getObjectUid("x_2"))

```

This object corresponds to the otherwise "hidden part"" of `x_1` that was implicitly created by the call to `setReactiveS3()`.

```
class(x_1_hidden)
ls(x_1_hidden)

## Some interesting fields //
x_1_hidden$id
x_1_hidden$where
x_1_hidden$uid
x_1_hidden$value
x_1_hidden$hasReferences()

x_2_hidden$id
x_2_hidden$where
x_2_hidden$uid
x_2_hidden$value
x_2_hidden$has_cached
x_2_hidden$hasReferences()
ls(x_2_hidden$references)
x_2_hidden$references[[x_1_hidden$uid]]
```
#### Remove from registry

```
## Via ID (and `where`) //
removeFromRegistry(id = "x_1")
## --> notice that entry `2fc2e352f72008b90a112f096cd2d029` has been removed

## Via UID //
removeFromRegistry(getObjectUid("x_2"))
## --> notice that entry `ab22808532ff42c87198461640612405` has been removed
```

#### Reset from registry

```
showRegistryContent()
resetRegistry()
showRegistryContent()
```
