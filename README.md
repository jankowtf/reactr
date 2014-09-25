reactr (v0.1.3.2)
======

Reactive object bindings with built-in caching

## Installation 

```
require("devtools")
devtools::install_github("Rappster/classr")
devtools::install_github("Rappster/reactr")
require("reactr")
```

## Quick intro 

The default location that objects are set to is `.GlobalEnv`.

However, in order not to mess up things in `.GlobalEnv`, we will use 
an example environment:

```
where <- new.env()
```

### Binding scenario 1: simple observing (identical values)

Set an object that can be observed:

```
setReactive(id = "x_1", value = 10, where = where)
# [1] 10
```

Set an object that observes `x_1` and has a reactive binding to it:

```
setReactive(id = "x_2", watch = "x_1", where = where)
# [1] 10
```

Whenever `x_1` changes, `x_2` changes accordingly:

```
where$x_1 
# [1] 10
where$x_2
# [1] 10
where$x_1 <- 100 
where$x_1 
# [1] 100
where$x_2
# [1] 100
```

#### NOTE

Using this approach, you can only set `x_1`. Object `x_2` is a mere
"observing" object. Trying to set it via `<-`, `assign()` or `setReactive()` is
disregarded:

```
where$x_2 <- 1000
where$x_2
# [1] 100
```

See scenario *Binding scenario: mutual binding* for an alternative
to this!

-----

### Binding scenario 2: simple observing (arbitrary functional relationship)

Set an object that observes `x_1` and has a reactive binding to it:

```
setReactive(id = "x_3", watch = "x_1", where = where, 
  binding = function(x) {x * 2})
# [1] 200  
```

Note how `x_3` changes according to its binding contract based on the provided
`binding` (`x * 2`):

```
where$x_1 
# [1] 100
where$x_2
# [1] 100
where$x_3
# [1] 200
where$x_1 <- 500
where$x_2
# [1] 500
where$x_3
# [1] 1000
```

-----

### Binding scenario 3: mutual binding (identical values)

Set two objects that have a mutual binding.
The main difference to *Binding scenario 1* is, that you can set 
both `x_1` **and** `x_4` and have the changes reflected.

In order to do that, it is necessary to reset the binding for `x_1` as well 
with `mutual = TRUE`:

```
setReactive(id = "x_1", watch = "x_4", where = where, mutual = TRUE)
# NULL
setReactive(id = "x_4", watch = "x_1", where = where, mutual = TRUE)
# NULL
```

Note that objects with mutual bindings are merely initialized by `setReactive()` 
and have a default value of `NULL`. You must actually assign a value to either 
one of them via `<-` **after** establishing the binding:

```
## Default value //
where$x_1
# NULL
where$x_4
# NULL

## Set actual initial value to either one of the objects //
where$x_1 <- 100
where$x_1
# [1] 100
where$x_4
# [1] 100
where$x_2
# [1] 100
where$x_3
# [1] 200

where$x_4 <- 1000
where$x_4
# [1] 1000
where$x_1
# [1] 1000
where$x_2
# [1] 1000
where$x_3
# [1] 2000
```

-----

### Binding scenario 4: mutual binding (valid bi-directional relationship)

The binding contract for objects with mutual bindings does not have to 
be based on the standard binding definition of 
(`function(x) {x}` set automatically inside `setReactive()`). You should just make
sure that it is a valid bi-directional relationship that you define:

```
setReactive(id = "x_5", watch = "x_6", where = where, 
  binding = function(x) {x * 2}, mutual = TRUE)
# NULL
setReactive(id = "x_6", watch = "x_5", where = where, 
  binding = function(x) {x / 2}, mutual = TRUE)
# NULL

## Initial default values //
where$x_5
# NULL
where$x_6
# NULL

## Actual initial value //
where$x_5 <- 100
where$x_5
# [1] 100
where$x_6
# [1] 50

where$x_6 <- 500
where$x_6
# [1] 500
where$x_5
# [1] 1000
```

-----

### Tracing what's actually going on

To understand what's going on behind the scenes, I've include a `.tracelevel`
argument that you can use:

```
setReactive(id = "x_7", watch = "x_8", where = where, mutual = TRUE, .tracelevel = 1)
# ----- INIT START -----
# id:
# x_7
# watch:
# x_8
# ----- INIT END -----
# ----- BINDING CONTRACT START -----
# id:
# x_7
# watch:
# x_8
# hash id/id:
# [1] "f9e884084b84794d762a535f3facec85"
# hash id/watch:
# NULL
# hash watch/watch:
# [1] "f9e884084b84794d762a535f3facec85"
# hash watch/id:
# [1] "f9e884084b84794d762a535f3facec85"
# ----- BINDING CONTRACT END -----
# NULL
```

```
setReactive(id = "x_8", watch = "x_7", where = where, mutual = TRUE, .tracelevel = 1)
# ----- INIT START -----
# id:
# x_8
# watch:
# x_7
# ----- BINDING CONTRACT START -----
# id:
# x_7
# watch:
# x_8
# hash id/id:
# [1] "f9e884084b84794d762a535f3facec85"
# hash id/watch:
# [1] "f9e884084b84794d762a535f3facec85"
# hash watch/watch:
# [1] "f9e884084b84794d762a535f3facec85"
# hash watch/id:
# [1] "f9e884084b84794d762a535f3facec85"
# ----- BINDING CONTRACT END -----
# ----- INIT END -----
# ----- BINDING CONTRACT START -----
# id:
# x_8
# watch:
# x_7
# hash id/id:
# [1] "f9e884084b84794d762a535f3facec85"
# hash id/watch:
# [1] "f9e884084b84794d762a535f3facec85"
# hash watch/watch:
# [1] "f9e884084b84794d762a535f3facec85"
# hash watch/id:
# [1] "f9e884084b84794d762a535f3facec85"
# retrieve (x_8 watching x_7)
# in sync (x_8 watching: x_7)
# ----- BINDING CONTRACT END -----
# NULL
```

```
where$x_7 <- Sys.time()
# ----- BINDING CONTRACT START -----
# id:
# x_7
# watch:
# x_8
# hash id/id:
# [1] "f9e884084b84794d762a535f3facec85"
# hash id/watch:
# [1] "f9e884084b84794d762a535f3facec85"
# hash watch/watch:
# [1] "f9e884084b84794d762a535f3facec85"
# hash watch/id:
# [1] "f9e884084b84794d762a535f3facec85"
# setting x_7
# new hash id/id:
# [1] "330bf68a5152022e9d08b995d6bb3d88"
# ----- BINDING CONTRACT END -----
```

```
where$x_7
# ----- BINDING CONTRACT START -----
# id:
# x_7
# watch:
# x_8
# hash id/id:
# [1] "91c8bc5b91169b03e8405d78132e8f00"
# hash id/watch:
# [1] "f9e884084b84794d762a535f3facec85"
# hash watch/watch:
# [1] "f9e884084b84794d762a535f3facec85"
# hash watch/id:
# [1] "f9e884084b84794d762a535f3facec85"
# retrieve (x_7 watching x_8)
# in sync (x_7 watching: x_8)
# ----- BINDING CONTRACT END -----
# [1] "2014-09-22 14:40:21 CEST"
```

```
where$x_8
# ----- BINDING CONTRACT START -----
# id:
# x_8
# watch:
# x_7
# hash id/id:
# [1] "f9e884084b84794d762a535f3facec85"
# hash id/watch:
# [1] "f9e884084b84794d762a535f3facec85"
# hash watch/watch:
# [1] "91c8bc5b91169b03e8405d78132e8f00"
# hash watch/id:
# [1] "f9e884084b84794d762a535f3facec85"
# retrieve (x_8 watching x_7)
# update based on contract (x_8 watching x_7)
# hash watch/watch old: 91c8bc5b91169b03e8405d78132e8f00
# hash watch/id old: f9e884084b84794d762a535f3facec85
# ----- BINDING CONTRACT START -----
# id:
# x_7
# watch:
# x_8
# hash id/id:
# [1] "91c8bc5b91169b03e8405d78132e8f00"
# hash id/watch:
# [1] "f9e884084b84794d762a535f3facec85"
# hash watch/watch:
# [1] "f9e884084b84794d762a535f3facec85"
# hash watch/id:
# [1] "f9e884084b84794d762a535f3facec85"
# retrieve (x_7 watching x_8)
# in sync (x_7 watching: x_8)
# ----- BINDING CONTRACT END -----
# hash watch/watch new: 91c8bc5b91169b03e8405d78132e8f00
# hash watch/id new: 91c8bc5b91169b03e8405d78132e8f00
# ----- BINDING CONTRACT END -----
# [1] "2014-09-22 14:40:21 CEST"
```

```
where$x_8 <- Sys.time()
# ----- BINDING CONTRACT START -----
# id:
# x_8
# watch:
# x_7
# hash id/id:
# [1] "91c8bc5b91169b03e8405d78132e8f00"
# hash id/watch:
# [1] "91c8bc5b91169b03e8405d78132e8f00"
# hash watch/watch:
# [1] "91c8bc5b91169b03e8405d78132e8f00"
# hash watch/id:
# [1] "91c8bc5b91169b03e8405d78132e8f00"
# setting x_8
# new hash id/id:
# [1] "d3e83df21e0f7b4da01f6ba2e215bb43"
# ----- BINDING CONTRACT END -----
```

```
where$x_8
# ----- BINDING CONTRACT START -----
# id:
# x_8
# watch:
# x_7
# hash id/id:
# [1] "d3e83df21e0f7b4da01f6ba2e215bb43"
# hash id/watch:
# [1] "91c8bc5b91169b03e8405d78132e8f00"
# hash watch/watch:
# [1] "91c8bc5b91169b03e8405d78132e8f00"
# hash watch/id:
# [1] "91c8bc5b91169b03e8405d78132e8f00"
# retrieve (x_8 watching x_7)
# in sync (x_8 watching: x_7)
# ----- BINDING CONTRACT END -----
# [1] "2014-09-22 14:41:18 CEST"
```

```
where$x_7
# ----- BINDING CONTRACT START -----
# id:
# x_7
# watch:
# x_8
# hash id/id:
# [1] "91c8bc5b91169b03e8405d78132e8f00"
# hash id/watch:
# [1] "91c8bc5b91169b03e8405d78132e8f00"
# hash watch/watch:
# [1] "d3e83df21e0f7b4da01f6ba2e215bb43"
# hash watch/id:
# [1] "91c8bc5b91169b03e8405d78132e8f00"
# retrieve (x_7 watching x_8)
# update based on contract (x_7 watching x_8)
# hash watch/watch old: d3e83df21e0f7b4da01f6ba2e215bb43
# hash watch/id old: 91c8bc5b91169b03e8405d78132e8f00
# ----- BINDING CONTRACT START -----
# id:
# x_8
# watch:
# x_7
# hash id/id:
# [1] "d3e83df21e0f7b4da01f6ba2e215bb43"
# hash id/watch:
# [1] "91c8bc5b91169b03e8405d78132e8f00"
# hash watch/watch:
# [1] "91c8bc5b91169b03e8405d78132e8f00"
# hash watch/id:
# [1] "91c8bc5b91169b03e8405d78132e8f00"
# retrieve (x_8 watching x_7)
# in sync (x_8 watching: x_7)
# ----- BINDING CONTRACT END -----
# hash watch/watch new: d3e83df21e0f7b4da01f6ba2e215bb43
# hash watch/id new: d3e83df21e0f7b4da01f6ba2e215bb43
# ----- BINDING CONTRACT END -----
# [1] "2014-09-22 14:41:18 CEST"
```

----

## Further examples

See `?setReactive` and `?setReactive_bare`.


----

## Unsetting reactive objects

```
where <- new.env()  

setReactive(id = "x_1", value = 10, where = where)
setReactive(id = "x_2", watch = "x_1", where = where)

## Illustrate reactiveness //
where$x_1
where$x_2
where$x_1 <- 50
where$x_1 
where$x_2

## Unset reactive --> turn it into a regular object again //
unsetReactive(id = "x_1", where = where)

## Illustrate removed reactiveness //
where$x_1
where$x_2
where$x_1 <- 10
where$x_1
where$x_2
## --> 'x_1' is not a reactive object anymore; from now on, 'x_2' simply returns
## the last value that has been cached
```

## Removing reactive objects

This means deleting the object alltogether. 

```
where <- new.env()  

setReactive(id = "x_1", value = 10, where = where)
setReactive(id = "x_2", watch = "x_1", where = where)

## Remove reactive --> remove it from 'where' //
unsetReactive(id = "x_1", where = where)

exists("x_1", envir = where, inherits = FALSE)

## Illustrate implications on observing objects //
where$x_2  
## --> returns last cached value
```
