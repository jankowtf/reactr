reactr
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

The default location that variables are set to is `.GlobalEnv`.

However, in order not to mess up things in `.GlobalEnv`, we will use 
an example environment:

```
where <- new.env()
```

### Binding scenario 1: simple monitoring (identical values)

Set a variable that can be monitored:

```
setValue(id = "x_1", value = 10, where = where)
```

Set a variable that monitors `x_1` and has a reactive binding to it:

```
setValue(id = "x_2", watch = "x_1", where = where)
```

Whenever `x_1` changes, `x_2` changes accordingly:

```
where$x_1 
where$x_2
where$x_1 <- 100 
where$x_2
```

#### NOTE

Using this approach, you can only set `x_1`. Variable `x_2` is a mere
"monitoring" variable. Trying to set it via `<-`, `assign()` or `setValue()` are
disregarded:

```
where$x_2 <- 1000
where$x_2
```

See scenario *Binding scenario: mutual binding* for an alternative
to this!

### Binding scenario 2: simple monitoring (arbitrary functional relationship)

Set a variable that monitors `x_1` and has a reactive binding to it:

```
setValue(id = "x_3", watch = "x_1", where = where, 
  binding = function(x) {x * 2})
```

Whenever `x_1` changes, `x_3` changes accordingly:

```
where$x_1 
where$x_2
where$x_3
where$x_1 <- 500
where$x_2
where$x_3
```

### Binding scenario 3: mutual binding (identical values)

#### NOTE: BUG (2014-09-21)
Currently, you need to reset `where` as a function to reset the hash registry
is still missing (this is scheduled for the next release):

```
where <- new.env()
```

Set two variables that have a mutual binding.
The main difference to *Binding scenario: identical* is, that you can set 
both `x_1` **and** `x_4`.


In order to do that, it is necessary to "re-set" `x_1` as well.

```
setValue(id = "x_1", watch = "x_4", where = where, mutual = TRUE)
setValue(id = "x_4", watch = "x_1", where = where, mutual = TRUE)
```

Whenever `x_1` changes, `x_4` changes accordingly and vice versa.

Note that variables with mutual bindings have a default value of `NULL`. 
After running `setValue()`, you must actually assign a value to either one 
of them via `<-`.

```
where$x_1
where$x_4

where$x_1 <- 100
where$x_1
where$x_4

where$x_4 <- 1000
where$x_4
where$x_1
```

### Binding scenario 4: mutual binding (valid bi-directional relationship)

```
setValue(id = "x_5", watch = "x_6", where = where, 
  binding = function(x) {x * 2}, mutual = TRUE)
setValue(id = "x_6", watch = "x_5", where = where, 
  binding = function(x) {x/2}, mutual = TRUE)

where$x_5
where$x_6

where$x_5 <- 100
where$x_5
where$x_6

where$x_6 <- 500
where$x_6
where$x_5
```

## Further examples

See `?setValue` and `setValue_bare`.
