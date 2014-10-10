\dontrun{
  
##------------------------------------------------------------------------------  
## Set reactive objects in parent environment //
##------------------------------------------------------------------------------

## Set object that other objects can have reactive bindings to //
setShinyReactive(id = "x_1", value = Sys.time())
## --> 'x_1' is set in 'environment()' so you don't explicitly need to assign
## the return value of 'shinyRective()'  to 'x_1'. Of course you can also do so:
x_1
x_1 <- setShinyReactive(id = "x_1", value = Sys.time())
x_1 <- Sys.time()
x_1

## Set object with reactive bindings to 'x_1' //
setShinyReactive(id = "x_2", value = reactive(x_1 + 60*60*24))
x_2
## --> 'x_1' + one day
(x_1 <- Sys.time())
x_2
## --> reactive

## Clean up //
suppressWarnings(rm(x_1))
suppressWarnings(rm(x_2))

##------------------------------------------------------------------------------  
## Set reactive objects in custom environment //
##------------------------------------------------------------------------------

where <- new.env()

## Set object that other objects can have reactive bindings to //
setShinyReactive(id = "x_1", value = Sys.time(), where = where)
where$x_1
where$x_1 <- Sys.time()
where$x_1

## Set object with reactive bindings to 'x_1' //
setShinyReactive(id = "x_2", 
  value = reactive(where$x_1 + 60*60*24), 
  where = where
)
where$x_2
## --> 'where$x_1' + one day
(where$x_1 <- Sys.time())
where$x_2
## --> reactive

## Clean up //
suppressWarnings(rm(where))

##------------------------------------------------------------------------------  
## Mutltiple reactive bindings //
##------------------------------------------------------------------------------

x_1 <- setShinyReactive("x_1", 10)
x_2 <- setShinyReactive("x_2", 20)
x_3 <- setShinyReactive("x_3", value = reactive(x_1 + x_2 * 2))
x_3
## --> 'x_1' + 'x_2' * 2
(x_1 <- 100)
x_3
(x_2 <- 100)
x_3

## Clean up //
suppressWarnings(rm(x_1))
suppressWarnings(rm(x_2))
suppressWarnings(rm(x_3))

##------------------------------------------------------------------------------  
## Mutual reactive bindings //
##------------------------------------------------------------------------------

## NOTE
## To the best of my knowledge, the reactive paradigm implemented by the 
## shiny framework does not offer the possibility to define mutual reactive 
## bindings. 
## 
## Thus, something like \code{x_1} has reactive binding \code{reactive{x_2 * 2}} 
## and \code{x_2} has reactive binding \code{reactive{x_1 / 2}} where \strong{both} objects can be 
## modified via \code{\link{<-}} can not be specified. The reason for this is
## that reactivity is implemented in a direct or immediate manner: whenever 
## \code{x_1} that has a reactive binding to \code{x_2} is requested, it runs
## its reactive binding function even though \code{x_2} might not have changed
## at all. Thus, mutual reactive bindings of the above form result in an 
## infinite recursion. 
## 
## If you would like to define mutual reactive bindings, you currently need to 
## use \code{\link[reactr]{setReactiveS3}} as it implements a value caching 
## mechanism that allows reactive functions only to be triggered when actually
## needed, i.e. when the referenced object has actually changed.

try(x_1 <- setShinyReactive("x_1", reactive(x_2 * 2)))
try(x_2 <- setShinyReactive("x_2", reactive(x_1 / 2)))

}
