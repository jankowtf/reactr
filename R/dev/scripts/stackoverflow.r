setShinyReactive(id = "x_1", value = 1:5)
setShinyReactive(id = "x_2", value = reactiveExpression(x_1 * 2))

x_1
x_2
x_1 <- x_1 * 2
x_2
x_1 <- 10
x_2

setShinyReactive(id = "x_3", value = reactiveExpression(
  data.frame(x_1 = x_1, x_2 = x_2)))

x_3
x_1 <- 1:3
x_2
x_3
x_1 <- 1
x_2
x_3
x_1 <- 1:3

setShinyReactive(id = "x_4", value = reactiveExpression(
  list(
    x_1 = summary(x_1), 
    x_2 = summary(x_2), 
    x_3_new = data.frame(x_3, prod = x_3$x_1 * x_3$x_2),
    filenames = paste0("file_", x_1)
  )
))

x_1
x_2
x_3
x_4
x_1 <- 1
x_2
x_3
x_4

################################################################################

setShinyReactive(id = "x_1", value = 1:5, typed = TRUE, strict_set = 2)
setShinyReactive(id = "x_2", value = reactiveExpression(x_1 * 2), typed = TRUE,
                 strict_set = 2)

setShinyReactive(id = "x_3", value = reactiveExpression(
  data.frame(x_1 = x_1, x_2 = x_2)), typed = TRUE, strict_set = 2)

setShinyReactive(id = "x_4", value = reactiveExpression(
  list(
    x_1 = summary(x_1), 
    x_2 = summary(x_2), 
    x_3_new = data.frame(x_3, prod = x_3$x_1 * x_3$x_2),
    filenames = paste0("file_", x_1)
  )
), strict_set = 2)

x_1
x_2
x_3
x_4
x_1 <- 1
x_2
x_3
x_4

################################################################################

## Ensure that shiny let's us do our thing //
shiny_opt <- getOption("shiny.suppressMissingContextError")
if (is.null(shiny_opt) || !shiny_opt) {
  options(shiny.suppressMissingContextError = TRUE)  
}

shiny::makeReactiveBinding("x_1")
x_1 <- 1:5
x_2 <- shiny::reactive(x_1 * 2)
x_3 <- shiny::reactive(data.frame(x_1 = x_1, x_2 = x_2()))
x_4 <- shiny::reactive(list(
  x_1 = summary(x_1), 
  x_2 = summary(x_2()), 
  x_3_new = data.frame(x_3(), prod = x_3()$x_1 * x_3()$x_2),
  filenames = paste0("file_", x_1)
))  

x_1
x_2()
x_3()
x_4()

x_1 <- 1
x_2()
x_3()
x_4()

x_1 <- 1:2
x_2()
x_3()
x_4()

rm(a, b)
a <- 10
shiny::makeReactiveBinding("a")
b <- shiny::reactive(a * 2)
b()

a <- 20
b()


library(shiny)
rm(a, b)
options(shiny.suppressMissingContextError=TRUE)
makeReactiveBinding("a")
makeReactiveBinding("b")
observe(a <<- b)
observe(b <<- a)
shiny:::setAutoflush(TRUE)
(a <- 1)
b
(b <- 2)
a
(a <- 3)
b

################################################################################

## R6 issue 
require("R6")
if (FALSE) {
#   .R6 <- setOldClass(".R6")
#   isClass(".R6")
  Test_1 <- setRefClass("Test1",
    fields = list(
      .x = "numeric"
    )
  )
  .R6 <- R6Class(
    classname = ".R6"
  )
  Test_2 <- R6Class(
    classname = "Test2",
    inherit = .R6,
    portable = TRUE,
    public = list(
      .x = "numeric"
    )
  )
  
  setGeneric("foo", signature = c("x", "y"),
    def = function(x, y) standardGeneric("foo")       
  )
  setMethod("foo", c(x = "character", y = "envRefClass"), 
    definition = function(x, y) {
      print(x)
      print(y)
    }
  )
  setMethod("foo", c(x = "character", y = "R6"), 
    definition = function(x, y) {
      print(x)
      print(y)
    }
  )
  foo(x = "1", y = Test_2$new())
  y <- Test_2$new()
  class(y)
  attributes(Test_2)
  
  setMethod("foo", c(x = "character", y = ".R6"), 
    definition = function(x, y) {
      print(x)
      print(y)
    }
  )
  foo(x = "1", y = Test_2$new())
  y <- Test_2$new()
  class(y)
  
  setGeneric("isReactive", signature = c("x", "y"),
    def = function(x, y) standardGeneric("isReactive")       
  )
  setMethod("isReactive", c(x = "character", y = "envRefClass"), 
    definition = function(x, y) {
      "envRefClass"
    }
  )
  setMethod("isReactive", c(x = "character", y = "R6"), 
    definition = function(x, y) {
      "r6"
    }
  )
  isReactive(x = "a", y = Test$new())
}
y <- Test_1$new()
foo(x = "a", y = y)

y <- Test_2$new()
class(y)
foo(x = "a", y = y)

?R6Class

## Actual question //
TestRefClass <- setRefClass("TestRefClass", fields= list(.x = "numeric"))
setGeneric("foo", signature = "x",
  def = function(x) standardGeneric("foo")
)
setMethod("foo", c(x = "envRefClass"),
  definition = function(x) {
    "I'm the method for `envRefClass`"
})
try(foo(x = TestRefClass$new()))

class(TestRefClass$new())

attributes(TestRefClass)
inherits(TestRefClass)

TestR6 <- R6Class("TestR6", public = list(.x = "numeric"))
setMethod("foo", c(x = "R6"),
  definition = function(x) {
    "I'm the method for `R6`"
})
try(foo(x = TestR6$new()))

class(TestR6$new())

R6 <- setOldClass("R6")
isClass("R6")

dummy <- structure("something", class = "R6")
foo(dummy)

try(foo(x = TestR6$new()))

.R6 <- R6Class(".R6")
TestR6_2 <- R6Class("TestR6_2", inherit = .R6, public = list(.x = "numeric"))
setMethod("foo", c(x = ".R6"),
  definition = function(x) {
    "I'm the method for `.R6`"
})
try(foo(x = TestR6_2$new()))


setOldClass("R6")
require("R6")
TestR6 <- R6Class("TestR6", public = list(.x = "numeric"))

setGeneric("foo", signature = "x",
  def = function(x) standardGeneric("foo")
)
setMethod("foo", c(x = "R6"),
  definition = function(x) {
    "I'm the method for `R6`"
})
try(foo(x = TestR6$new()))

.R6 <- R6Class(".R6")
setOldClass(c("TestR6_2", ".R6", "R6"))
TestR6_2 <- R6Class("TestR6_2", inherit = .R6, public = list(.x = "numeric"))

setGeneric("foo", signature = "x",
  def = function(x) standardGeneric("foo")
)
setMethod("foo", c(x = "R6"),
  definition = function(x) {
    "I'm the method for `R6`"
})
try(foo(x = TestR6_2$new()))


.onLoad <- function(libname, pkgname) {
  setOldClass(c("TestR6_1", "R6"))
  setOldClass(c("TestR6_2", "R6"))
  setOldClass(c("TestR6_3", "R6"))
}
