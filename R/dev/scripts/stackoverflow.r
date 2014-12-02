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

if (FALSE) {
  Test_1 <- setRefClass("Test",
    fields = list(
      .x = "numeric"
    )
  )
  Test_2 <- R6Class(
    classname = "Test",
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
}
y <- Test_1$new()
foo(x = "a", y = y)

y <- Test_2$new()
class(y)
foo(x = "a", y = y)


