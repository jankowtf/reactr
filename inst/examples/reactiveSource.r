\dontrun{

## Create reactive source //
reactiveSource(id = "x_1", value = 10)
x_1
x_1 <- 20
x_1

## No overwrite //
reactiveSource(id = "x_1", value = 10)
x_1
reactiveSource(id = "x_1", value = 20, overwrite = FALSE)
x_1
## --> as `x_1` already existed with a non-NULL value and `overwrite = FALSE`,
## no overwrite has been performed

##------------------------------------------------------------------------------
## Typed //
##------------------------------------------------------------------------------

## Basics //
## Strict = 0:
(reactiveSource(id = "x_1", value = 10, typed = TRUE))
x_1 <- "hello world!"
x_1
## --> simply ignored 

## Strict = 1:
(reactiveSource(id = "x_1", value = 10, typed = TRUE, strict = 1))
try(x_1 <- "hello world!")
x_1
## --> ignored with warning

## Strict = 2:
(reactiveSource(id = "x_1", value = 10, typed = TRUE, strict = 2))
try(x_1 <- "hello world!")
x_1
## --> ignored with error

## Advanced //
(reactiveSource(id = "x_1", typed = TRUE, from_null = FALSE, strict = 2))
try(x_1 <- "hello world!")

(reactiveSource(id = "x_1", value = 10, typed = TRUE, to_null = FALSE, strict = 2))
try(x_1 <- NULL)

(reactiveSource(id = "x_1", value = 10, typed = TRUE, numint = FALSE, strict = 2))
try(x_1 <- as.integer(10))

}
