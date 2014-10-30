\dontrun{

## Create reactive source //
setReactiveSource(id = "x_1", value = 10)
x_1
x_1 <- 20
x_1

## No overwrite //
setReactiveSource(id = "x_1", value = 10)
x_1
setReactiveSource(id = "x_1", value = 20, overwrite = FALSE)
x_1
## --> as `x_1` already existed with a non-NULL value and `overwrite = FALSE`,
## no overwrite has been performed

## Typed //
setReactiveSource(id = "x_1", value = 10, typed = TRUE)
x_1 <- 20
try(x_1 <- "hello world!")

}
