\dontrun{

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

}
