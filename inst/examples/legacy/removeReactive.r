\dontrun{

where = new.env()  

setReactive(id = "x_1", value = 10, where = where)
setReactive(id = "x_2", watch = "x_1", where = where)

removeReactive(id = "x_1", where = where)
exists("x_1", envir = where, inherits = FALSE)
where$x_2

## Strict //
where = new.env()  
  
setReactive(id = "x_1", value = 10, where = where)
setReactive(id = "x_2", watch = "x_1", where = where, strict = TRUE)
removeReactive(id = "x_1", where = where)
where$x_2
## --> as observed object 'x_1' does not exist anymore

}
