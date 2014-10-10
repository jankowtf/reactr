\dontrun{

where = new.env()  
setReactiveS3(id = "x_1", value = 10, where = where)
setReactiveS3(id = "x_2", 
              value = function(deps = list(x_1 = where)) x_1,
              where = where)

unsetReactiveByUid(uid = getReactiveUid("x_1", where))
where$x_1
identical(where$x_2, where$x_1)
where$x_1 <- 20
where$x_1
where$x_2
## --> no reactive binding to 'x_1' anymore

}
