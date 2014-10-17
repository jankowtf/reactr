\dontrun{ 

setReactiveS3(id = "x_1", value = 10)
getReactive(id = "x_1")
res <- getReactive(id = "x_1", hidden = TRUE)
res
class(res)

identical(res, getFromRegistry(id = "x_1"))

}
