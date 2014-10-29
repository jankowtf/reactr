\dontrun{

##------------------------------------------------------------------------------
## setReactiveS3 --> ReactiveObject.S3 //
##------------------------------------------------------------------------------

setReactiveS3(id = "x_1", value = 10)
copyReactive(id_from = "x_1", id_to = "x_1_c")
x_1_c == x_1

x_1 <- 20
x_1_c
## --> x_1_c is not affected by the change in `x_1` as the two underlying 
## hidden objects are independent from each other:
identical(getFromRegistry("x_1"), getFromRegistry("x_1_c"))

## Classes //
class(getFromRegistry("x_1"))
class(getFromRegistry("x_1_c"))


##------------------------------------------------------------------------------
## setShinyReactive --> ReactiveShinyObject //
##------------------------------------------------------------------------------

setShinyReactive(id = "x_1", value = 10)
copyReactive(id_from = "x_1", id_to = "x_1_c")
x_1_c == x_1

x_1 <- 20
x_1_c
## --> x_1_c is not affected by the change in `x_1` as the two underlying 
## hidden objects are independent from each other:
identical(getFromRegistry("x_1"), getFromRegistry("x_1_c"))

## Classes //
class(getFromRegistry("x_1"))
class(getFromRegistry("x_1_c"))

}
