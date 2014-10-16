\dontrun{

##------------------------------------------------------------------------------  
## Non-strict get behavior //  
##------------------------------------------------------------------------------

## Set example reactive objects //  
setReactiveS3(id = "x_1", value = 10)
setReactiveS3(id = "x_2", value = function() "object-ref: {id: x_1}")

## Unset //
## --> transforms objects into non-reactive/regular objects again
unsetReactive("x_1")

## Inspect //
x_1
identical(x_2, x_1)
x_1 <- 20
x_1
x_2
## --> no reactive relationship to 'x_1' anymore; last cached value returned
isReactive("x_1")
isReactive("x_2")

## Reset referenced object again //
setReactiveS3(id = "x_1", value = 50)
x_1
x_2 == x_1
## --> reactive relationship re-established again

## Clean up //
resetRegistry()
rm(x_2)
rm(x_1)

##------------------------------------------------------------------------------  
## Strict get behavior level 1 //  
##------------------------------------------------------------------------------

setReactiveS3(id = "x_1", value = 10)
setReactiveS3(id = "x_2", value = function() "object-ref: {id: x_1}",
              strict_get = 1)
unsetReactive("x_1")
try(x_2)
## --> warning and NULL
setReactiveS3(id = "x_1", value = 50)
x_2
## --> reactive relationship restored

## Clean up //
resetRegistry()
rm(x_2)
rm(x_1)

##------------------------------------------------------------------------------  
## Strict get behavior level 2 //  
##------------------------------------------------------------------------------

setReactiveS3(id = "x_1", value = 10)
setReactiveS3(id = "x_2", value = function() "object-ref: {id: x_1}",
              strict_get = 2)
unsetReactive("x_1")
try(x_2)
## --> error
setReactiveS3(id = "x_1", value = 50)
x_2
## --> reactive relationship restored

## Clean up //
resetRegistry()
rm(x_2)
rm(x_1)

}
