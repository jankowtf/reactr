\dontrun{
  
##------------------------------------------------------------------------------  
## Set reactive objects in parent environment //
##------------------------------------------------------------------------------

## Set reactive object that can be referenced by others //
setShinyReactive(id = "x_1", value = Sys.time())
## --> 'x_1' is set in 'environment()' so you don't explicitly need to assign
## the return value of 'shinyRective()'  to 'x_1'. Of course you can also do so:
x_1
(x_1 <- setShinyReactive(id = "x_1", value = Sys.time()))
## Update reactive objects via `<-` or `assign()`
x_1 <- Sys.time()
x_1

## Set reactive object that has a reactive binding to 'x_1' //
setShinyReactive(id = "x_2", value = function() {
  "object-ref: {id: x_1, as: ref_1}"
  ref_1 + 60*60*24
})
x_2
## --> 'x_1' + one day

## Modification of referenced object //
(x_1 <- Sys.time())
x_2
## --> updated according to binding function
x_2
## --> cached value, no update 
## For subsequent requests of `x_2` and as long as `x_1` has not changed
## again it is safe to return the value that has been cached after the last
## update cycle was completed --> possible efficiency increase for more 
## situations where binding functions are more complex or the amount of data 
## stored in referenced objects is significantly big. 

## Clean up //
removeReactive("x_1")
removeReactive("x_2")

##------------------------------------------------------------------------------  
## Set reactive objects in custom environment //
##------------------------------------------------------------------------------

where_1 <- new.env()

## Set reactive object that can be referenced by others //
setShinyReactive(id = "x_1", value = Sys.time(), where = where_1)
where_1$x_1
where_1$x_1 <- Sys.time()
where_1$x_1

## Set reactive object that has a reactive binding to 'x_1' //
setShinyReactive(
  id = "x_2", 
  value = function() {
    "object-ref: {id: x_1, where: where_1, as: ref_1}"
      ref_1 + 60*60*24
  }, 
  where = where_1
)
where_1$x_2
## --> 'where_1$x_1' + one day

## Modification of referenced object //
(where_1$x_1 <- Sys.time())
where_1$x_2
## --> updated according to binding function
where_1$x_2
## --> cached value

## Clean up //
removeReactive("x_1", where_1)
removeReactive("x_2", where_1)
suppressWarnings(rm(where_1))

##------------------------------------------------------------------------------  
## Mutltiple reactive bindings //
##------------------------------------------------------------------------------

x_1 <- setShinyReactive("x_1", 10)
x_2 <- setShinyReactive("x_2", 20)
x_3 <- setShinyReactive("x_3", value = function() {
  "object-ref: {id: x_1, as: ref_1}"
  "object-ref: {id: x_2, as: ref_2}"
  (ref_1 + ref_2) * 2
})

x_1
x_2
x_3
## --> `(x_1 + x_2) * 2`

## Modifications and implications on referencees //
(x_1 <- 100)
x_3
(x_2 <- 100)
x_3

## Clean up //
removeReactive("x_1")
removeReactive("x_2")
removeReactive("x_3")

##------------------------------------------------------------------------------  
## Bi-directional bindings //
##------------------------------------------------------------------------------

setShinyReactive(id = "x_1", value = function() {
  "object-ref: {id: x_2}"
  x_2
})
setShinyReactive(id = "x_2", value = function() {
  "object-ref: {id: x_1}"
  x_1
})

## Bi-directionally referenced objects are initialized to `NULL` //
x_1
x_2

## Either one of bi-directionally referenced objects accept assignment values //
(x_1 <- 10)
x_2
## --> updated according to binding function 
x_2
## --> cached value
(x_2 <- 20)
x_1
## --> updated according to binding function 
x_1
## --> cached value

##------------------------------------------------------------------------------  
## Classes used and relations to the shiny framework //
##------------------------------------------------------------------------------

## In order to use as much of existing shiny functionality as possible for
## implementing the desired reactivity features (hiding of actual binding 
## functions, caching and bi-directinal bindings), a class `ReactiveShinyObject`. 
## Theoretically, this class **could** inherit directly from 
## `shiny::Observable` if all class components/functions were exported by shiny. 
## As this is currently not the case, an cloned version of 
## `shiny::Observable` named `ReactrObservable` is introduced which class 
## `ReactiveShinyObject` inherits from. That way, all additional functionality
## with respect to shiny's reacitivity features are encapsulated in an own 
## class.

## Instances of class `ReactiveShinyObject` provide the invisible object structure
## that powers the reactivity mechanism and is, for the most part (*),  
## in accordance with the way that reactivity is implemented by shiny. 
## The visible part only consist in the value of field `.value`, 

setShinyReactive(id = "x_1", value = 10)
(inst <- getFromRegistry("x_1"))
class(inst)
inst$.value
removeReactive("x_1")

## (*)
## The shiny framework distinguishes between instances of class `ReactiveValues`
## that are created by calling `makeReactiveBinding()` and instances of 
## class `Observable` that are created by calling `reactive()`.
## The shiny functionality used by `setShinyReactive()` does not follow this
## distinction and only uses instances of `ReactiveShinyObject()` for
## objects that **are referenced** by other objects as well as for objects 
## **referencing** other objects.
## While this is all the shiny functionality needed for the scope and 
## purpose of this package, it might mean that current instances of class 
## `ReactiveShinyObject` are not completely compatible with classic shiny 
## applications yet (not tested yet).

## See vignette `Relations to shiny` for more details.

################################################################################
## Profiling //
################################################################################

##------------------------------------------------------------------------------
## Microbenchmark 1: setShinyReactive vs. regular //
##------------------------------------------------------------------------------

require("microbenchmark")

## Session info //

# > sessionInfo()
# R version 3.1.1 (2014-07-10)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# 
# locale:
# [1] LC_COLLATE=German_Germany.1252  LC_CTYPE=German_Germany.1252   
# [3] LC_MONETARY=German_Germany.1252 LC_NUMERIC=C                   
# [5] LC_TIME=German_Germany.1252    
# 
# attached base packages:
# [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
# [1] microbenchmark_1.4-2 reactr_0.1.8         testthat_0.9        
# 
# loaded via a namespace (and not attached):
#  [1] colorspace_1.2-4    conditionr_0.1.3    devtools_1.6.0.9000 digest_0.6.4       
#  [5] ggplot2_1.0.0       grid_3.1.1          gtable_0.1.2        htmltools_0.2.6    
#  [9] httpuv_1.3.0        MASS_7.3-33         mime_0.2            munsell_0.4.2      
# [13] plyr_1.8.1          proto_0.3-10        R6_2.0              Rcpp_0.11.3        
# [17] reshape2_1.4        RJSONIO_1.3-0       scales_0.2.4        shiny_0.10.2.1     
# [21] stringr_0.6.2       tools_3.1.1         xtable_1.7-4        yaml_2.1.13        
# [25] yamlr_0.4.10    

## Making sure that all objects are removed from `.GlobalEnv`
rm(list = ls(environment(), all.names = TRUE))

resetRegistry()
object.size(getRegistry())
## --> cost of having an empty registry is 56 bytes

## NOTE:
## Due to some strange behavior with respect to environments, you might need
## to run this function a couple of times until no error is issued anymore!

res <- microbenchmark(
  "set/x_1/setShinyReactive" = setShinyReactive(id = "x_1", value = 10),
  "set/x_2/regular" = assign("x_2", value = 10, envir = environment()),
  "get x_1" = get("x_1", envir = environment()),
  "get x_2" = get("x_2", envir = environment()),
  "set/x_3/setShinyReactive" = setShinyReactive(
    id = "x_3", 
    value = function() {
      ## object-ref: {id: x_1}
      x_1 * 2
    }
  ),
  "get x_3" = get("x_3", envir = environment()),
  "change x_1" = assign("x_1", 100),
  "change x_2" = assign("x_2", 100),
  "get x_3 (2)" = get("x_3", envir = environment())
)

res
# Unit: microseconds
#                      expr      min        lq       mean   median        uq       max neval
#  set/x_1/setShinyReactive 1869.734 2010.6895 2122.40532 2122.625 2207.3165  2654.465   100
#           set/x_2/regular    1.185    2.9610    3.57160    3.554    4.1460     9.477   100
#                   get x_1   71.663   82.9150  105.00027   87.653   96.2410  1542.812   100
#                   get x_2    1.184    2.3695    3.40575    3.553    3.5540    19.544   100
#  set/x_3/setShinyReactive 6684.135 7096.6370 7685.51670 7660.163 7880.7760 10295.083   100
#                   get x_3  219.725  243.7110  407.26102  260.294  299.6790  1327.233   100
#                change x_1  183.005  202.8460  238.07289  217.060  236.0115  1779.712   100
#                change x_2    1.184    2.9610    3.70764    3.554    4.1460    19.544   100
#               get x_3 (2)  219.133  247.5610 1081.34824  263.848  310.9320 71234.060   100

## Costs //
## 1) Setting: simple `setShinyReactive()` compared to regular assignment:
2122.625/3.554
## --> about 595 times slower, but nominal time is still quite small:
2122.625/10^9 ## in seconds
##
## 2) Setting: one-directional `setShinyReactive()` compared to regular assignment:
7660.163/3.554
## --> about 2150 times slower, but nominal time is still quite small:
7660.163/10^9 ## in seconds
##
## 3) Update: reactive object compared to regular object:
217.060/3.554
## --> about 60 times slower, but nominal time is still quite small:
217.060/10^9 ## in seconds
##
## 4) Getting: simple reactive object compared to regular object:
87.653/3.553
## --> about 25 times slower, but nominal time is still quite small:
87.653/10^9 ## in seconds
##
## 5) Getting: referencing reactive object compared to regular object:
260.294/3.553
## --> about 73 times slower, but nominal time is still quite small:
260.294/10^9 ## in seconds

##------------------------------------------------------------------------------
## Memory //
##------------------------------------------------------------------------------

## Reactive objects //
rm(list = ls(environment(), all.names = TRUE))
resetRegistry()

(memsize_1 <- memory.size(max = FALSE))
## --> total memory used before setting reactive objects

setShinyReactive(id = "x_1", value = 10)
setShinyReactive(
  id = "x_2", 
  value = function() {
    ## object-ref: {id: x_1}
    x_1 * 2
  }
)

(memsize_2 <- memory.size(max = FALSE))
## --> total memory used after setting reactive objects

## Difference:
memsize_2 / memsize_1
## --> about 0,1 % increase 

## Object sizes //
object.size(getRegistry())
## --> still 56 bytes (?)
object.size(getFromRegistry("x_1"))
## --> 432 bytes
object.size(getFromRegistry("x_2"))
## --> 432 bytes

object.size(x_1)
## --> 48 bytes
object.size(x_2)
## --> 48 bytes

##----------

## Regular objects //
rm(list = ls(environment(), all.names = TRUE))
resetRegistry()

(memsize_1 <- memory.size(max = FALSE))
## --> total memory used before setting reactive objects

## Assign:
x_1 <- 10
x_2 <- x_1 * 2

(memsize_2 <- memory.size(max = FALSE))
## --> total memory used after setting reactive objects

## Difference:
memsize_2 / memsize_1
## --> about 0,01 % increase

object.size(x_1)
## --> 48 bytes
object.size(x_2)
## --> 48 bytes

##------------------------------------------------------------------------------
## Microbenchmark 2: setShinyReactive vs. regular, no cache //
##------------------------------------------------------------------------------

require("microbenchmark")

rm(list = ls(environment(), all.names = TRUE))
resetRegistry()

## NOTE:
## Due to some strange behavior with respect to environments, you might need
## to run this function a couple of times until no error is issued anymore!

res <- microbenchmark(
  "set/x_1/setShinyReactive" = setShinyReactive(id = "x_1", value = 10, cache = FALSE),
  "set/x_2/regular" = assign("x_2", value = 10, envir = environment()),
  "get x_1" = get("x_1", envir = environment()),
  "get x_2" = get("x_2", envir = environment()),
  "set/x_3/setShinyReactive" = setShinyReactive(
    id = "x_3", 
    value = function() {
      ## object-ref: {id: x_1}
      x_1 * 2
    },
    cache = FALSE
  ),
  "get x_3" = get("x_3", envir = environment()),
  "change x_1" = assign("x_1", 100),
  "change x_2" = assign("x_2", 100),
  "get x_3 (2)" = get("x_3", envir = environment())
)

res

# Unit: microseconds
#                      expr      min        lq       mean    median        uq      max neval
#  set/x_1/setShinyReactive 1880.394 2023.1265 2127.71193 2143.3530 2182.7380 2662.164   100
#           set/x_2/regular    1.185    2.3690    3.48267    3.2575    4.1460   15.398   100
#                   get x_1   34.351   39.6810   45.46727   42.3460   50.9335   74.624   100
#                   get x_2    1.185    2.3700    3.70181    3.5540    4.1460   24.875   100
#  set/x_3/setShinyReactive 6112.021 6737.4365 6982.78817 6873.0620 7058.7320 8630.268   100
#                   get x_3  617.125  710.1085  793.87002  731.1325  775.8480 2493.965   100
#                change x_1  177.676  191.8895  232.73078  207.2880  234.5310 2004.767   100
#                change x_2    1.184    2.9620    3.85584    3.5540    4.1460   18.360   100
#               get x_3 (2)  633.116  689.9710  728.38488  722.2490  745.9390  896.074   100

## Costs //
## 1) Setting: simple `setShinyReactive()` compared to regular assignment:
2143.3530/3.2575
## --> about 655 times slower, but nominal time is still quite small:
2143.3530/10^9 ## in seconds
## 1.a) Compared to enabled caching:
2122.625/2143.3530
## --> about the same
##
## 2) Setting: one-directional `setShinyReactive()` compared to regular assignment:
6873.0620/3.2575
## --> about 2100 times slower, but nominal time is still quite small:
6873.0620/10^9 ## in seconds
## 2.a) Compared to enabled caching:
7660.163/6873.0620
## --> about 11.5 % faster
##
## 3) Update: reactive object compared to regular object:
207.2880/3.5540
## --> about 60 times slower, but nominal time is still quite small:
207.2880/10^9 ## in seconds
## 3.a) Compared to enabled caching:
217.060/207.2880
## --> about 5 % faster
##
## 4) Getting: simple reactive object compared to regular object:
42.3460/3.5540
## --> about 12 times slower, but nominal time is still quite small:
42.3460/10^9 ## in seconds
## 4.a) Compared to enabled caching:
87.653/42.3460
## --> about 2 times faster
##
## 5) Getting: referencing reactive object compared to regular object:
731.1325/3.5540
## --> about 200 times slower, but nominal time is still quite small:
731.1325/10^9 ## in seconds
## 5.a) Compared to enabled caching:
260.294/731.1325
## --> about 65 % slower (!?)

##------------------------------------------------------------------------------
## Microbenchmark 2: setShinyReactive vs. shiny::reactive //
##------------------------------------------------------------------------------

require("microbenchmark")

rm(list = ls(environment(), all.names = TRUE))
resetRegistry()

## NOTE:
## Due to some strange behavior with respect to environments, you might need
## to run this function a couple of times until no error is issued anymore!

res <- microbenchmark(
  "set/x_1/setShinyReactive" = setShinyReactive(id = "x_1", value = 10, where = environment()),
  "set/x_2/regular" = assign("x_2", value = 10, envir = environment()),
  "makeReactiveBinding/x_2" = shiny::makeReactiveBinding("x_2"),
  "get x_1" = get("x_1", envir = environment()),
  "get x_2" = get("x_2", envir = environment()),
  "set/x_3/setShinyReactive" = setShinyReactive(
    id = "x_3", 
    value = function() {
      ## object-ref: {id: x_1}
      x_1 * 2
    },
    where = environment()
  ),
  "set/x_4/reactive" = assign("x_4", shiny::reactive(x_2 * 2), envir = environment()),
  "get/x_3" = get("x_3", envir = environment()),
  "get/x_4" = get("x_4", envir = environment()),
  "change/x_1" = assign("x_1", 100),
  "change/x_2" = assign("x_2", 100),
  "get/x_3 (2)" = get("x_3", envir = environment()),
  "get/x_4 (2)" = get("x_4", envir = environment())
)

res

# Unit: microseconds
#                      expr      min        lq       mean    median        uq       max neval
#  set/x_1/setShinyReactive 1900.531 2111.9635 2273.89114 2185.1070 2302.6685  3993.542   100
#           set/x_2/regular   55.672   68.4050  128.84423  106.6055  180.9325   268.290   100
#   makeReactiveBinding/x_2 1289.921 1446.8665 1510.57540 1506.0920 1571.5355  1796.294   100
#                   get x_1   72.255   88.2450   99.82401   94.1680  105.4210   158.131   100
#                   get x_2  122.003  132.6645  146.61764  141.2520  153.3930   226.832   100
#  set/x_3/setShinyReactive 6956.569 7792.8265 8109.08802 7984.7155 8228.7225 10914.576   100
#          set/x_4/reactive  716.623  799.5380  872.23658  852.2480  905.8470  2438.294   100
#                   get/x_3  235.715  268.5860  415.75391  285.4650  326.6265  1217.666   100
#                   get/x_4    1.777    2.9610    3.28730    2.9620    3.5540    18.360   100
#                change/x_1  181.229  206.1035  226.09762  223.2785  239.2690   319.223   100
#                change/x_2   55.080   68.7010  144.05921  123.1885  180.3400  1722.856   100
#               get/x_3 (2)  222.686  261.4790  437.66713  275.9890  328.1070  1315.387   100
#               get/x_4 (2)    1.185    2.6655    3.20434    3.5530    3.5540     5.923   100

## Costs //
## 1) Setting: simple `setShinyReactive()` compared to shiny::makeReactiveBinding:
2185.1070 / (106.6055 + 1506.0920)
## --> about 35 % slower, but nominal time is still quite small:
2185.1070/10^9 ## in seconds
##
## 2) Setting: one-directional `setShinyReactive()` compared to shiny::reactive:
7984.7155/852.2480
## --> about 9 times slower, but nominal time is still quite small:
7984.7155/10^9 ## in seconds
##
## 3) Update: reactive object compared to regular object:
223.2785/123.1885
## --> about 80 % times slower, but nominal time is still quite small:
223.2785/10^9 ## in seconds
##
## 4) Getting: simple reactive object compared to shiny reactive object:
94.1680/141.2520
## --> about 35 % faster
94.1680/10^9 ## in seconds
##
## 5) Getting: referencing reactive object compared to shiny reactive object:
285.4650/2.9620
## --> about 95 times slower, but nominal time is still quite small:
285.4650/10^9 ## in seconds

}


