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
setShinyReactive(id = "x_2", value = reactiveExpression(x_1 + 60*60*24))
x_2
## --> 'x_1' + one day

## Modification of referenced object //
(x_1 <- Sys.time())
x_2
## --> updated according to reactive expression/binding
x_2
## --> cached value, no update 
## For subsequent requests of `x_2` and as long as `x_1` has not changed
## again it is safe to return the value that has been cached after the last
## update cycle was completed --> possible efficiency increase for more 
## situations where binding functions are more complex or the amount of data 
## stored in referenced objects is significantly big. 

## Clean up //
rmReactive("x_1")
rmReactive("x_2")

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
  value = reactiveExpression(where_1$x_1 + 60*60*24), 
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
rmReactive("x_1", where_1)
rmReactive("x_2", where_1)
suppressWarnings(rm(where_1))

##------------------------------------------------------------------------------  
## Mutltiple reactive bindings //
##------------------------------------------------------------------------------

x_1 <- setShinyReactive("x_1", 10)
x_2 <- setShinyReactive("x_2", 20)
x_3 <- setShinyReactive("x_3", value = reactiveExpression((x_1 + x_2) * 2))

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
rmReactive("x_1")
rmReactive("x_2")
rmReactive("x_3")

##------------------------------------------------------------------------------  
## Bi-directional bindings //
##------------------------------------------------------------------------------

## NOTE
## Currently not possible yet, or to be more precise, only when using the 
## version at GitHub branch `legacy-shinyOld`: 

setShinyReactive(id = "x_1", value = reactiveExpression(x_2))
setShinyReactive(id = "x_2", value = reactiveExpression(x_1))

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

## NOTE
## Due to some strange behavior with respect to environments, you might need
## to run this function a couple of times until no error is issued anymore!

res <- microbenchmark(
  "set/x_1/setShinyReactive" = setShinyReactive(id = "x_1", value = 10),
  "set/x_2/regular" = assign("x_2", value = 10, envir = environment()),
  "get x_1" = get("x_1", envir = environment()),
  "get x_2" = get("x_2", envir = environment()),
  "set/x_3/setShinyReactive" = setShinyReactive(
    id = "x_3", 
    value = reactiveExpression(x_1 * 2)
  ),
  "get x_3" = get("x_3", envir = environment()),
  "change x_1" = assign("x_1", 100),
  "change x_2" = assign("x_2", 100),
  "get x_3 (2)" = get("x_3", envir = environment())
)

res
# Unit: microseconds
#                      expr      min        lq       mean    median        uq      max neval
#  set/x_1/setShinyReactive 1834.791 1957.0910 1227.7340 2197.2475 2336.1305 3817.052   100
#           set/x_2/regular    1.184    2.9610    3.88546    3.5530    4.1460   30.797   100
#                   get x_1  122.004  143.6205  165.85389  164.3490  175.8980  278.950   100
#                   get x_2    1.185    2.3700    3.54782    2.9620    3.55300   18.953   100
#  set/x_3/setShinyReactive  973.067 1069.0115 1216.52330 1227.7340 1268.3040 2065.769   100
#                   get x_3  127.334  162.8690  314.86403  183.3015  511.4085  929.241   100
#                change x_1   61.595   83.8035  169.43716  162.5730  216.4675  467.285   100
#                change x_2    1.184    2.9615    3.74326    3.5540    4.1460   15.991   100
#               get x_3 (2)  122.004  147.4710  333.05206  181.5250  571.8175  935.163   100

## Costs //
## 1) Setting: simple `setShinyReactive()` compared to regular assignment:
1227.7340/3.5530
## --> about 595 times slower, but nominal time is still quite small:
1227.7340/10^9 ## in seconds
##
## 2) Setting: one-directional `setShinyReactive()` compared to regular assignment:
7660.163/3.5530
## --> about 2150 times slower, but nominal time is still quite small:
7660.163/10^9 ## in seconds
##
## 3) Update: reactive object compared to regular object:
3.5540/3.5530
## --> about the same
3.5540/10^9 ## in seconds
##
## 4) Getting: simple reactive object compared to regular object:
164.3490/3.553
## --> about 46 times slower, but nominal time is still quite small:
164.3490/10^9 ## in seconds
##
## 5) Getting: referencing reactive object compared to regular object:
183.3015/3.553
## --> about 51 times slower, but nominal time is still quite small:
183.3015/10^9 ## in seconds

##------------------------------------------------------------------------------
## Memory //
##------------------------------------------------------------------------------

## Reactive objects //
rm(list = ls(environment(), all.names = TRUE))

(memsize_1 <- memory.size(max = FALSE))
## --> total memory used before setting reactive objects

setShinyReactive(id = "x_1", value = 10)
setShinyReactive(id = "x_2", value = reactiveExpression(x_1 * 2))

(memsize_2 <- memory.size(max = FALSE))
## --> total memory used after setting reactive objects

## Difference:
memsize_2 / memsize_1
## --> about 4 % decrease (!?)

## Object sizes //
object.size(x_1)
## --> 48 bytes
object.size(x_2)
## --> 48 bytes

##----------

## Regular objects //
rm(list = ls(environment(), all.names = TRUE))

(memsize_1 <- memory.size(max = FALSE))
## --> total memory used before setting reactive objects

## Assign:
x_1 <- 10
x_2 <- x_1 * 2

(memsize_2 <- memory.size(max = FALSE))
## --> total memory used after setting reactive objects

## Difference:
memsize_2 / memsize_1
## --> about 5 % decrease (!?)

object.size(x_1)
## --> 48 bytes
object.size(x_2)
## --> 48 bytes

##------------------------------------------------------------------------------
## Microbenchmark 2: setShinyReactive vs. shiny::reactive //
##------------------------------------------------------------------------------

require("microbenchmark")

rm(list = ls(environment(), all.names = TRUE))

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
    value = reactiveExpression(x_1 * 2),
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
#                      expr      min        lq       mean    median        uq      max neval
#  set/x_1/setShinyReactive 1788.595 2009.5045 2257.01787 2197.8400 2375.8115 4083.564   100
#           set/x_2/regular   54.487   72.8470  144.48562  129.4070  205.2150  395.031   100
#   makeReactiveBinding/x_2 1389.418 1555.5450 1720.35629 1705.3845 1841.3050 3382.341   100
#                   get x_1  123.780  141.8440  167.54777  162.2765  183.3015  291.387   100
#                   get x_2  120.819  142.7325  160.70136  156.0580  173.2335  268.882   100
#  set/x_3/setShinyReactive  941.086 1046.2095 1176.58785 1180.3550 1272.7455 2578.657   100
#          set/x_4/reactive  757.488  856.0980  970.31309  935.7555 1026.9620 2525.354   100
#                   get/x_3  128.519  159.9075  317.83118  190.4085  494.2335  803.683   100
#                   get/x_4    1.185    2.3690    3.19248    2.9610    3.5540   17.768   100
#                change/x_1   59.225   73.4395  157.60986  134.1455  216.1710  438.265   100
#                change/x_2   55.671   75.2160  151.91835  130.5915  201.0690 1468.781   100
#               get/x_3 (2)  120.820  156.6505  317.46407  175.0100  523.2535 1862.626   100
#               get/x_4 (2)    1.185    2.3700    3.06225    2.9620    3.5540    4.739   100

## Costs //
## 1) Setting: simple `setShinyReactive()` compared to shiny::makeReactiveBinding:
2197.8400 / (129.4070 + 1705.3845)
## --> about 20 % slower, but nominal time is still quite small:
2197.8400/10^9 ## in seconds
##
## 2) Setting: one-directional `setShinyReactive()` compared to shiny::reactive:
1180.3550/935.7555
## --> about 26 % slower, but nominal time is still quite small:
1180.3550/10^9 ## in seconds
##
## 3) Update: reactive object compared to regular object:
134.1455/130.5915
## --> about 3 % slower, but nominal time is still quite small:
134.1455/10^9 ## in seconds
##
## 4) Getting: simple reactive object compared to shiny reactive object:
162.2765/156.0580
## --> about 4 % slower
162.2765/10^9 ## in seconds
##
## 5) Getting: referencing reactive object compared to shiny reactive object:
190.4085/2.9610
## --> about 64 times slower, but nominal time is still quite small:
190.4085/10^9 ## in seconds

}


