require("microbenchmark")

##------------------------------------------------------------------------------
## setReactive() //
##------------------------------------------------------------------------------

where <- new.env()  

res_1 <- microbenchmark(
  "1" = setReactive(id = "x_1", value = 10, where = where),
  "2" = getThis(id = "x_1", where = where),
  "3" = setReactive(id = "x_2", where = where, watch = "x_1",
    binding = function(x) {x + 100}),
  "4" = getThis(id = "x_2", where = where),
  "5" = setReactive(id = "x_1", value = 100, where = where),
  "6" = getThis(id = "x_2", where = where),
  control = list(order = "inorder")
)

##-----------

where <- new.env()

res_2 <- microbenchmark(
  "1" = setReactive(id = "x_1", value = Sys.time(), where = where,
                 binding_type = 2),
  "2" = getThis(id = "x_1", where = where),
  "3" = setReactive(id = "x_2", where = where,
    binding = substitute(function(x) {
        x + 60*60*24
      }), watch = "x_1", binding_type = 2),
  "4" = getThis(id = "x_2", where = where),
  "5" = setReactive(id = "x_1", value = Sys.time(), where = where,
                 binding_type = 2),
  "6" = getThis(id = "x_2", where = where),
  control = list(order = "inorder")
)


##------------------------------------------------------------------------------
## setReactive_bare() //
##------------------------------------------------------------------------------

where <- new.env()

res_3 <- microbenchmark(
  "1" = setReactive_bare(id = "x_1", value = 10, where = where),
  "2" = getThis(id = "x_1", where = where),
  "3" = setReactive_bare(id = "x_2", where = where, watch = "x_1",
    binding = function(x) {x + 100}),
  "4" = getThis(id = "x_2", where = where),
  "5" = setReactive_bare(id = "x_1", value = 100, where = where),
  "6" = getThis(id = "x_2", where = where),
  control = list(order = "inorder")
)

##-----------

where <- new.env()  

res_4 <- microbenchmark(
  "1" = setReactive_bare(id = "x_1", value = Sys.time(), where = where,
                 binding_type = 2),
  "2" = getThis(id = "x_1", where = where),
  "3" = setReactive_bare(id = "x_2", where = where,
    binding = substitute(function(x) {
        x + 60*60*24
      }), watch = "x_1", binding_type = 2),
  "4" = getThis(id = "x_2", where = where),
  "5" = setReactive_bare(id = "x_1", value = Sys.time(), where = where,
                 binding_type = 2),
  "6" = getThis(id = "x_2", where = where),
  control = list(order = "inorder")
)


##------------------------------------------------------------------------------
## Comparison //
##------------------------------------------------------------------------------

res_1
res_2
res_3
res_4

res_bt_1 <- lapply(list(res_1, res_3), function(ii) {
  res <- aggregate(time ~ expr, ii, function(z) c(fivenum(z), 
          length(z)))
  res <- cbind(res$expr, as.data.frame(res$time))
    colnames(res) <- c("expr", "min", "lq", "median", "uq", "max", 
        "neval")
  res
})
res_bt_2 <- lapply(list(res_2, res_4), function(ii) {
  res <- aggregate(time ~ expr, ii, function(z) c(fivenum(z), 
          length(z)))
  res <- cbind(res$expr, as.data.frame(res$time))
    colnames(res) <- c("expr", "min", "lq", "median", "uq", "max", 
        "neval")
  res
})

## Relative differences //
getRelativeBenchmarkValues <- function(
  data_list,
  cols = c("min", "lq", "median", "uq", "max")
) {
#   ii=cols[1]
  x_bench <- data_list[[1]]
#   dat=data_list[[2]]
  out <- lapply(data_list, function(dat) {
    tmp <- lapply(cols, function(ii) {
      dat[,ii] / x_bench[,ii]
    })
    tmp <- data.frame(tmp)
    colnames(tmp) <- cols
    tmp
    list(
      rel = tmp,
      rel_inverse = apply(tmp, 2, function(x) {1-x})
    )
  })
  names(out) <- paste0("data_", 1:length(out))
  out
}
rel_bt_1 <- getRelativeBenchmarkValues(data_list = res_bt_1)
rel_bt_2 <- getRelativeBenchmarkValues(data_list = res_bt_2)

rel_bt_1
rel_bt_2
