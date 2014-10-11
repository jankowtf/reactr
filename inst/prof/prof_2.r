# install.packages("microbenchmark")
require("microbenchmark")

x_1 <- 10
value <- reactive(x_1 + 10)
t1 <- microbenchmark(
  "without_env" = reactive_expr <- capture.output(value),
  "with_env" = reactive_expr <- gsub(") $", ", env = where)", capture.output(value)),
  "via_attrs" = reactive_expr <- attributes(value)$observable$.label
)
t2 <- microbenchmark(
  value_expr <- substitute(value <<- eval(VALUE)(), 
                           list(VALUE = parse(text = reactive_expr)))
)
t2
