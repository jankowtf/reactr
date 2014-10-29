library(shiny)
options(shiny.suppressMissingContextError=TRUE)

makeReactiveBinding("x_1")
x_1 <- Sys.time()
x_2 <- reactive(x_1 + 60*60*24)
x_1
x_2()
x_1 <- Sys.time()
x_1
x_2()
x_2 <- reactive(x_1 + 60*60*24*2)

# Now let's try an observer
shiny:::setAutoflush(TRUE)
observe(print(paste("The time changed:", x_1)))
x_1 <- Sys.time()
x_2()
##------------------------------------------------------------------------------

makeReactiveBinding("a")
values <- reactiveValues(value = 10)
values$value

.subset2(values, "impl")$.allValuesDeps
ls(.subset2(values, "impl")$.allValuesDeps)
.subset2(values, "impl")$.allValuesDeps$initialize
.subset2(values, "impl")$.allValuesDeps$invalidate
.subset2(values, "impl")$.allValuesDeps$.register
.subset2(values, "impl")$.allValuesDeps$self
ls(.subset2(values, "impl")$.allValuesDeps$self)
.subset2(values, "impl")$mset
.subset2(values, "impl")$.dependents
ls(.subset2(values, "impl")$.dependents)
.subset2(values, "impl")$.label
.subset2(values, "impl")$.nameDeps
.subset2(values, "impl")$.setLabel
.subset2(values, "impl")$.values
ls(.subset2(values, "impl")$.values)
.subset2(values, "impl")$.valuesDeps
ls(.subset2(values, "impl")$.valuesDeps)
.subset2(values, "impl")$get
.subset2(values, "impl")$initialize
.subset2(values, "impl")$mset
.subset2(values, "impl")$names
.subset2(values, "impl")$self
.subset2(values, "impl")$set
.subset2(values, "impl")$toList

b <- reactive(a * -1)
observe(print(b()))
a <- 20

rm(x_1)
rm(x_2)
quote(reactive(x_2))
x = quote(x_1)
env = parent.frame()
quoted = TRUE
label = NULL
domain = getDefaultReactiveDomain()
reactive2 <- function (x, env = parent.frame(), quoted = FALSE, label = NULL, 
    domain = getDefaultReactiveDomain()) 
{
    fun <- exprToFunction(x, env, quoted)
    if (is.null(label)) 
        label <- sprintf("reactive(%s)", paste(deparse(body(fun)), 
            collapse = "\n"))
    srcref <- attr(substitute(x), "srcref")
    if (length(srcref) >= 2) 
        attr(label, "srcref") <- srcref[[2]]
    attr(label, "srcfile") <- shiny:::srcFileOfRef(srcref[[1]])
    o <- shiny:::Observable$new(fun, label, domain)
    shiny:::registerDebugHook(".func", o, "Reactive")
#     structure(o$getValue, observable = o, class = "reactive")
    structure(o$getValue, observable = o, class = "reactive2")
}

x_1 <- 10
x_2 <- reactive2(x_1)
x_1
x_2()
x_1 <- 20
x_2()
makeReactiveBinding("x_1")
x_2 <- reactive2(x_1)
x_2()
x_1 <- 30
x_2()
x_2
x_1 <- 100
function () 
{
    this <- attributes(x_2)$observable
    
#     ls(this$.dependents)
#     shiny:::Map$new
#     this$.dependents$initialize()
    this$.dependents$.register()
    if (this$.invalidated || this$.running) {
        this$self$.updateValue()
    }
    ## This somehow has to do with some lookup/comparison of depending and
    ## dependee variable via IDs
    shiny:::.graphDependsOnId(
      shiny:::getCurrentContext()$id, 
      this$.mostRecentCtxId
    )
    if (identical(class(this$.value), "try-error")) 
        stop(attr(this$.value, "condition"))
    if (this$.visible) 
        .value
    else invisible(this$.value)
}

################################################################################
################################################################################
################################################################################

## OLD //

context("setReactive_reactive-1")
test_that("setReactive_reactive", {

  if (FALSE) {
  require("shiny")
  tracelevel <- 0
  where <- new.env()
  
  setReactive_reactive(id = "x_1", value = Sys.time(), where = where)
  id <- "x_2"
  watch <- "x_1"
  
  reactive <- function(
    x, 
    env = parent.frame(), 
    quoted = FALSE, 
    label = NULL,
    domain = getDefaultReactiveDomain()
  ) {
    fun <- exprToFunction(x, env, quoted)
    # Attach a label and a reference to the original user source for debugging
    if (is.null(label))
      label <- sprintf('reactive(%s)', paste(deparse(body(fun)), collapse='\n'))
    srcref <- attr(substitute(x), "srcref")
    if (length(srcref) >= 2) attr(label, "srcref") <- srcref[[2]]
    attr(label, "srcfile") <- shiny:::srcFileOfRef(srcref[[1]])
    o <- shiny:::Observable$new(fun, label, domain)
#     shiny:::registerDebugHook(".func", o, "Reactive")
    structure(o$getValue, observable = o, class = "reactive")
  }
  x = where$x_1
  env = environment()
  reactive(x_1 + 60*60*24)
  binding <- reactive(x_1 + 60*60*24)
  attributes(binding)

  value = NULL
#   where = .GlobalEnv
#   watch = character()
#   binding = substitute(expression())
  binding_type = 1
  mutual = FALSE
  force = FALSE
  where_watch = where
  .hash_id = "._HASH"
  .tracelevel = 0

  setReactive_reactive(id = "x_2", watch = "x_1", binding = reactive(x_1 + 60*60*24), 
                   where = where)

  getBoilerplateCode(ns = reactr::BindingContractMutual.S3())

  options(shiny.suppressMissingContextError=TRUE)
  binding <- reactive(x_1 + 60*60*24)
  makeReactiveBinding("x_1")
  x_1 <- Sys.time()
  rm("x_2")
  makeActiveBinding("x_2", 
    local({
      value <- NULL
      function(v) {
          if (!missing(v)) {
              value <<- v
          } else {
#               value <<- reactive(x_1 + 60*60*24)()
              value <<- binding()
          }
          value
      }
    }),
    env = .GlobalEnv
  )
  x_1
  x_2
  x_1 <- Sys.time()
  x_1
  x_2

  where <- new.env()
  makeReactiveBinding("x_1", env = where)
  where$x_1 <- Sys.time()
  rm(list = "x_2", envir = where)
  binding <- reactive(x_1 + 60*60*24, env = where)
  makeActiveBinding("x_2", 
    local({
      value <- NULL
      function(v) {
          if (!missing(v)) {
              value <<- v
          } else {
#               value <<- reactive(x_1 + 60*60*24)()
              value <<- binding()
          }
          value
      }
    }),
    env = where
  )
  where$x_1
  where$x_2
  where$x_1 <- Sys.time()
  where$x_1
  where$x_2

  makeReactiveBinding("x_1")
  makeReactiveBinding("x_2")
  x_1 <- 100
  x_2 <- 50
  rm("x_3")
  binding <- reactive((x_1 + x_2) * 2)
  makeActiveBinding("x_3", 
    local({
      value <- NULL
      function(v) {
          if (!missing(v)) {
              value <<- v
          } else {
#               value <<- reactive(x_1 + 60*60*24)()
              value <<- binding()
          }
          value
      }
    }),
    env = .GlobalEnv
  )
  x_1
  x_2
  x_3
#   x_2 <- reactive(x_1 + 60*60*24)
#   x_2()
  x_2
  x_1 <- Sys.time()
  x_1
  x_2

  ##--------------------------------------------------------------------------------

  setReactive <- function(
    id = id,
    value = NULL,
    where = .GlobalEnv,
    .tracelevel = 0,
    ...
  ) {
    ## Ensure shiny let's me do this //
    shiny_opt <- getOption("shiny.suppressMissingContextError")
    if (is.null(shiny_opt) || !shiny_opt) {
      options(shiny.suppressMissingContextError = TRUE)  
    }
    
    ## Check if regular value assignment or reactive function //
    if (!inherits(value, "reactive")) {
      is_reactive <- FALSE
      shiny::makeReactiveBinding(symbol = id, env = where)
  #       assign(id, value, where)
      value_expr <- substitute(VALUE, list(VALUE = value))
    } else {
      is_reactive <- TRUE
      ## Put together the "line of lines" //
      value_expr <- substitute(value <<- VALUE(), list(VALUE = value))
      ## --> works initially but seems to be static
      ## --> seems like the call to 'local()' needs to contain the *actual*
      ## "literate" version of 'reactive(...)'. Evaluation the line above 
      ## results in the reactive object "behind" 'reactive(()' to be assigned
      ## and that seems to make it static.
      
      ## Workaround based character strings and re-parsing //
      reactive_expr <- gsub(") $", ", env = where)", capture.output(value))
      value_expr <- substitute(value <<- eval(VALUE)(), 
                               list(VALUE = parse(text = reactive_expr)))
    }
  
    ## Call to 'makeActiveBinding' //
    expr <- substitute(
      makeActiveBinding(
        id,
        local({
          value <- VALUE
          function(v) {
            if (!missing(v)) {
                value <<- v
            } else {
                VALUE_EXPR
            }
            value
          }
        }),
        env = where
      ),
      list(
        VALUE = value,
        VALUE_EXPR = value_expr
       )
    )
    if (.tracelevel == 1) {
      print(expr)
    }
    eval(expr)
  
    ## Return value //
    if (is_reactive) {
      out <- get(id, envir = where, inherits = FALSE)
    } else {
      out <- value
    }
    return(out)
  }

  require("shiny")

  ## In .GlobalEnv //
  ## Make sure 'x_1' and 'x_2' are removed:
  suppressWarnings(rm(x_1))
  suppressWarnings(rm(x_2))
  setReactive(id = "x_1", value = Sys.time())
  x_1
  x_1 <- Sys.time()
  x_1

  setReactive(id = "x_2", value = reactive(x_1 + 60*60*24))
  x_2
  x_1 <- Sys.time()
  x_1
  x_2
# [1] "2014-09-25 18:35:51 CEST"
# > x_2
# [1] "2014-09-25 18:35:51 CEST"
# > x_1 <- Sys.time()
# > x_1
# [1] "2014-09-24 18:36:47 CEST"
# > x_2
# [1] "2014-09-25 18:36:47 CEST"

setReactive("x_3", value = reactive({
  message(x_1)
  message(x_2)
  out <- x_2 + 60*60*24
  message(paste0("Difference: ", out - x_1))
  out
}))
x_3
x_1 <- Sys.time()
x_1
x_2
x_3
x_2 <- 100
x_2
x_3

  ## Mutual bindings //
  prepareReactive <- function(
    id = id,
    value = NULL,
    where = .GlobalEnv,
    .tracelevel = 0,
    ...
  ) {
    ## Ensure shiny let's me do this //
    shiny_opt <- getOption("shiny.suppressMissingContextError")
    if (is.null(shiny_opt) || !shiny_opt) {
      options(shiny.suppressMissingContextError = TRUE)  
    }
    
    ## Check if regular value assignment or reactive function //
    if (!inherits(value, "reactive")) {
      is_reactive <- FALSE
      shiny::makeReactiveBinding(symbol = id, env = where)
      assign(id, value, where)
      value_expr <- substitute(VALUE, list(VALUE = value))
    } else {
      is_reactive <- TRUE
      ## Put together the "line of lines" //
#       value_expr <- substitute(value <<- VALUE(), list(VALUE = value))
      ## --> works initially but seems to be static
      ## --> seems like the call to 'local()' needs to contain the *actual*
      ## "literate" version of 'reactive(...)'. Evaluation the line above 
      ## results in the reactive object "behind" 'reactive(()' to be assigned
      ## and that seems to make it static.
      
      ## Workaround based character strings and re-parsing //
      reactive_expr <- gsub(") $", ", env = where)", capture.output(value))
      value_expr <- substitute(eval(VALUE)(), 
                               list(VALUE = parse(text = reactive_expr)))
    }
  
    ## Call to 'makeActiveBinding' //
    expr <- substitute(
      makeActiveBinding(
        ID,
        local({
          value <- VALUE
          function(v) {
            if (!missing(v)) {
                value <<- v
            } else {
                VALUE_EXPR
            }
            value
          }
        }),
        env = WHERE
      ),
      list(
        ID = id,
        VALUE = value,
        VALUE_EXPR = value_expr,
        WHERE = where
       )
    )
    if (.tracelevel == 1) {
      print(expr)
    }
    assign(id, expr, envir = where)

    return(NULL)
  }

  suppressWarnings(rm(x_1))
  suppressWarnings(rm(x_2))
  suppressWarnings(rm(x_3))
  prepareReactive("x_1", value = reactive(x_2), .tracelevel = 1)
  x_1
  prepareReactive("x_2", value = reactive(x_1), .tracelevel = 1)
  x_2

  id = "x_1"
  where = .GlobalEnv
  activateReactive <- function(id, where = .GlobalEnv) {
    tmp <- get(id, envir = where, inherits = FALSE)
    rm(list = id, envir = where, inherits = FALSE)
    eval(tmp)
  }
  activateReactive(id = "x_1")
  activateReactive(id = "x_2")
  x_1 <- eval(x_1)
  x_1 <- Sys.time()
  x_1()
  
  x_2
  x_1 <- Sys.time()
  x_1
  x_2

  ## In custom environment //
  where <- new.env()
  suppressWarnings(rm(x_1, envir = where))
  suppressWarnings(rm(x_2, envir = where))
  
  setReactive("x_1", value = Sys.time(), where = where)
  where$x_1
  where$x_1 <- Sys.time()
  where$x_1

  setReactive("x_2", value = reactive(x_1 + 60*60*24, env = where), where = where)
  where$x_2
  where$x_1 <- Sys.time()
  where$x_1
  where$x_2
  
  ## Stackoverflow //

  ## Where to ensure this?
  getOption("shiny.suppressMissingContextError")
  if ("microbenchmark" %in% .packages(all.available = TRUE)) {
    require("microbenchmark")
    res <- microbenchmark(getOption("shiny.suppressMissingContextError"))
    in_secs <- median(res$time)/1000000000
    in_secs
    ## --> doesn't cost me much, so okay to check each time instead of only once 
    ## globaly
  }

  func <- attributes(reactive(x_1 + 60*60*24))$observable$.func
  func
  where <- new.env()
  value <- reactive(x_1 + 60*60*24, env = where)
  func <- attributes(value)$observable$.func
  class(func)
  environment(func)
  norm <- function(x) sqrt(x%*%x)
  formals(norm)
  reactive_expr <- gsub(") $", ", env = where)", capture.output(value))
# function () 
# x_1 + 60 * 60 * 24
# attr(,"_rs_shinyDebugPtr")
# <pointer: 0x0000000008930380>
# attr(,"_rs_shinyDebugId")
# [1] 858
# attr(,"_rs_shinyDebugLabel")
# [1] "Reactive"  

  options(shiny.suppressMissingContextError = TRUE)
  where_1 <- new.env()
  where_2 <- new.env()
  where_1$x_1 <- 10
  where_2$x_2 <- 20
  value <- reactive((x_1 + x_2)*2, env = list(where_2, where_2))
  x_3 <- value
  x_3()
  }

})

b <- reactive(a * -1)
observe(print(b()))
b()
a <- 20
