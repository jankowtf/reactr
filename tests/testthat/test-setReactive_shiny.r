context("setReactive_shiny-A")
test_that("setReactive_shiny", {

  ## In .GlobalEnv //
  ## Make sure 'x_1' and 'x_2' are removed:
  suppressWarnings(rm(x_1))
  suppressWarnings(rm(x_2))
  setReactive_shiny(id = "x_1", value = Sys.time())
  x_1
  x_1 <- Sys.time()
  x_1

  setReactive_shiny(id = "x_2", value = reactive(x_1 + 60*60*24))
  x_2
  x_1 <- Sys.time()
  x_1
  x_2

  setReactive_shiny("x_3", value = reactive({
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
  
  suppressWarnings(rm(x_1))
  suppressWarnings(rm(x_2))
  suppressWarnings(rm(x_3))
  
  setReactive_shiny("x_1", value = 10)
  setReactive_shiny("x_2", value = reactive(x_1 + 10))
  setReactive_shiny("x_3", value = reactive(x_1 + x_2))
  x_1
  x_2
  x_3
  x_1 <- 100
  x_2
  x_3
  
  suppressWarnings(rm(x_1))
  suppressWarnings(rm(x_2))
  suppressWarnings(rm(x_3))
  id="x_1"
  value <- reactive(x_2 + 10)
  setReactive_shiny("x_1", value = reactive(x_2))
  setReactive_shiny("x_2", value = reactive(x_1))
  x_1 <- 10
  x_1
  
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
