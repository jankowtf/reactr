if (FALSE) {
      ## Maybe eep as a reference //
      expr=body(value)
      expr_2 = expr[[2]]
      .atomicElements <- function(expr) {
        lapply(expr, function(expr_2) {
    #       lapply(expr, function(ii) ii)
          if (is.call(expr_2)) {
            .atomicElements(expr_2)
          } else {
            expr_2  
          }
        })
      }
      els <- unlist(.atomicElements(expr = body(value)))
  
      sapply(els, is.symbol)
      
      .filterAtomicElements <- function(x) {
        idx <- which(sapply(x, is.symbol))
        if (length(idx)) {
          x <- sapply(x[idx], as.character)
          idx <- which(!x %in% c("{", "+", "*"))
          if (length(idx)) {
            x <- unique(x[idx])
            return(x)
          }
        }
      }
      .filterAtomicElements(x = els)
    }
