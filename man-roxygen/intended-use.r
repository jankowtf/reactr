#' @section Intended use of this class:
#' 
#' This S3 class, or to be more precise its constructor function, exists mainly 
#' for rapid prototyping purposes. 
#' This is mainly reflected in the fact, that when specifying \code{.x}, this 
#' constructor function will simply update the \code{class} attribute of 
#' whatever object has been provided. 
#' 
#' However, it also allows for a more formal OOP-style of rapid 
#' prototyping by offering explicit \emph{class fields} (all arguments except 
#' \code{.x}). Nevertheless, it is probably advisable to switch to an 
#' explicit formal approach such as \emph{S4} and/or \emph{Reference Classes} 
#' once the package or application has reached a certain state of maturity.
