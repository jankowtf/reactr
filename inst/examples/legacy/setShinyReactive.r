\dontrun{
  
##------------------------------------------------------------------------------  
## Bi-directional bindings //
##------------------------------------------------------------------------------

## NOTE
## To the best of my knowledge, the reactive paradigm implemented by the 
## shiny framework does not offer the possibility to define mutual reactive 
## bindings. 
## 
## Thus, something like 
##
##    "\code{x_1} has reactive binding \code{reactive{x_2 * 2}} 
##    and \code{x_2} has reactive binding \code{reactive{x_1 / 2}} 
##    where \strong{both} objects can be modified via \code{\link{<-}}" 
##
## can not be specified. 
## The reason for this is that reactivity is implemented in a direct or 
## immediate manner: whenever \code{x_1} that has a reactive binding to 
## \code{x_2} is requested, it runs its reactive binding function even though 
## \code{x_2} might not have changed at all. 
## Thus, mutual reactive bindings of the above form result in an 
## infinite recursion:

try(x_1 <- setShinyReactive("x_1", reactive(x_2 * 2)))
try(x_2 <- setShinyReactive("x_2", reactive(x_1 / 2)))

## If you would like to define mutual reactive bindings, you currently need to 
## use \code{\link[reactr]{setReactive}} as it implements a value caching 
## mechanism that allows reactive functions only to be triggered when actually
## needed, i.e. when the referenced object has actually changed.  
  
}
