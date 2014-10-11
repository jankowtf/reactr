setGeneric(
  name = "foo",
  signature = c(
    "input"
  ),
  def = function(
    input = NULL,
    id = character(),
    value = NULL,
    where = parent.frame(), ## TODO: verify this!!!
    ...
  ) {
    standardGeneric("foo")       
  }
)
setMethod(
  f = "foo", 
  signature = signature(
    input = "missing"
  ), 
  definition = function(
    input,
    value,
    where,
    ...
  ) {
  
  print(ls(where))
    
  }
)
foo()
