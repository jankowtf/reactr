\dontrun{

## Informal use (intended mainly for rapid prototyping) //
## Takes *any* object and simply changes the class attributes
BindingContractObserving.S3(
  list(
    id = "x_2",
    where = .GlobalEnv,
    what = "x_1",
    where_what = .GlobalEnv,
    binding = function(x) {x * 2}
  )
)  
BindingContractObserving.S3(TRUE)  

## Formal use (explicitly using 'fields') //
BindingContractObserving.S3()
BindingContractObserving.S3(
  id = "x_2",
  where = .GlobalEnv,
  what = "x_1",
  where_what = .GlobalEnv,
  binding = function(x) {x * 2}
)

## Recommended: include namespace //
## Regardless if you plan on using this class in an informal or formal way
reactr::BindingContractObserving.S3(
  id = "x_2",
  where = .GlobalEnv,
  what = "x_1",
  where_what = .GlobalEnv,
  binding = function(x) {x * 2}
)

}
