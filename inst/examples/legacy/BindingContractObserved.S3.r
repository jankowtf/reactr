\dontrun{

## Informal use (intended mainly for rapid prototyping) //
## Takes *any* object and simply changes the class attributes
BindingContractObserved.S3(
  list(
    where = new.env(),
    binding = function(x) {x}
  )
)  
BindingContractObserved.S3(TRUE)  

## Formal use (explicitly using 'fields') //
BindingContractObserved.S3()
BindingContractObserved.S3(
  where = new.env(),
  binding = function(x) {x}
)

## Recommended: include namespace //
## Regardless if you plan on using this class in an informal or formal way
reactr::BindingContractObserved.S3(
  where = new.env(),
  binding = function(x) {x}
)

}
