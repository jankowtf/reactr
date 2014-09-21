\dontrun{

## Note //
## This is a function that is currently only used inside a call to 
## 'setValue()'
## Thus, in order to see what's going on, we need to mimick the expected 
## system state first
  
## Mimick expected system state //  
where <- new.env()
setValue(id = "test", value = Sys.time(), where = where)
watch <- "test"

## This is the binding contract that you'd normally pass to 'setValue()'
## via the argument 'binding':
binding_contract <- function(x) {
  ## Add 24 hours //
  x + 60*60*24
}
binding <- getBoilerplateCode(
  ns = classr:createClassInstance(cl = "Reactr.BindingContractGet.S3")
)

## As the binding contract is automatically renamed to '.binding' inside 
## of 'processBoilerplateCode()', we need to make sure that such a variable
## exists before evaluating the return value:
.binding <- binding_contract

## Evaluate binding contract //
eval(binding)()

## Change observed variable //
where$test <- Sys.time()
where$test

## Re-evaluate binding contract //
eval(binding)()  


}
