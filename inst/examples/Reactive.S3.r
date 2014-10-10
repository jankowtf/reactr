\dontrun{

## Informal use (intended mainly for rapid prototyping) //
## Takes *any* object and simply changes the class attributes
Reactive.S3(
  list(
    id = "x_1",
    value = 10
  )
)  
Reactive.S3(TRUE)  

## Formal use (explicitly using 'fields') //
res <- Reactive.S3()
ls(res)
res <- Reactive.S3(
  id = "x_1",
  value = 10
)
res$id
res$uid
## --> automatically computed; important for handling of hash values

## Recommended: include namespace //
## Regardless if you plan on using this class in an informal or formal way
reactr::Reactive.S3(
  id = "x_1",
  value = 10
)

}
