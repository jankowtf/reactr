\dontrun{

## Informal use (intended mainly for rapid prototyping) //
## Takes *any* object and simply changes the class attributes
ReactiveObject.S3(
  list(
    .id = "x_1",
    .value = 10
  )
)  
ReactiveObject.S3(TRUE)  

## Formal use (explicitly using 'fields') //
res <- ReactiveObject.S3()
ls(res, all.names = TRUE)
res <- ReactiveObject.S3(
  .id = "x_1",
  .value = 10
)
res$.id
res$.uid
## --> automatically computed; important for handling checksum values

## Recommended: include namespace //
## Regardless if you plan on using this class in an informal or formal way
reactr::ReactiveObject.S3(
  .id = "x_1",
  .value = 10
)

##------------------------------------------------------------------------------
## Methods //
##------------------------------------------------------------------------------

obj <- reactr::ReactiveObject.S3(
  .id = "x_1",
  .value = 10
)

## Compute checksum //
digest::digest(x_1)
obj$.checksum
obj$.value <- x_1 <- 100
digest::digest(x_1)
obj$.computeChecksum()

## Compute UID //
x_1_uid <- computeObjectUid(id = "x_1")
x_1_uid
obj$.uid
obj$.computeUid()
## --> automatically executed in constructor based on 'obj$id' and 'obj$where'

## Copy //
obj$.copy(id = "x_1_copied")
x_1_copied
x_1 <- 100
x_1
x_1_copied
## --> independent
getFromRegistry("x_1")
getFromRegistry("x_1_copied")
## --> independent

## References //
obj$.hasPullReferences()
obj$.hasPushReferences()
## --> difficult to illustrate "stand-alone"; these methods are executed 
## when calling `setReactiveS3()`

## Register and unregister //
exists(obj$.uid, getRegistry())
## --> actually, the object is already registered through call to 'setReactiveS3()' 
## but we overwrite the existing value here for illustration:
obj$.register(overwrite = TRUE)

obj$.unregister()
exists(obj$.uid, getRegistry())
obj$.register()
exists(obj$.uid, getRegistry())

## Remove //
## Remove visible object from its associated environment and invisible object 
## from the registry
obj$.remove()
try(x_1)
## --> removed 

}
