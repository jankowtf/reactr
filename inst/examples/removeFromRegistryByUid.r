\dontrun{

## Set reactives so registry contains elements //  
id_1 <- "x_1"
id_2 <- "x_2"
where <- environment()

setReactiveS3(id = id_1, value = 10)
setReactiveS3(id = id_2, value = function() {
  .ref_1 <- get("x_1", envir = where)
})

## Comput UIDs //
uid_1 <- getObjectUid(id = id_1, where = where)
uid_2 <- getObjectUid(id = id_2, where = where)

## Inspect current state of registry //
registry <- getRegistry()
ls(registry)
exists(uid_1, registry)
exists(uid_2, registry)

## Remove //
removeFromRegistryByUid(uid = uid_1)
exists(uid_1, registry)
removeFromRegistryByUid(uid = uid_2)
exists(uid_2, registry)

}
