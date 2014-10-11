\dontrun{

## Set reactives so hash registry contains elements //  
id_1 <- "x_1"
id_2 <- "x_2"
where <- environment()

setReactiveS3(id = id_1, value = 10)
setReactiveS3(id = id_2, value = function() {
  .ref_1 <- get("x_1", envir = where)
})

## Comput UIDs //
uid_1 <- getReactiveUid(id = id_1, where = where)
uid_2 <- getReactiveUid(id = id_2, where = where)

## Inspect current state of hash registry //
hash <- getHashRegistry()
ls(hash)
exists(uid_1, hash)
exists(uid_2, hash)

## Remove //
removeFromHashRegistryByUid(uid = uid_1)
exists(uid_1, hash)
removeFromHashRegistryByUid(uid = uid_2)
exists(uid_2, hash)

}
