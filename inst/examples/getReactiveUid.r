\dontrun{

## Set reactives so UIDs can be checked //
resetHashRegistry()

id_1 <- "x_1"
id_2 <- "x_2"

setReactiveS3(id = id_1, value = 10)
setReactiveS3(id = id_2, value = function() {
  .react_1 <- get(id_1, envir = where)
})

uid_1 <- getReactiveUid(id = id_1, where = where)
uid_1
uid_2 <- getReactiveUid(id = id_2, where = where)
uid_2

## If these UIDs are available in the hash registry, they are correct //
hash <- getHashRegistry()
exists(uid_1, hash)
exists(uid_2, hash)

## Clean up //
resetHashRegistry()

}
