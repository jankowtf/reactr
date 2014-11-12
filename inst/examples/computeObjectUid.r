\dontrun{

##------------------------------------------------------------------------------
## W/o explicit 'where' //
##------------------------------------------------------------------------------

id_1 <- "x_1"
id_2 <- "x_2"

setReactive(id = id_1, value = 10)
setReactive(id = id_2, value = function() {
  .ref_1 <- get(x = id_1)
})
x_1
x_2

uid_1 <- computeObjectUid(id = id_1)
uid_1
uid_2 <- computeObjectUid(id = id_2)
uid_2

## If these UIDs are available in the registry, they are correct //
registry <- getRegistry()
exists(uid_1, registry)
exists(uid_2, registry)

## Clean up //
rm(x_1)
rm(x_2)
resetRegistry()

##------------------------------------------------------------------------------
## With explicit 'where' //
##------------------------------------------------------------------------------

id_1 <- "x_1"
id_2 <- "x_2"
where <- new.env()

setReactive(id = id_1, value = 10, where = where)
setReactive(id = id_2, value = function() {
  .ref_1 <- get(x = id_1)
}, where = where)
where$x_1
where$x_2

uid_1 <- computeObjectUid(id = id_1, where = where)
uid_1
uid_2 <- computeObjectUid(id = id_2, where = where)
uid_2

## If these UIDs are available in the registry, they are correct //
registry <- getRegistry()
exists(uid_1, registry)
exists(uid_2, registry)

## Clean up //
rm(where)
resetRegistry()

}
