\dontrun{

## Ensure hash for variable only //
.hash_id <- "._HASH"
where <- new.env()

ensureHashRegistryState(id = "x_1", where = where)
ls(where[[.hash_id]])
ls(where[[.hash_id]]$x_1)
where[[.hash_id]]$x_1$x_1

## Ensure hash values for variable and monitored variable //
where <- new.env()

ensureHashRegistryState(id = "x_1", watch = "x_2", where = where)
ls(where[[.hash_id]])
ls(where[[.hash_id]]$x_2)
where[[.hash_id]]$x_2$x_2
where[[.hash_id]]$x_2$x_1

}
