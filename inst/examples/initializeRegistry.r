\dontrun{

where <- new.env()
res <- initializeRegistry(where = where)
exists(".registry", where, inherits = FALSE)
res <- initializeRegistry(id = "test", where = where)
exists("test", where, inherits = FALSE)

}
