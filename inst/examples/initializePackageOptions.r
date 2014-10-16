\dontrun{

where <- new.env()
res <- initializePackageOptions(where = where)
exists(".registry", where, inherits = FALSE)
res <- initializePackageOptions(id = "test", where = where)
exists("test", where, inherits = FALSE)

}
