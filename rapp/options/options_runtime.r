options_runtime <- list(
	rapp_global = file.path(Sys.getenv("HOME"), "rapp"),
	runtime_mode = "dev",
	lib = .libPaths()[1],
	path_app = getwd()
)
