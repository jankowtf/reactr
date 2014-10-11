\dontrun{

## Start with a fresh hash registry state //
resetHashRegistry()

## Example instance //
input <- reactr::Reactive.S3()

where <- new.env()
input <- prepareReactiveInstance(
  input = input, 
  id = "x_1",
  value = "hello world!",
  where = where
)
identical(input$uid, getReactiveUid("x_1", where))
identical(input$value, "hello world!")
identical(input$where, where)

## In hash registry //
hash <- getHashRegistry() 
exists(input$uid, hash, inherits = FALSE)
identical(hash[[input$uid]]$uid, input$uid)
identical(hash[[input$uid]]$where, input$where)

}
