context("prepareReactiveInstance_A")
test_that("prepareReactiveInstance", {

  where <- new.env()
  
  input <- reactr::Reactive.S3()
  
  expect_is(
    input <- prepareReactiveInstance(
      input = input, 
      id = "x_1",
      value = "hello world!",
      where = where
    ),
    "Reactive.S3"
  )
  expect_equal(input$uid, getReactiveUid("x_1", where))
  expect_equal(input$value, "hello world!")
  expect_equal(input$where, where)
  
  ## In hash registry //
  hash <- getHashRegistry() 
  expect_true(exists(input$uid, hash, inherits = FALSE))
  expect_equal(hash[[input$uid]]$uid, input$uid)
  expect_equal(hash[[input$uid]]$where, input$where)

})

