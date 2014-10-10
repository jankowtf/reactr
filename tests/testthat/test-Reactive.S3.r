context("Reactive.S3_A")
test_that("Reactive.S3", {

  expect_is(Reactive.S3(), "Reactive.S3")
  expect_is(Reactive.S3(TRUE), "Reactive.S3")
  hash <- new.env()
  
  expect_is(res <- Reactive.S3(
      id = "x",
      value = 10
    ), 
    "Reactive.S3"
  )
  expect_equal(res$value, 10)
  expect_equal(res$uid, digest::digest(list(id = "x", where = .GlobalEnv)))
  
})

