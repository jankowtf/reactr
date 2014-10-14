context("Reactive.S3_A")
test_that("Reactive.S3", {

  skip("manual only due to environment issues")
  expect_is(Reactive.S3(), "Reactive.S3")
  expect_is(Reactive.S3(TRUE), "Reactive.S3")
  
  expect_is(res <- Reactive.S3(
      id = "x",
      value = 10
    ), 
    "Reactive.S3"
  )
  expect_equal(res$value, 10)
  
  if (basename(getwd()) != "testthat") {
    expect_equal(res$uid, digest::digest(list(id = "x", where = res$where)))
  }
  
})

