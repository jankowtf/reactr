## Globals //
this <- environment()
id_1 <- "x_1"
id_2 <- "x_2"

is_testthat <- basename(getwd()) == "testthat"

if (!is_testthat) {
  
##------------------------------------------------------------------------------
context("computeObjectUid/basics")
##------------------------------------------------------------------------------

test_that("computeObjectUid/no where", {
  
  resetRegistry()
  
  id_1 <- "x_1"
  id_2 <- "x_2"
  
  setReactive(id = id_1, value = 10)
  if (!is_testthat) {
    setReactive(id = id_2, value = function() {
      .ref_1 <- get(x = id_1)
    })
  } else {
    this <- this
    setReactive(id = id_2, value = function() {
      .ref_1 <- get(x = get("id_1", envir = eval(this)))
    })
  }

  uid_1 <- computeObjectUid(id = id_1)
  uid_2 <- computeObjectUid(id = id_2)
  
  registry <- getRegistry()
#   print(ls(registry))
#   print(uid_1)
#   print(uid_2)
  expect_true(exists(uid_1, registry))
  expect_true(exists(uid_2, registry))

  on.exit(resetRegistry())
  
})

test_that("computeObjectUid/where", {
  
  resetRegistry()
  
  id_1 <- "x_1"
  id_2 <- "x_2"
  
  where <- new.env()
  setReactive(id = id_1, value = 10, where = where)
  if (!is_testthat) {
    setReactive(id = id_2, value = function() {
      .ref_1 <- get(x = id_1)
    }, where = where)
  } else {
    this <- this
    setReactive(id = id_2, value = function() {
      .ref_1 <- get(x = get("id_1", envir = eval(this)))
    }, where = where)
  }
  
  uid_1 <- computeObjectUid(id = id_1, where = where)
  uid_2 <- computeObjectUid(id = id_2, where = where)
  
  registry <- getRegistry()
#   print(ls(registry))
#   print(uid_1)
#   print(uid_2)
  expect_true(exists(uid_1, registry))
  expect_true(exists(uid_2, registry))
  
  on.exit(resetRegistry())
  
})

}
