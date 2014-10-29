##------------------------------------------------------------------------------
context("setShinyReactive/basics")
##------------------------------------------------------------------------------

test_that("setShinyReactive", {

  skip("not finished yet")
#   require("shiny")
#   shiny:::makeFunction
  x_1 <- 10
resetRegistry()  
  reactive2(id = "x_2", x = {
    "object-ref: {id: x_1}"
    x_1 * 2
  })
  setShinyReactive(id = "x_2", value = function() {
    "object-ref: {id: x_1}"
    x_1 * 2
  })
  x_1
  x_2
  x_1 <- 20
  x_2

rm(x_1)
resetRegistry()  
# showRegistry()

  setShinyReactive(id = "x_1", value = 10)
  setShinyReactive(id = "x_2", value = function() {
    "object-ref: {id: x_1}"
    x_1 * 2
  })
  x_1
  x_2
  x_1 <- 100
  x_2

o$label
o$getValue
  expect_error(
    reactive2(id = "x_2", x = {
      "object-ref: {id: x_2}"
      x_2 * 2
    })
  )

  rmReactive("x_1")
  rmReactive("x_2")
  rm(x_1)
  rm(x_2)
  resetRegistry()
  setShinyReactive(id = "x_1", value = function() {
    "object-ref: {id: x_2}"
    x_2
  }, verbose = TRUE)
  showRegistry()
  setShinyReactive(id = "x_2", value = function() {
    "object-ref: {id: x_1}"
    x_1
  }, verbose = TRUE)
  x_1
  x_2

  x_1 <- 10
  x_2
  x_1 <- 20
  x_2

  ##----------

  rmReactive("x_1")
  rmReactive("x_2")
  
  setReactiveS3(id = "x_1", value = function() {
    "object-ref: {id: x_2}"
    x_2
  }, verbose = TRUE)
  setReactiveS3(id = "x_2", value = function() {
    "object-ref: {id: x_1}"
    x_1
  }, verbose = TRUE)
  x_1
  x_2

  x_1 <- 10
  x_2
  x_1 <- 20
  x_2

o$.func
ls(o$.refs_checksum)
ls(o$.refs_checksum)
ls(getRegistry())
  x_2
  
  inst <- Observable2$new(id = "x_2")
  inst$.uid  
  inst$.where
  inst$.computeUid()
  inst$.registerPullReferences(refs = list(x_1 = list(id = "x_1", where = environment())))
  ls(inst$.refs_pull)
  expect_true(inst$.hasPullReferences())
  expect_true(inst$.has_pull_refs)

  inst$.refs_pull$"2fc2e352f72008b90a112f096cd2d029"
  ls(inst$.registry)
  computeObjectUid("abcde")
  getRegistry()
  
})
