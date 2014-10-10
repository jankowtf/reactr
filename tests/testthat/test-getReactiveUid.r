context("getReactiveUid-A")
test_that("getReactiveUid", {
  
  resetHashRegistry()
  
  if (!basename(getwd()) == "testthat") {
    
    id_1 <- "x_1"
    id_2 <- "x_2"
    
    setReactiveS3(id = id_1, value = 10)
    setReactiveS3(id = id_2, value = function() {
      .react_1 <- get(id_1, envir = where)
    })
  
    uid_1 <- getReactiveUid(id = id_1, where = where)
    uid_2 <- getReactiveUid(id = id_2, where = where)
    
    hash <- getHashRegistry()
  #   print(ls(hash))
  #   print(uid_1)
  #   print(uid_2)
    expect_true(exists(uid_1, hash))
    expect_true(exists(uid_2, hash))
  }
  
  on.exit(resetHashRegistry())
  
})
