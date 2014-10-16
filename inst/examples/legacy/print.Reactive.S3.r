\dontrun{

test <- reactr::ReactiveObject.S3(
  value = 10,
  hash = digest::digest(10)
)
test
print(test)
print(test, as_is = FALSE)
res <- print(test, as_is = FALSE)
res
class(res)

}
