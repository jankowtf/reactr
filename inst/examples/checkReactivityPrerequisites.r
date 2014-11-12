\dontrun{

## NOTE
## This function has a strong internal character as it is mainly designed to 
## be used inside of 'setReactive()'.
## Thus not all aspects can be properly illustrated if the function is not 
## used in its default context

## Start with a fresh registry state //
resetRegistry()

suppressWarnings(rm(x_1))
input <- reactr::ReactiveObject.S3(
  id = "x_1",
  value = 10
)

## Error reason: object does not exist yet //
try(checkReactivityPrerequisites(input = input, strict = 2))

## Error reason: object exists but is non-reactive //
x_1 <- 10
try(checkReactivityPrerequisites(input = input, strict = 2))

## Error reason: reactive object already exists //
## This cannot be illustrated outside the context of 'setReactive()'

## All prerequisites met //
checkReactivityPrerequisites(input = input)

}
