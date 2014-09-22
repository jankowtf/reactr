# CHANGES IN reactr VERSION 0.1.0.5

## NEW FEATURES

## BUG FIXES

- fixed documentation in `R/setThis_bare.r`

## MAJOR CHANGES

- changed `setValue*()` to `setThis*()`
- changed `getValue()` to `getThis()`

## MINOR CHANGES

## MISC

-----

# CHANGES IN reactr VERSION 0.1.0.4

## NEW FEATURES

## BUG FIXES

## MAJOR CHANGES

## MINOR CHANGES

- cleaned up a little bit, no major changes

## MISC

-----

# CHANGES IN reactr VERSION 0.1.0.3

## NEW FEATURES

## BUG FIXES

## MAJOR CHANGES

- added S3 bare function `setThis_bare`
- added argument `mutual` and implemented functionality to set mutual bindings
- added argument `where_watch` 
- added argument `.hash_id` 
- changed ID for hash environment from `".hash"` to `._HASH` in order to 
  reduce the risk of the ID already being used in `where` or `where_watch`
- made boilerplate code more concise and more general (with respect to `HASH`)
- added `.tracelevel` 

## MINOR CHANGES

## MISC

-----

# CHANGES IN reactr VERSION 0.1.0.2

## NEW FEATURES

## BUG FIXES

## MAJOR CHANGES

- argument `envir` changed to `where` in order to make the interfaces for 
  `setThis` and `getThis` more generic
- added dependencies for `classr` and `digest`
- started to implement mutual bindings (not fully functional yet)

## MINOR CHANGES

## MISC

-----

# CHANGES IN reactr VERSION 0.1.0.1

## NEW FEATURES

## BUG FIXES

## MAJOR CHANGES

## MINOR CHANGES

## MISC

- initial version

-----
