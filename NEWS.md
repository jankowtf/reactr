# CHANGES IN reactr VERSION 0.1.5

## NEW FEATURES

## BUG FIXES

- fixed: #5

## MAJOR CHANGES

## MINOR CHANGES

## MISC

-----

# CHANGES IN VERSION 0.1.4

## NEW FEATURES

## BUG FIXES

## MAJOR CHANGES

- removed package dependency: `classr`
- added: class `BindingContractObserved.S3`
- added: class `BindingContractObserving.S3`
- added: class `BindingContractMutual.S3`

## MINOR CHANGES

## MISC

-----

# CHANGES IN VERSION 0.1.3.3

## NEW FEATURES

## BUG FIXES

## MAJOR CHANGES

## MINOR CHANGES

## MISC

-----

# CHANGES IN VERSION 0.1.3.2

## NEW FEATURES

## BUG FIXES

## MAJOR CHANGES

## MINOR CHANGES

## MISC

-----

# CHANGES IN reactr VERSION 0.1.3.1

## NEW FEATURES

## BUG FIXES

- renamed: `getReactive()` --> `getReactive()`

## MAJOR CHANGES

## MINOR CHANGES

## MISC

-----

# CHANGES IN reactr VERSION 0.1.2

## NEW FEATURES

## BUG FIXES

## MAJOR CHANGES

## MINOR CHANGES

## MISC

-----

# CHANGES IN reactr VERSION 0.1.1

## NEW FEATURES

## BUG FIXES

## MAJOR CHANGES

## MINOR CHANGES

## MISC

-----

# CHANGES IN reactr VERSION 0.1.0.7

## NEW FEATURES

## BUG FIXES

- fixes #1

## MAJOR CHANGES

## MINOR CHANGES

## MISC

- auto-closing feature for issues on GitHub did not work --> trying again

-----

# CHANGES IN reactr VERSION 0.1.0.7

## NEW FEATURES

## BUG FIXES

- fixes #1

## MAJOR CHANGES

## MINOR CHANGES

## MISC

- auto-closing feature for issues on GitHub did not work --> trying again

-----

# CHANGES IN reactr VERSION 0.1.0.6

## NEW FEATURES

## BUG FIXES

- fixed issue #1

## MAJOR CHANGES

- added argument `force` in `setReactive()`: force set/re-set of binding function
  even though there might already have been one defined. Fixes 

## MINOR CHANGES

- modified `README.md`

## MISC

- trying to play around with the auto-closing feature for issues on GitHub

-----

# CHANGES IN reactr VERSION 0.1.0.5

## NEW FEATURES

## BUG FIXES

- fixed documentation in `R/setReactive_bare.r`

## MAJOR CHANGES

- changed `setValue*()` to `setReactive*()`
- changed `getValue()` to `getReactive()`

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

- added S3 bare function `setReactive_bare`
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
  `setReactive` and `getReactive` more generic
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
