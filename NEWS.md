# CHANGES IN reactr VERSION 0.1.16

## NEW FEATURES

## BUG FIXES

- fixed: #29
- fixed: #30

## MAJOR CHANGES

## MINOR CHANGES

## MISC

-----

# CHANGES IN reactr VERSION 0.1.15

## NEW FEATURES

- Support for reactive fields in Reference Classes and R6 Classes

## BUG FIXES

- fixed: #29

## MAJOR CHANGES

- M: `reactiveSource()`
  Refactored in order to support RC and R6
- A: `unlockEnvironment()`
  Required in order to implement reactive fields for R6 classes

## MINOR CHANGES

- M: README
- M: example for `setShinyReactive()`

## MISC

-----

# CHANGES IN reactr VERSION 0.1.14

## NEW FEATURES

- added: typed sources now use functionality of package [`type`](https://github.com/Rappster/typr)
- added: `...` in call to `reactiveSource()` in `setShinyReactive()` to pass along additional argument when `typed = TRUE`

## BUG FIXES

## MAJOR CHANGES

- added: [`type`](https://github.com/Rappster/typr) imported

## MINOR CHANGES

- modified: unit tests `test-setShinyReactive.r` and `test-reactiveSource.r`
- modified: examples `setShinyReactive.r` and `reactiveSource.r`

## MISC

-----

# CHANGES IN reactr VERSION 0.1.13

## NEW FEATURES

## BUG FIXES

- fixed: #27

## MAJOR CHANGES

## MINOR CHANGES

## MISC

-----

# CHANGES IN reactr VERSION 0.1.12

## NEW FEATURES

## BUG FIXES

## MAJOR CHANGES

- modified: `README`
- modified: example for `setShinyReactive()`

## MINOR CHANGES

## MISC

-----

# CHANGES IN reactr VERSION 0.1.11

## NEW FEATURES

## BUG FIXES

## MAJOR CHANGES

- behavior for already-in-place regular bindings in `setShinyReactive()`:
  these are now silently overwritten

## MINOR CHANGES

## MISC

-----

# CHANGES IN reactr VERSION 0.1.10

## NEW FEATURES

## BUG FIXES

- some bugfixes

## MAJOR CHANGES

## MINOR CHANGES

- some minor changes to better align it with the use of other packages

## MISC

-----

# CHANGES IN reactr VERSION 0.1.9

## NEW FEATURES

- Some new convenience functions:
  - `getBinding()`
  - `getChecksum()`
  - `showPullRefs()`
  - `showPushRefs()`

## BUG FIXES

- fixed: #25

## MAJOR CHANGES

- renamed: `removeReactive()` --> `rmReactive()`
- renamed: `removeFromRegistry()` --> `rmFromRegistry()`

## MINOR CHANGES

## MISC

----------

# CHANGES IN reactr VERSION 0.1.8

## NEW FEATURES

## BUG FIXES

## MAJOR CHANGES

## MINOR CHANGES

- modified `README.md`

## MISC

-----

# CHANGES IN reactr VERSION 0.1.7

## NEW FEATURES

## BUG FIXES

## MAJOR CHANGES

## MINOR CHANGES

- some corrections in `REAMDE.md`

## MISC

-----

# CHANGES IN reactr VERSION 0.1.6

## NEW FEATURES

- push paradigm:
  changes in object `A` can be actively propagated to all objects that are referencing `A` (argument `push` in `setReactiveS3()`)
- ensured integrity:
  Integrity of registry references ensured but can be optionally disabled
  (argument `integrity` in `setReactiveS3()`)
- added convenience function `showRegistryContent()`

## BUG FIXES

- fixed: #13
- fixed: #14

## MAJOR CHANGES

- removed a couple of functions that are no longer needed

## MINOR CHANGES

- modified: unit tests
- modified: examples
- modified: `README.md`

## MISC

-----

# CHANGES IN reactr VERSION 0.1.5

## NEW FEATURES

## BUG FIXES

- fixed: #1
- fixed: #2
- fixed: #4
- fixed: #5
- fixed: #6
- fixed: #7
- fixed: #8

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
