# test-cockpit.el's changelog

This documents the changes of every release starting of 0.1.0.

## Unstable

### New features

* Implement pytest cycling verbosity levels (#38)
* Issue hint what test repeat is going to do (#40)
* Support additional constant switches for test runs
* Support `uv run` to run pytest (#47)
* Support preselection of switches for test commands (#42)
* Support for non persistent flags (#41)

### Bug fixes

* Fix test-cockpit--repeat-interactive-test function name

### Documentation

* Add documentation about transient recursion issue with `:inapt-if-not` predicates
* Explain proper usage of `transient-get-value` vs `transient-args` in predicates
* Add comprehensive guide in TRANSIENT-RECURSION-FIX.md

### Other changes

* Make CI/CD fit for Emacs 31
* Make codebase Emacs 31 compliatn
* Refactor test suite
* Refactor engine discovery


## Release 0.1.0

Initial release
