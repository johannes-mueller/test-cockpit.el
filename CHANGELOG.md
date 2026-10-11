# test-cockpit.el's changelog

This documents the changes of every release starting of 0.1.0.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Breaking

All the `*-or-projectile-*` commands have been dropped. The functionality is
now done by other means. (Will be documented)

### Added

* Add eldev test engine for elisp
* Implement pytest cycling verbosity levels (#38)
* Issue hint what test repeat is going to do (#40)
* Support additional constant switches for test runs
* Support `uv run` to run pytest (#47)
* Support to specify python version, when uv is used (#49)
* Support to specify a specific version of a package, when uv is used
* Support preselection of switches for test commands (#42)
* Support for non persistent flags (#41)
* More flexible switch for pytest coverage report

### Fixed

* Fix test-cockpit--repeat-interactive-test function name

### Changed

* Make CI/CD fit for Emacs 31
* Make codebase Emacs 31 compliant
* Refactor test suite
* Refactor engine discovery
* Change build and CI/CD system from Cask to Eldev

## [0.1.0] – 2025-02-16

Initial release
