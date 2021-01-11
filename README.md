![Tests](https://github.com/johannes-mueller/test-cockpit.el/workflows/Tests/badge.svg)

# test-cockpit.el

Run tests for multiple languages from emacs.


## Synopsis

This project attempts to create a user interface to run tests for different
programming languages with a consistent user interface. There are commands
common to all the supported programming languages and additional settings which
are language specific.

### Supported languages / testing environments

So far there are modules for the following languages in connection with the
indicated testing environments

* elisp – cask / ert
* python – pytest
* rust – cargo

## Planned languages

* elixir – mix


## Installation

So far there is no smoothly installable package on MELPA. So you have to put
the `*.el` files to your emacs' `loadpath`. Eventually there will be a MELPA
package when the project gets more advanced.

### Dependencies

* `projectile` as in MELPA to determine the project type
* `transient` as in MELPA for the UI

Language speciefic

* rust
  - `emacs-toml` not from MELPA but from my fork branch
    [johmue-merges](https://github.com/johannes-mueller/emacs-toml/tree/johmue-merges),
    see issues section.


## Usage

The most convenient way of running a test is to invoke the user interface with
the command `test-cockpit-dispatch`. Then a transient user interface (mostly
known from `magit`) is showing up indicating the common and the language
specific key bindings.

You can also use the following commands to run tests

* `test-cockpit-test-project` to run the whole test suite.
* `test-cockpit-test-module` runs only the tests of the current module.
* `test-cockpit-test-function` runs only the test of the function at point.
* `test-cockpit-repeat-test` repeats exactly the previous test run.

It is suggested to bind `test-cockpit-dispatch` and maybe
`test-cockpit-repeat-test` to the keybinding of your liking.


## Language specific settings

### elisp – cask / ert

* `-i` sets the `--install` option in ert, thus installs all the dependencies
  defined in the `Cask` file


### python – pytest

Planned

= `-l` run only the tests that failed in the last test

### rust – cargo

* `-d` runs only the doctests (`--doc` option)
* `-t` runs only the usual tests (`--tests` option)
* `-b` runs the benchmarks (`--bench` option)
* `-x` checks i the example build succeeds (`--examples` option)

Planned:
* `-f` toggles a feature defined in `Cargo.toml`
* `-i` run also the tests that are marked with `#[ignore]` (`--ignored` option)


The planned feature list is not complete. I will implement what turns out to be
useful.


## Status

The project just started. I will now start using it in my daily work and
develop it further. Major API changes can still happen any time


## Future

As soon as the whole thing stabilizes I will put it on MELPA. I am not yet
decided if it will be one package, or individual ones for each language that
have the core module as dependency. This would have the advantage that one does
not need to install the dependencies for a language module that one does not
want to use.


## Issues

The rust module uses `emacs-toml` in order to parse `Cargo.toml`. There are two
issues with `emacs-toml` for both of which I filed a pull request. As long as
they are not merged you need to use my fork of `emacs-toml`.


## Contributing

Ideas, issues, feature requests, PRs always welcome.
