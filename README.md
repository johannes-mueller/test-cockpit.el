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

### Planned languages

* elixir – mix
* any other ... as soon as I need it or as soon as you implement it

## Installation

For now it's easiest to use
[straight.el](https://github.com/raxod502/straight.el).

``` elisp
(straight-use-package
 '(test-cockpit-<language> :type git :host github :repo "johannes-mueller/test-cockpit.el"))
```
where `<langauge>` is the programming language you need:
* `python` for python - pytest
* `cask` for elisp - cask / ert
* `cargo` for rust - cargo

Eventually there will be a MELPA package when the project gets more advanced.

### Dependencies

* `projectile` as in MELPA to determine the project type
* `transient` as in MELPA for the OO

Language specific

* rust
  - `emacs-toml` not from MELPA but from my fork branch
    [johmue-merges](https://github.com/johannes-mueller/emacs-toml/tree/johmue-merges),
    see issues section.
    Install it for example this way using [straight.el](https://github.com/raxod502/straight.el)
``` elisp
(straight-use-poackage '(toml :type git :host github :repo "gongo/emacs-toml"
                              :fork (:host github
                              :repo "johannes-mueller/emacs-toml"
                              :branch "johmue-merges")))
```


## Usage

The most convenient way of running a test is to invoke the user interface with
the command `test-cockpit-dispatch`. Then a transient user interface (mostly
known from `magit`) is showing up indicating the common and the language
specific key bindings. When you call `test-cockpit-dispatch` next time, all the
settings that you made will be the same you have set before in the current
project.

You can also use the following commands to run tests

* `test-cockpit-test-project` to run the whole test suite.
* `test-cockpit-test-module` runs only the tests of the current module.
* `test-cockpit-test-function` runs only the test of the function at point.
* `test-cockpit-repeat-test` repeats exactly the *previous* test run.

It is suggested to bind `test-cockpit-dispatch` and maybe
`test-cockpit-repeat-test` to the keybinding of your liking.


## Status

The project just started. I will now start using it in my daily work and
develop it further. Major API changes can still happen any time


## Future

As soon as the whole thing stabilizes I will put it on MELPA. I am not yet
decided if it will be one package, or individual ones for each language that
have the core module as dependency. This would have the advantage that one does
not need to install the dependencies for a language module that one does not
want to use.


### Interesting feature ideas

* Test discovery
* Parsing test results to determine failed tests
* dap-mode integration – launch lastly failed test in dap-mode
* Generalizing it to a more comprehensive build-cockpit also doing simple
  builds and things like release uploads.


## Issues

The rust module uses `emacs-toml` in order to parse `Cargo.toml`. There are two
issues with `emacs-toml` for both of which I filed a pull request. As long as
they are not merged you need to use my fork of `emacs-toml`.


## Limitations

Lots. I implement stuff as soon as I need it and have some time.


## Contributing

Ideas, issues, feature requests, PRs always welcome.
