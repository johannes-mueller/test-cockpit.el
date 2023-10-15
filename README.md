[![Test](https://github.com/johannes-mueller/test-cockpit.el/workflows/Tests/badge.svg)](https://github.com/johannes-mueller/test-cockpit.el/actions/workflows/test.yml)

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
* elixir – mix
* JavaScript / TypeScript – jest

### Planned languages

* any other ... as soon as I need it or as soon as you implement it

## Installation

Easiest way to install is from [MELPA](https://melpa.org).  If you have
configured the MELPA sources you can just install the `test-cockpit`
package using the `package-install` command.

You can of course also use
[straight.el](https://github.com/raxod502/straight.el). Put the following lines
into your startup file.

``` elisp
(straight-use-package
 '(test-cockpit-<language> :type git :host github :repo "johannes-mueller/test-cockpit.el"))
```

where `<langauge>` is the programming language you need:
* `python` for python - pytest
* `cask` for elisp - cask / ert
* `cargo` for rust - cargo
* `elixir` for elixir - mix
* `npm-jest` for JavaScript / TypeScript – jest




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

### Suggested keybindings

It is suggested that you bind the following two commands to keybindings that
suit you best.

* `test-cockpit-repeat-test-or-projectile-test`
  This should be bound to a quickly reachable keybinding, that you can find
  easily and quickly. It tries to the last test that the current project
  has been tested with. If in the current session the project has not been
  tested yet, a dialog is opened for you to choose the way the testing should
  be performed.

  If the project type is not supported it falls back to
  `projectile-test-project`.

  In either way, the test command that you give is remembered. Next time you
  hit your key binding, the exact same test command for the project is
  repeated.

* `test-cockpit-test-or-projectile-test`
  This does open the test dialog for you to setup the test command. If the
  project type is not supported, it falls back to `projectile-test-command`. So
  use this if you don't want to repeat the last test, but run a different one.


### Other commands

* `test-cockpit-repeat-test-or-projectile-build`
  Like `test-cockpit-repeat-test-or-projectile-test` but only builds the
  project if the project type is not supported.

* `test-cockpit-test-or-projectile-build`
  Like `test-cockpit-test-or-projectile-test` but only builds the project if
  the project type is not supported.

* `test-cockpit-repeat-test`
  Like `test-cockpit-repeat-test-or-projectile-test` but does not fallback to
  projectile.

* `test-cockpit-dispatch`
  Like `test-cockpit-test-or-projectile-test` but does not fallback to
  projectile.


You can also use the following commands to run tests in a more manual way

* `test-cockpit-test-project` to run the whole test suite.
* `test-cockpit-test-module` runs only the tests of the current module.
* `test-cockpit-test-function` runs only the test of the function at point.
* `test-cockpit-repeat-test` repeats exactly the *previous* test run.

If the current function at point or the current module cannot be determined,
the last tested module resp. last tested function are tested.  If there are no
last tests, an error message is thrown.

## Status

The development started more than a year ago in early 2021.  Since then I have
used it for my daily work and added new features every now and then.  It turns
out to work smoothly and to be quite useful.


### Interesting feature ideas

* Test discovery
* Parsing test results to determine failed tests
* dap-mode integration – launch lastly failed test in dap-mode
* Generalizing it to a more comprehensive build-cockpit also doing simple
  builds and things like release uploads.

Check the [issues](https://github.com/johannes-mueller/test-cockpit.el/issues)
for more short term goals.

## Limitations

Lots. I implement stuff as soon as I need it and have some time.


## Contributing

Ideas, issues, feature requests, PRs always welcome.
