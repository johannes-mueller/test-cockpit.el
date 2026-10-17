# AGENTS.md

Guidance for AI agents and contributors working on test-cockpit.el.

## Project Overview

`test-cockpit.el` is a unified transient-based UI for running tests across
different programming languages and test runners. It uses projectile to detect
the project type and dispatches to a language-specific "engine" that builds the
actual test commands (e.g. cargo, mix, pytest, jest, cask, eldev). The core
commands — `test-cockpit-test-project', `test-cockpit-test-module',
`test-cockpit-test-function', and the repeat variants — are identical for all
project types; only the command builders and the transient switches differ per
language.



## Requirements

- Emacs 28.1+ (`Package-Requires` in `test-cockpit.el`).
- Dependencies: `projectile', `transient' (0.7.0+), `toml'.
- Language modules may have additional runtime dependencies (e.g. `which-func'
  for the cask/eldev modules, `dape' optionally for debug sessions).
- Development tooling: Eldev. Tests use `mocker.el' and `ert'.


## Architecture

- **Engine pattern.** The core defines the abstract class `test-cockpit--engine'.
  Each language module defines a derived engine class and implements the
  `cl-defmethod' hooks:
  - `test-cockpit--test-project-command' / `-test-module-command' /
    `-test-function-command' return a builder function (signature
    `(THING ARGS)') that produces the command string for the runner.
  - `test-cockpit--engine-current-module-string' /
    `-engine-current-function-string' detect the module/function at point.
  - `test-cockpit--transient-infix' supplies the project-type-specific
    transient switch menu.
  - `test-cockpit--engine-switch-filter' lists switches that must not be
    persisted as `last-switches' (e.g. one-shot switches like `install').
  - `test-cockpit--engine-dape-last-test-config' optionally supplies a Dape
    debug configuration.
- **Registration.** A module registers itself via
  `test-cockpit-register-project-type' (mapping a projectile project type to
  an engine class); aliases are supported via
  `test-cockpit-register-project-type-alias'.
- **Per-project state.** Engine instances are cached in
  `test-cockpit--project-engines', keyed by the projectile project root, and
  hold the "last run" state (last command, module, function, switches, args).
- **Command flow.** Public commands read transient args, call the engine's
  builder (extended by additional switches via
  `test-cockpit--additional-switches'), issue the command through `compile'
  inside `projectile-with-default-dir', and update the engine state.
- **Extensibility.** Custom actions can be attached per project type with
  `test-cockpit-add-custom-action' and
  `test-cockpit-add-dynamic-custom-action' (supporting `%P', `%F', `%f'
  placeholders); additional switches and preselected switches can be declared
  with `test-cockpit-add-additional-switch' and
  `test-cockpit-preselect-switch'.
- **Fallback.** Unsupported project types fall back to a dummy engine that
  delegates to `test-cockpit-fallback-test-command' (default:
  `projectile-test-project').


### File layout

- `test-cockpit.el` — core package: engine base class, project-type
  registration, transient prefix, public commands, custom actions.
- `test-cockpit-<runner>.el` — one file per test runner
  (`test-cockpit-cargo.el`, `test-cockpit-cask.el`, `test-cockpit-eldev.el`,
  `test-cockpit-mix.el`, `test-cockpit-npm-jest.el`,
  `test-cockpit-python.el`), each defining an engine class, the command
  builders, and the transient infix.
- `test/test-<runner>.el-test.el` — tests, one file per module, plus
  `test/test-cockpit.el-test.el` (core tests, shared fixtures) and
  `test/test-transient.el-test.el` (transient selection class tests).


## Code Conventions

### Clean code principles

- **Short, single-purpose functions** — each function does one thing.
  Target <= 15–20 lines; extract helpers when a function grows.
- **One level of abstraction per function** — public commands orchestrate
  (read input → delegate); helpers do the mechanics.
- **No duplication** — one canonical builder for arg lists / command lines
  shared by all commands and dispatch suffixes.
- **Pure helpers where possible** — parsing/formatting helpers take explicit
  arguments and return values; only stateful helpers touch globals/processes.
- Internal helpers use `test-cockpit--` pr `test-cockpit-<runner>-- `prefix;
  public functions use `test-cockpit-`.

### Emacs Lisp conventions

- Use `if-let*`/`when-let*` for nil-handling (Emacs 30.1+); never `if-let`/`when-let`.
- Docstrings: first line is a complete sentence; two spaces after periods;
  quote Lisp symbols with `` ` `` (e.g. `` `buffer-file-name' ``).
- Byte-compile must be warning-free.

## Testing

### Running tests

```sh
eldev test          # run tests
eldev compile       # byte-compile
eldev lint package-lint
eldev lint relint
eldev lint checkdoc
```

### Test conventions

- Tests interact with production code **only via public functions/variables**.
  No references to `test-cockpit--*` internals.
- Use `with-temp-git-repo` macro (defined in test file) to create real
  temporary git repos — a `.git` directory suffices for `vc-git-root` to
  detect the repo; no need to run `git init`.
- Mock external boundaries (`compile`) with `mocker.el`.

### Public API covered by tests

- `test-cockpit-register-project-type',
  `test-cockpit-register-project-type-alias'.
- `test-cockpit-test-project', `test-cockpit-test-module',
  `test-cockpit-test-function' (including last-module/last-function fallback
  behavior and the "Not in a unit test module file" message).
- `test-cockpit-repeat-module', `test-cockpit-repeat-function',
  `test-cockpit-repeat-test', `test-cockpit-repeat-interactive-test'.
- `test-cockpit-custom-test-command', `test-cockpit-dynamic-custom-test-command'
  (including `%P', `%F', `%f' placeholder handling).
- `test-cockpit-add-custom-action', `test-cockpit-add-dynamic-custom-action'
  (reflected in the transient main suffix).
- `test-cockpit-add-additional-switch', `test-cockpit-preselect-switch'.
- `test-cockpit-dispatch' and the transient main suffix structure.
- Dape integration: `test-cockpit-dape-debug-repeat-test'.
- Per-runner command builders, exercised through the public registration +
  command flow (see the `test-<runner>.el-test.el' files).
