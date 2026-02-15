# Transient Recursion Issue and Fix

## Problem Statement

When using `transient.el` to create interactive menus, a common pattern is to have options that depend on other options. For example, you might want to enable/disable certain menu items based on whether another option is selected.

The following code demonstrates a recursion problem:

```elisp
(defun use-extended-p ()
  (let ((args (transient-args 'extended-option-prefix)))
    (message "args: %s" args)
    (member "use-extended" args)))

(transient-define-prefix extended-option-prefix ()
  :refresh-suffixes t
  ["Options"
   ("u" "use extended options" "use-extended")
   ("e" "extension" "some-extension" :inapt-if-not use-extended-p)])
```

## Why Does This Cause Recursion?

The recursion occurs due to the interaction between three factors:

1. **`:refresh-suffixes t`** - This tells transient to re-initialize suffixes when returning to the prefix
2. **`:inapt-if-not use-extended-p`** - This calls a predicate function to determine if the suffix should be enabled
3. **`(transient-args 'extended-option-prefix)`** - This is called within the predicate

Here's what happens:

1. Transient tries to refresh/display the suffixes
2. To determine if the "extension" suffix is inapt, it calls `use-extended-p`
3. `use-extended-p` calls `transient-args` to get the current arguments
4. `transient-args` triggers the transient system to access/refresh the current state
5. This causes suffixes to be refreshed again (due to `:refresh-suffixes t`)
6. Go to step 2 → **Infinite recursion!**

## The Solution

Use `transient-get-value` instead of `transient-args` in predicate functions:

```elisp
(defun use-extended-p ()
  "Check if 'use-extended' option is enabled."
  (let ((args (transient-get-value 'extended-option-prefix)))
    (member "use-extended" args)))

(transient-define-prefix extended-option-prefix ()
  :refresh-suffixes t
  ["Options"
   ("u" "use extended options" "use-extended")
   ("e" "extension" "some-extension" :inapt-if-not use-extended-p)])
```

## When to Use Each Function

### `transient-get-value`

**Use in:** Predicate functions (`:inapt-if-not`, `:inapt-if`, `:if`, `:if-not`, etc.)

**Purpose:** Safely retrieves the current transient value within the menu context without triggering refreshes.

**Example:**
```elisp
(defun my-predicate ()
  (member "option" (transient-get-value 'my-prefix)))
```

### `transient-args`

**Use in:** Suffix command bodies (the actual functions that execute when you select an option)

**Purpose:** Retrieves the arguments that were active when the suffix was invoked.

**Example:**
```elisp
(defun my-test-command (args)
  "Run tests with ARGS."
  (interactive (list (transient-args 'test-cockpit-prefix)))
  (message "Running with args: %s" args))
```

## Best Practices

1. **Always use `transient-get-value` in predicates** - This includes any function used with `:inapt-if-not`, `:inapt-if`, `:if`, `:if-not`, `:if-nil`, `:if-non-nil`, etc.

2. **Use `transient-args` in interactive forms** - When defining suffix commands that need to know what arguments are active.

3. **Be careful with `:refresh-suffixes`** - While useful, it can cause recursion if predicates aren't written correctly.

4. **Test your transient menus** - Always test that your menus don't cause recursion, especially when using dynamic predicates.

## See Also

- [Transient Manual - Using Infix Arguments](https://docs.magit.vc/transient/Using-Infix-Arguments.html)
- [Transient Manual - Predicate Slots](https://magit.vc/manual/transient/Predicate-Slots.html)
- Test file: `test/test-recursion-fix.el-test.el` for working examples
