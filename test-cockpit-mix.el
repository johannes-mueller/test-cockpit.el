;;; test-cockpit-mix.el --- The package to test Elixir/mix projects in test-cockpit -*- lexical-binding: t; -*-

;; Author: Johannes Mueller <github@johannes-mueller.org>
;; URL: https://github.com/johannes-mueller/test-cockpit.el
;; Version: 0.1.0
;; License: GPLv3
;; SPDX-License-Identifier: GPL-3.0-only

;;; Commentary:

;; test-cockpit is a unified user interface for test runners of different
;; programming languages resp. their testing tools.  This is the module for the
;; mix runner for the elixir programming language.

;;; Code:

(require 'test-cockpit)

(defclass test-cockpit-mix--engine (test-cockpit--engine) ())

(cl-defmethod test-cockpit--test-project-command ((_obj test-cockpit-mix--engine))
  "Implement test-cockpit--test-project-command." 'test-cockpit-mix--test-project-command)

(cl-defmethod test-cockpit--test-module-command ((_obj test-cockpit-mix--engine))
  "Implement test-cockpit--test-module-command." 'test-cockpit-mix--test-module-command)

(cl-defmethod test-cockpit--test-function-command ((_obj test-cockpit-mix--engine))
  "Implement test-cockpit--test-function-command." 'test-cockpit-mix--test-function-command)

(cl-defmethod test-cockpit--transient-infix ((_obj test-cockpit-mix--engine))
  "Implement test-cockpit--test-infix."
  (test-cockpit-mix--infix))

(cl-defmethod test-cockpit--engine-current-module-string ((_obj test-cockpit-mix--engine))
  "Implement test-cockpit--engine-current-module-string."
  (when-let ((fn (buffer-file-name))) (when (string-suffix-p "_test.exs" fn) fn)))

(cl-defmethod test-cockpit--engine-current-function-string ((_obj test-cockpit-mix--engine))
  "Implement test-cockpit--engine-current-function-string."
  (when-let ((fn (buffer-file-name)))
    (when (string-suffix-p "_test.exs" fn)
      (concat fn ":" (number-to-string (line-number-at-pos))))))


(test-cockpit-register-project-type 'elixir 'test-cockpit-mix--engine)

(defun test-cockpit-mix--apply-switch (command switch)
  "Apply the SWITCH to COMMAND.

Some switches are not actual switches but are realized as environment variable."
  (cond ((equal switch "reset") (concat "MIX_ENV=test mix ecto.reset && " command))
        ((equal switch "debuglog") (concat "MIX_TEST_LOGLEVEL=debug " command))
        (t (concat command " " switch))))

(defun test-cockpit-mix--apply-switches(command switches)
  "Apply all the SWITCHES to COMMAND if needed."
  (if switches
      (let ((command (test-cockpit-mix--apply-switch command (car switches))))
        (test-cockpit-mix--apply-switches command (cdr switches)))
    command))

(defun test-cockpit-mix--test-project-command (_ switches)
  "Make the test project command with SWITCHES."
  (test-cockpit-mix--apply-switches "mix test" switches))

(defun test-cockpit-mix--test-module-command (module switches)
  "Make the test module command for MODULE  with SWITCHES."
  (test-cockpit-mix--apply-switches (concat "mix test " module) switches))

(defun test-cockpit-mix--test-function-command (func switches)
  "Make the test function command for FUNC with SWITCHES."
  (test-cockpit-mix--apply-switches (concat "mix test " func) switches))

(defun test-cockpit-mix--infix ()
  "Setup project type specific infix."
  ["Mix specific switches"
   ("-r" "Reset Ecto before test" "reset")
   ("-f" "Only lastly failed tests" "--failed")
   ("-d" "Set loglevel to \"debug\"" "debuglog")
   ("-t" "Output trace" "--trace")])

(provide 'test-cockpit-mix)

;;; test-cockpit-mix.el ends here
