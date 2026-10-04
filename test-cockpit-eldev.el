;;; test-cockpit-cask.el --- The package to test cask projects in test-cockpit -*- lexical-binding: t; package-lint-main-file: "test-cockpit.el"; -*-

;; Author: Johannes Mueller <github@johannes-mueller.org>
;; URL: https://github.com/johannes-mueller/test-cockpit.el
;; Version: 0.1.0
;; License: GPLv3
;; SPDX-License-Identifier: GPL-3.0-only

;;; Commentary:

;; test-cockpit is a unified user interface for test runners of different
;; programming languages resp. their testing tools.  This is the module for the
;; ert-runner for the Emacs Lisp programming language.

;;; Code:

(require 'test-cockpit)
(require 'which-func)


(defclass test-cockpit-eldev-engine (test-cockpit--engine) ())

(cl-defmethod test-cockpit--test-project-command ((_obj test-cockpit-eldev-engine))
  "Implement test-cockpit--test-project-command." 'test-cockpit-eldev--test-project-command)

(cl-defmethod test-cockpit--test-module-command ((_obj test-cockpit-eldev-engine))
  "Implement test-cockpit--test-module-command." 'test-cockpit-eldev--test-module-command )

(cl-defmethod test-cockpit--test-function-command ((_obj test-cockpit-eldev-engine))
  "Implement test-cockpit--test-function-command." 'test-cockpit-eldev--test-function-command)

(cl-defmethod test-cockpit--transient-infix ((_obj test-cockpit-eldev-engine))
  "Implement test-cockpit--test-infix."
  (test-cockpit-eldev--infix))

(cl-defmethod test-cockpit--engine-current-module-string ((_obj test-cockpit-eldev-engine))
  "Implement test-cockpit--engine-current-module-string."
  (when-let* ((fn (buffer-file-name))) (when (string-suffix-p ".el-test.el" fn) fn)))

(cl-defmethod test-cockpit--engine-current-function-string ((_obj test-cockpit-eldev-engine))
  "Implement test-cockpit--engine-current-function-string."
  (when-let* ((fn (buffer-file-name)))
    (when (string-suffix-p "test.el" fn)
      (which-function))))

(cl-defmethod test-cockpit--engine-switch-filter ((_obj test-cockpit-eldev-engine))
  "Filter `install' switch not to be persistent."
  '("install"))

(test-cockpit-register-project-type 'emacs-eldev 'test-cockpit-eldev-engine)

(defun test-cockpit-eldev--cli-switches (args)
  "Grab all strings from ARGS that are no cli switches."
  (string-join (seq-filter (lambda (elt) (string-prefix-p "--" elt)) args) " "))

(defun test-cockpit-eldev--setup-cli (args)
  "Setup the elddev cli according to ARGS."
  (string-join (seq-filter (lambda (elt) (length> elt 0))
                           `("eldev test -r concise"
                             ,(test-cockpit-eldev--backtraces args)
                             ,(test-cockpit-eldev--cli-switches args)))
               " "))

(defun test-cockpit-eldev--backtraces (args)
  "Honor the `backtraces' argument of ARGS."
  (if (member "backtraces" args)
      "--print-backtraces=0"
    "--omit-backtraces"))

(defun test-cockpit-eldev--test-project-command (_ args)
  "Make the test project command according to ARGS."
  (test-cockpit-eldev--setup-cli args))

(defun test-cockpit-eldev--test-module-command (module args)
  "Make the test module command for MODULE according to ARGS."
  (concat (test-cockpit-eldev--setup-cli args)
          " "
          (substring module (length (projectile-project-root)))))

(defun test-cockpit-eldev--test-function-command (func args)
  "Make the test module command for FUNC according to ARGS."
  (concat (test-cockpit-eldev--setup-cli args) " " func))

(defun test-cockpit-eldev--infix ()
  "Setup project type specific switch menu."
  [["Eldev specific switches"
    ("-b" "print backtraces" "backtraces")
    ("-x" "exit after one error" "--stop")]])

(provide 'test-cockpit-eldev)

;;; test-cockpit-eldev.el ends here
