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


(defclass test-cockpit-cask-engine (test-cockpit--engine) ())

(cl-defmethod test-cockpit--test-project-command ((_obj test-cockpit-cask-engine))
  "Implement test-cockpit--test-project-command." 'test-cockpit-cask--test-project-command)

(cl-defmethod test-cockpit--test-module-command ((_obj test-cockpit-cask-engine))
  "Implement test-cockpit--test-module-command." 'test-cockpit-cask--test-module-command )

(cl-defmethod test-cockpit--test-function-command ((_obj test-cockpit-cask-engine))
  "Implement test-cockpit--test-function-command." 'test-cockpit-cask--test-function-command)

(cl-defmethod test-cockpit--transient-infix ((_obj test-cockpit-cask-engine))
  "Implement test-cockpit--test-infix."
  (test-cockpit-cask--infix))

(cl-defmethod test-cockpit--engine-current-module-string ((_obj test-cockpit-cask-engine))
  "Implement test-cockpit--engine-current-module-string."
  (when-let ((fn (buffer-file-name))) (when (string-suffix-p ".el-test.el" fn) fn)))

(cl-defmethod test-cockpit--engine-current-function-string ((_obj test-cockpit-cask-engine))
  "Implement test-cockpit--engine-current-function-string."
  (when-let ((fn (buffer-file-name)))
    (when (string-suffix-p "test.el" fn)
      (which-function))))


(test-cockpit-register-project-type 'emacs-cask 'test-cockpit-cask-engine)


(defun test-cockpit-cask--insert-install-command (command install)
  "Prepend \"cask install\" command to the test COMMAND if requested by INSTALL."
  (if (equal install '"install") (concat "cask install && " command) command))

(defun test-cockpit-cask--test-project-command (_ install)
  "Make the test project command according to INSTALL."
  (test-cockpit-cask--insert-install-command "cask exec ert-runner" (car install)))

(defun test-cockpit-cask--test-module-command (module install)
  "Make the test module command for MODULE according to INSTALL."
  (test-cockpit-cask--insert-install-command
   (concat "cask exec ert-runner " module) (car install)))

(defun test-cockpit-cask--test-function-command (func install)
  "Make the test module command for FUNC according to INSTALL."
  (test-cockpit-cask--insert-install-command
   (concat "cask exec ert-runner -p " func) (car install)))

(defun test-cockpit-cask--infix ()
  "Setup project type specific switch menu."
  ["Cask specific switches"
   ("-i" "Run `cask install` before test" "install")])

(provide 'test-cockpit-cask)

;;; test-cockpit-cask.el ends here
