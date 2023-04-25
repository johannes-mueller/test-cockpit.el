;;; test-cockpit-cask.el --- The package to test cask projects in test-cockpit

;; Author: Johannes Mueller <github@johannes-mueller.org>
;; URL: https://github.com/johannes-mueller/test-cockpit.el
;; Version: 0.1.0
;; License: GPLv3
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; This is the package to test Elisp/Cask projects in test-cockpit.

;; Specific switches and settings

;;; Code:

(require 'test-cockpit)


(defclass test-cockpit-cask-engine (test-cockpit--engine) ())

(cl-defmethod test-cockpit--test-project-command ((obj test-cockpit-cask-engine))
  'test-cockpit-cask--test-project-command)

(cl-defmethod test-cockpit--test-module-command ((obj test-cockpit-cask-engine))
  'test-cockpit-cask--test-module-command )

(cl-defmethod test-cockpit--test-function-command ((obj test-cockpit-cask-engine))
  'test-cockpit-cask--test-function-command)

(cl-defmethod test-cockpit--transient-infix ((obj test-cockpit-cask-engine))
  (test-cockpit-cask--infix))

(cl-defmethod test-cockpit--engine-current-module-string ((obj test-cockpit-cask-engine))
  (when-let ((fn (buffer-file-name))) (when (string-suffix-p ".el-test.el" fn) fn)))

(cl-defmethod test-cockpit--engine-current-function-string ((obj test-cockpit-cask-engine))
  (when-let ((fn (buffer-file-name)))
    (when (string-suffix-p ".el-test.el" fn)
      (which-function))))


(test-cockpit-register-project-type 'emacs-cask 'test-cockpit-cask-engine)


(defun test-cockpit-cask--insert-install-command (command install)
  (if (equal install '"install") (concat "cask install && " command) command))

(defun test-cockpit-cask--test-project-command (_ install)
  (test-cockpit-cask--insert-install-command "cask exec ert-runner" (car install)))

(defun test-cockpit-cask--test-module-command (module install)
  (test-cockpit-cask--insert-install-command
   (concat "cask exec ert-runner " module) (car install)))

(defun test-cockpit-cask--test-function-command (func install)
  (test-cockpit-cask--insert-install-command
   (concat "cask exec ert-runner -p " func) (car install)))

(defun test-cockpit-cask--infix ()
  ["Cask specific switches"
   ("-i" "Run `cask install` before test" "install")])

(provide 'test-cockpit-cask)

;;; test-cockpit-cask.el ends here
