
(require 'test-cockpit)


(defclass test-cockpit--cask-engine (test-cockpit--engine) ())

(cl-defmethod test-cockpit--test-project-command ((obj test-cockpit--cask-engine))
  'test-cockpit--cask--test-project-command)
(cl-defmethod test-cockpit--test-module-command ((obj test-cockpit--cask-engine))
  'test-cockpit--cask--test-module-command )
(cl-defmethod test-cockpit--test-function-command ((obj test-cockpit--cask-engine))
  'test-cockpit--cask--test-function-command)
(cl-defmethod test-cockpit--transient-infix ((obj test-cockpit--cask-engine))
  'test-cockpit--cask--infix)


(test-cockpit-register-project-type 'emacs-cask 'test-cockpit--cask-engine)


(defun test-cockpit--cask--insert-install-command (command install)
  (if (equal install '"install") (concat "cask install && " command) command))

(defun test-cockpit--cask--test-project-command (install)
  (test-cockpit--cask--insert-install-command "cask exec ert-runner" (car install)))

(defun test-cockpit--cask--test-module-command (install)
  (test-cockpit--cask--insert-install-command
   (concat "cask exec ert-runner " (buffer-file-name)) (car install)))

(defun test-cockpit--cask--test-function-command (install)
  (test-cockpit--cask--insert-install-command
   (concat "cask exec ert-runner -p " (which-function)) (car install)))

(defun test-cockpit--cask--infix ()
  ["Cask specific switches"
   ("-i" "Run `cask install` before test" "install")])

(provide 'test-cockpit-cask)

;;; test-cockpit-cask.el ends here
