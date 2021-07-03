
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
(cl-defmethod test-cockpit--engine-current-module-string ((obj test-cockpit--cask-engine))
  (buffer-file-name))
(cl-defmethod test-cockpit--engine-current-function-string ((obj test-cockpit--cask-engine))
  (which-function))


(test-cockpit-register-project-type 'emacs-cask 'test-cockpit--cask-engine)


(defun test-cockpit--cask--insert-install-command (command install)
  (if (equal install '"install") (concat "cask install && " command) command))

(defun test-cockpit--cask--test-project-command (_ install)
  (test-cockpit--cask--insert-install-command "cask exec ert-runner" (car install)))

(defun test-cockpit--cask--test-module-command (module install)
  (test-cockpit--cask--insert-install-command
   (concat "cask exec ert-runner " module) (car install)))

(defun test-cockpit--cask--test-function-command (func install)
  (test-cockpit--cask--insert-install-command
   (concat "cask exec ert-runner -p " func) (car install)))

(defun test-cockpit--cask--infix ()
  ["Cask specific switches"
   ("-i" "Run `cask install` before test" "install")])

(provide 'test-cockpit-cask)

;;; test-cockpit-cask.el ends here
