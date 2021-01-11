
(require 'test-cockpit)

(test-cockpit-register-project-type 'emacs-cask
				    'test-cockpit--cask--test-project-command
				    'test-cockpit--cask--test-module-command
				    'test-cockpit--cask--test-function-command
				    'test-cockpit--cask--infix)

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
