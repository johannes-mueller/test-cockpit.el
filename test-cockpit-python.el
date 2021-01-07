(require 'test-cockpit)

(test-cockpit-register-project-type 'python-pip
				    'test-cockpit--python--test-project-command
				    'test-cockpit--python--test-module-command
				    'test-cockpit--python--test-function-command
				    nil)

(test-cockpit-register-project-type-alias 'python-pkg 'python-pip)

(defun test-cockpit--python--test-project-command (args)
  (test-cockpit--python--pytest-binary-path))

(defun test-cockpit--python--test-module-command (args)
  (concat (test-cockpit--python--pytest-binary-path) " -k " (file-name-base (buffer-file-name))))

(defun test-cockpit--python--test-function-command (args)
  (concat (test-cockpit--python--pytest-binary-path) " -k " (which-function)))

(defun test-cockpit--python--pytest-binary-path ()
  (if pyvenv-virtual-env
      (let ((candidate (concat (file-name-as-directory pyvenv-virtual-env) "bin/pytest")))
  	(if (file-executable-p candidate) candidate "pytest"))
    "pytest"))

(provide 'test-cockpit-python)

;;; test-cockpit-python.el ends here
