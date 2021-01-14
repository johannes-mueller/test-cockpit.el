(require 'test-cockpit)

(test-cockpit-register-project-type 'python-pip
				    'test-cockpit--python--test-project-command
				    'test-cockpit--python--test-module-command
				    'test-cockpit--python--test-function-command
				    'test-cockpit--python--infix)

(test-cockpit-register-project-type-alias 'python-pkg 'python-pip)

(defconst test-cockpit--python--allowed-switches
  '("--last-failed"
    "--cov-report="
    "--cov-report=term"))

(defun test-cockpit--python--test-project-command (args)
  (concat (test-cockpit--python--pytest-binary-path)
	  (test-cockpit-add-leading-space-to-switches
	   (test-cockpit--join-filter-switches
	    (test-cockpit--python--insert-no-coverage-to-switches args)
	    test-cockpit--python--allowed-switches))))

(defun test-cockpit--python--test-module-command (args)
  (concat (test-cockpit--python--pytest-binary-path)
	  (test-cockpit-add-leading-space-to-switches
	   (test-cockpit--join-filter-switches
	    (test-cockpit--python--insert-no-coverage-to-switches args)
	    test-cockpit--python--allowed-switches))
	  " -k "
	  (file-name-base (buffer-file-name))))

(defun test-cockpit--python--test-function-command (args)
  (concat (test-cockpit--python--pytest-binary-path)
	  (test-cockpit-add-leading-space-to-switches
	   (test-cockpit--join-filter-switches
	    (test-cockpit--python--insert-no-coverage-to-switches args)
	    test-cockpit--python--allowed-switches))
	  " -k "
	  (which-function)))

(defun test-cockpit--python--pytest-binary-path ()
  (if pyvenv-virtual-env
      (let ((candidate (concat (file-name-as-directory pyvenv-virtual-env) "bin/pytest")))
  	(if (file-executable-p candidate) candidate "pytest"))
    "pytest"))

(defun test-cockpit--python--insert-no-coverage-to-switches (switches)
  (if (not (seq-find (lambda (sw) (string-prefix-p "--cov-report=" sw)) switches))
      (append switches '("--cov-report="))
    switches))

(defun test-cockpit--python--infix ()
  [["Switches"
    ("-l" "only lastly failed tests" "--last-failed")
    ("-c" "print coverage report" "--cov-report=term")]])

(provide 'test-cockpit-python)

;;; test-cockpit-python.el ends here
