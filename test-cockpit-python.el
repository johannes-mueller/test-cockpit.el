(require 'test-cockpit)

(defvar test-cockpit-python-build-ext-command "python setup.py build_ext --inplace"
  "The command to build the python extensions")

(test-cockpit-register-project-type 'python-pip
				    'test-cockpit--python--test-project-command
				    'test-cockpit--python--test-module-command
				    'test-cockpit--python--test-function-command
				    'test-cockpit--python--infix)

(test-cockpit-register-project-type-alias 'python-pkg 'python-pip)

(defconst test-cockpit--python--allowed-switches
  '("--last-failed"
    "--verbose"
    "--cov-report="
    "--cov-report=term"
    "-rFP"
    "--disable-warnings"))

(defun test-cockpit--python--test-project-command (args)
  (concat (test-cockpit--python--common-switches args)))

(defun test-cockpit--python--test-module-command (args)
  (concat (test-cockpit--python--common-switches args)
	  " -k "
	  (file-name-base (buffer-file-name))))

(defun test-cockpit--python--test-function-command (args)
  (concat (test-cockpit--python--common-switches args)
	  " -k "
	  (which-function)))

(defun test-cockpit--python--build-ext-command (args)
  (if (member "build_ext" args)
      (concat test-cockpit-python-build-ext-command " && ")
    ""))

(defun test-cockpit--python--common-switches (args)
  (concat (test-cockpit--python--build-ext-command args)
	  (test-cockpit--python--pytest-binary-path)
	  " --color=yes"
	  (test-cockpit-add-leading-space-to-switches
	   (test-cockpit--join-filter-switches
	    (test-cockpit--python--insert-no-coverage-to-switches args)
	    test-cockpit--python--allowed-switches))))

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
    ("-b" "build extensions before testing" "build_ext")]
   ["Output"
    ("-v" "show single tests" "--verbose")
    ("-c" "print coverage report" "--cov-report=term")
    ("-r" "report output of passed tests" "-rFP")
    ("-w" "don't output warnings" "--disable-warnings")]])

(provide 'test-cockpit-python)

;;; test-cockpit-python.el ends here
