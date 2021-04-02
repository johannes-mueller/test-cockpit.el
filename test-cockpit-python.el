;;; test-cockpit.el --- test-cockpit package for python projects

;; Author: Johannes Mueller <github@johannes-mueller.org
;; URL: https://github.com/johannes-mueller/test-cockpit.el
;; License: GPLv2

;;; Commentary:

;; test-cockpit is a unified user interface for test runners of different
;; programming languages resp. their testing tools.  This is the module for the
;; pytest runner for the python programming language.

;;; Code:

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
	  " "
	  (test-cockpit--python--choose-module)
))

(defun test-cockpit--python--choose-module ()
  (let ((file-name-path (buffer-file-name)))
    (if (string-prefix-p "test_" (file-name-nondirectory file-name-path))
	(string-remove-prefix (file-name-as-directory (projectile-project-root)) file-name-path)
      (concat "-k " (file-name-base file-name-path))
    )))

(defun test-cockpit--python--test-function-command (args)
  (concat (test-cockpit--python--common-switches args)
	  " "
	  (test-cockpit--python--test-function-path)))

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

(defun test-cockpit--python--find-last-unindented-line ()
  (save-excursion
    (end-of-line)
    (if (search-backward-regexp "^\\([[:alpha:]].*\\)$" nil t)
	(match-string 1))))

(defun test-cockpit--python--find-current-test ()
  (if-let* ((unindented-line (test-cockpit--python--find-last-unindented-line))
	    (unindented-pos (match-beginning 0)))
      (if (string-match "def \\(test_[[:alpha:]][[:word:]_]*\\)" unindented-line)
	  (match-string 1 unindented-line)
	(if-let ((test-class (if (string-match "^class \\([[:alpha:]][[:word:]_]*\\)(\\(unittest\.\\)?TestCase):" unindented-line)
				 (match-string 1 unindented-line))))
	    (concat test-class
		    (save-excursion
		      (if (and (search-backward-regexp "def \\([[:alpha:]][[:word:]_]*\\)" nil t)
			       (< unindented-pos (match-beginning 0))
			       (string-prefix-p "test_" (match-string 1)))
			  (concat "::" (match-string 1)))))))))

(defun test-cockpit--python--test-function-path ()
  (concat (string-remove-prefix (file-name-as-directory (projectile-project-root)) (buffer-file-name))
	  (if-let ((test-function (test-cockpit--python--find-current-test)))
	      (concat "::" test-function))))

(provide 'test-cockpit-python)

;;; test-cockpit-python.el ends here
