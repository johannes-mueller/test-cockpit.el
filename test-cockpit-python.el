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

(defclass test-cockpit--python-engine (test-cockpit--engine) ())

(cl-defmethod test-cockpit--test-project-command ((obj test-cockpit--python-engine))
  'test-cockpit--python--test-project-command)
(cl-defmethod test-cockpit--test-module-command ((obj test-cockpit--python-engine))
  'test-cockpit--python--test-module-command)
(cl-defmethod test-cockpit--test-function-command ((obj test-cockpit--python-engine))
  'test-cockpit--python--test-function-command)
(cl-defmethod test-cockpit--transient-infix ((obj test-cockpit--python-engine))
  (test-cockpit--python--infix))
(cl-defmethod test-cockpit--engine-current-module-string ((obj test-cockpit--python-engine))
  (test-cockpit--python--choose-module))
(cl-defmethod test-cockpit--engine-current-function-string ((obj test-cockpit--python-engine))
  (test-cockpit--python--test-function-path))

(test-cockpit-register-project-type 'python-pip 'test-cockpit--python-engine)
(test-cockpit-register-project-type-alias 'python-pkg 'python-pip)
(test-cockpit-register-project-type-alias 'python-tox 'python-pip)

(defconst test-cockpit--python--allowed-switches
  '("--last-failed"
    "--verbose"
    "--cov-report="
    "--cov-report=term"
    "-rFP"
    "--disable-warnings"
    "--capture=no"
    "-k"
    "-m"))

(defun test-cockpit--python--test-project-command (_ args)
  (concat (test-cockpit--python--common-switches args)))

(defun test-cockpit--python--test-module-command (string args)
  (concat (test-cockpit--python--common-switches args) " " string))

(defun test-cockpit--python--choose-module ()
  (if-let ((file-name-path (buffer-file-name)))
    (if (string-prefix-p "test_" (file-name-nondirectory file-name-path))
	(test-cockpit--strip-project-root file-name-path)
      (concat "-k " (file-name-base file-name-path))
    )))

(defun test-cockpit--python--test-function-command (string args)
  (concat (test-cockpit--python--common-switches args) " " string))

(defun test-cockpit--python--build-ext-command (args)
  (if (member "build_ext" args)
      (concat test-cockpit-python-build-ext-command " && ")
    ""))

(defun test-cockpit--python--common-switches (args)
  (concat (test-cockpit--python--build-ext-command args)
	  (test-cockpit--python--pytest-binary-path)
	  " --color=yes"
	  (test-cockpit--add-leading-space-to-switches
	   (test-cockpit--join-filter-switches
	    (test-cockpit--python--insert-no-coverage-to-switches args)
	    test-cockpit--python--allowed-switches))))

(defun test-cockpit--python--pytest-binary-path () "pytest")

(defun test-cockpit--python--insert-no-coverage-to-switches (switches)
  (if (not (seq-find (lambda (sw) (string-prefix-p "--cov-report=" sw)) switches))
      (append switches '("--cov-report="))
    switches))

(transient-define-argument test-cockpit--python--restrict-substring ()
  :description "Restrict to tests matching string"
  :class 'transient-option
  :key "-k"
  :argument "-k")

(transient-define-argument test-cockpit--python--marker-switch ()
  :description "Add marker switch "
  :class 'transient-option
  :key "-m"
  :argument "-m")

(defun test-cockpit--python--infix ()
  [["Switches"
    ("-k" test-cockpit--python--restrict-substring)
    ("-l" "only lastly failed tests" "--last-failed")
    ("-b" "build extensions before testing" "build_ext")
    ("-m" test-cockpit--python--marker-switch)]
   ["Output"
    ("-v" "show single tests" "--verbose")
    ("-c" "print coverage report" "--cov-report=term")
    ("-r" "report output of passed tests" "-rFP")
    ("-w" "don't output warnings" "--disable-warnings")
    ("-n" "don't capture output" "--capture=no")]])

(defun test-cockpit--python--find-last-unindented-line ()
  (save-excursion
    (end-of-line)
    (if (search-backward-regexp "^\\([[:alpha:]].*\\)$" nil t)
	(match-string 1))))

(defun test-cockpit--python--test-function-in-line (line)
  (if (string-match "def[[:space:]]+\\(test_[[:word:]_]*\\)" line)
      (match-string 1 line)))

(defun test-cockpit--python--class-in-line (line)
  (if (string-match "^class[[:space:]]+\\(Test[[:word:]_]*\\)\\((.*)\\)?:" line)
      (match-string 1 line)))

(defun test-cockpit--python--maybe-test-method (line pos)
  (save-excursion
    (if (and (search-backward-regexp "def[[:space:]]+\\([[:alpha:]][[:word:]_]*\\)" nil t)
	     (< pos (match-beginning 0))
	     (string-prefix-p "test_" (match-string 1)))
	(concat "::" (match-string 1)))))

(defun test-cockpit--python--test-method-or-class (line pos)
  (if-let ((test-class (test-cockpit--python--class-in-line line)))
      (concat test-class
	      (test-cockpit--python--maybe-test-method line pos))))

(defun test-cockpit--python--find-current-test ()
  (if-let* ((unindented-line (test-cockpit--python--find-last-unindented-line))
	    (unindented-pos (match-beginning 0)))
      (or (test-cockpit--python--test-function-in-line unindented-line)
	  (test-cockpit--python--test-method-or-class unindented-line unindented-pos))))

(defun test-cockpit--python--test-function-path ()
  (if-let ((file-name (buffer-file-name)))
      (concat (test-cockpit--strip-project-root file-name)
	   (if-let ((test-function (test-cockpit--python--find-current-test)))
	       (concat "::" test-function)))))

(provide 'test-cockpit-python)

;;; test-cockpit-python.el ends here
