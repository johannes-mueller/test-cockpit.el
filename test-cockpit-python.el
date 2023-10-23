;;; test-cockpit-python.el --- The test-cockpit package for python projects -*- lexical-binding: t; -*-

;; Author: Johannes Mueller <github@johannes-mueller.org>
;; URL: https://github.com/johannes-mueller/test-cockpit.el
;; Version: 0.1.0
;; License: GPLv3
;; SPDX-License-Identifier: GPL-3.0-only

;;; Commentary:

;; test-cockpit is a unified user interface for test runners of different
;; programming languages resp. their testing tools.  This is the module for the
;; pytest runner for the python programming language.

;;; Code:

(require 'test-cockpit)

(defvar test-cockpit-python-build-ext-command "python setup.py build_ext --inplace"
  "The command to build the python extensions.")

(defclass test-cockpit-python-engine (test-cockpit--engine) ())

(cl-defmethod test-cockpit--test-project-command ((_obj test-cockpit-python-engine))
  "Implement test-cockpit--test-project-command." 'test-cockpit-python--test-project-command)

(cl-defmethod test-cockpit--test-module-command ((_obj test-cockpit-python-engine))
  "Implement test-cockpit--test-module-command." 'test-cockpit-python--test-module-command)

(cl-defmethod test-cockpit--test-function-command ((_obj test-cockpit-python-engine))
  "Implement test-cockpit--test-function-command." 'test-cockpit-python--test-function-command)

(cl-defmethod test-cockpit--transient-infix ((_obj test-cockpit-python-engine))
  "Implement test-cockpit--test-infix."
  (test-cockpit-python--infix))

(cl-defmethod test-cockpit--engine-current-module-string ((_obj test-cockpit-python-engine))
  "Implement test-cockpit--engine-current-module-string."
  (test-cockpit-python--choose-module))

(cl-defmethod test-cockpit--engine-current-function-string ((_obj test-cockpit-python-engine))
  "Implement test-cockpit--engine-current-function-string."
  (test-cockpit-python--test-function-path))

(test-cockpit-register-project-type 'python-pip 'test-cockpit-python-engine)
(test-cockpit-register-project-type-alias 'python-pkg 'python-pip)
(test-cockpit-register-project-type-alias 'python-tox 'python-pip)
(test-cockpit-register-project-type-alias 'python-toml 'python-pip)

(defconst test-cockpit-python--allowed-switches
  '("--last-failed"
    "--verbose"
    "--no-cov"
    "--cov"
    "--cov-report"
    "-rFP"
    "--disable-warnings"
    "--capture=no"
    "-k"
    "-m"
    "--mypy"))

(defun test-cockpit-python--test-project-command (_ args)
  "Make the test project command from ARGS."
  (concat (test-cockpit-python--common-switches args)))

(defun test-cockpit-python--test-module-command (string args)
  "Make the test module command from STRING and ARGS."
  (concat (test-cockpit-python--common-switches args) " " string))

(defun test-cockpit-python--choose-module ()
  "Find the current test module."
  (when-let ((file-name-path (buffer-file-name))
             ((string-prefix-p "test_" (file-name-nondirectory file-name-path))))
    (test-cockpit--strip-project-root file-name-path)))

(defun test-cockpit-python--test-function-command (string args)
  "Make the test function command from STRING and ARGS."
  (concat (test-cockpit-python--common-switches args) " " string))

(defun test-cockpit-python--build-ext-command (args)
  "Add the build extensions command if ARGS demands it."
  (if (member "build_ext" args)
      (concat test-cockpit-python-build-ext-command " && ")
    ""))

(defun test-cockpit-python--common-switches (args)
  "Extract the common python switches from ARGS."

  (concat (test-cockpit-python--build-ext-command args)
          "pytest --color=yes"
          (test-cockpit--add-leading-space-to-switches
           (test-cockpit--join-filter-switches
            (test-cockpit-python--insert-project-coverage-to-switches
             (test-cockpit-python--insert-no-coverage-to-switches args))
            test-cockpit-python--allowed-switches))))

(defun test-cockpit-python--insert-no-coverage-to-switches (switches)
  "Adjust the coverage report switch according to SWITCHES."
  (if (not (seq-find (lambda (sw) (string-prefix-p "--cov-report=" sw)) switches))
      (append switches '("--no-cov"))
    switches))

(defun test-cockpit-python--coverage-project-switch ()
  "Make the switch `--cov <projectname>'."
  (list (string-join `("--cov " ,(string-replace "-" "_" (projectile-project-name))))))

(defun test-cockpit-python--insert-project-coverage-to-switches (switches)
  "Adjust the coverage report switch according to SWITCHES."
  (seq-reduce (lambda (acc sw)
                (append
                 (if (string-prefix-p "--cov-report=" sw)
                     (append acc (test-cockpit-python--coverage-project-switch))
                   acc)
                 (list sw)))
              switches
              '()))

(transient-define-argument test-cockpit-python--restrict-substring ()
  :description "Restrict to tests matching string"
  :class 'transient-option
  :key "-k"
  :argument "-k")

(transient-define-argument test-cockpit-python--marker-switch ()
  :description "Add marker switch "
  :class 'transient-option
  :key "-m"
  :argument "-m")

(defun test-cockpit-python--infix ()
  "Setup the pytest specific test switches."
  [["Switches"
    ("-k" test-cockpit-python--restrict-substring)
    ("-f" "only lastly failed tests" "--last-failed")
    ("-b" "build extensions before testing" "build_ext")
    ("-m" test-cockpit-python--marker-switch)
    ("-M" "test type hints" "--mypy")]
   ["Output"
    ("-v" "show single tests" "--verbose")
    ("-c" "print coverage report" "--cov-report=term-missing")
    ("-r" "report output of passed tests" "-rFP")
    ("-w" "don't output warnings" "--disable-warnings")
    ("-n" "don't capture output" "--capture=no")]])

(defun test-cockpit-python--find-last-unindented-line ()
  "Find the last unindented line from current point in current buffer."
  (save-excursion
    (end-of-line)
    (when (search-backward-regexp "^\\([[:alpha:]].*\\)$" nil t)
      (match-string 1))))

(defun test-cockpit-python--test-function-in-line (line)
  "Find a test function in LINE."
  (when (string-match "def[[:space:]]+\\(test_[[:word:]_]*\\)" line)
    (match-string 1 line)))

(defun test-cockpit-python--class-in-line (line)
  "Find a test class in LINE."
  (when (string-match "^class[[:space:]]+\\(Test[[:word:]_]*\\)\\((.*)\\)?:" line)
    (match-string 1 line)))

(defun test-cockpit-python--maybe-test-method (pos)
  "Determine the string of the method at POS to be tested, if any."
  (save-excursion
    (when (and (search-backward-regexp "def[[:space:]]+\\([[:alpha:]][[:word:]_]*\\)" nil t)
               (< pos (match-beginning 0))
               (string-prefix-p "test_" (match-string 1)))
      (concat "::" (match-string 1)))))

(defun test-cockpit-python--test-method-or-class (line pos)
  "Determine the current test at LINE and POS (class or method)."
  (when-let ((test-class (test-cockpit-python--class-in-line line)))
    (concat test-class (test-cockpit-python--maybe-test-method pos))))

(defun test-cockpit-python--find-current-test ()
  "Determine the current test at point."
  (when-let* ((unindented-line (test-cockpit-python--find-last-unindented-line))
              (unindented-pos (match-beginning 0)))
    (or (test-cockpit-python--test-function-in-line unindented-line)
        (test-cockpit-python--test-method-or-class unindented-line unindented-pos))))

(defun test-cockpit-python--test-function-path ()
  "Determine the path to the test function at point."
  (when-let* ((file-name (buffer-file-name))
              ((string-prefix-p "test_" (file-name-nondirectory file-name))))
    (concat (test-cockpit--strip-project-root file-name)
            (when-let ((test-function (test-cockpit-python--find-current-test)))
              (concat "::" test-function)))))

(provide 'test-cockpit-python)

;;; test-cockpit-python.el ends here
