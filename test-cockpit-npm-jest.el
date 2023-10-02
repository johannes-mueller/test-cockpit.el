;;; test-cockpit-npm-jest.el --- The package to test ts-jest projects in test-cockpit -*- lexical-binding: t; -*-

;; Author: Johannes Mueller <github@johannes-mueller.org>
;; URL: https://github.com/johannes-mueller/test-cockpit.el
;; Version: 0.1.0
;; License: GPLv3
;; SPDX-License-Identifier: GPL-3.0-only

;;; Commentary:

;; This is the package to test TypeScript/Jest projects in test-cockpit.

;;; Code:

(require 'test-cockpit)

(defclass test-cockpit-npm-jest--engine (test-cockpit--engine) ())

(cl-defmethod test-cockpit--test-project-command ((_obj test-cockpit-npm-jest--engine))
  "Implement test-cockpit--test-project-command." 'test-cockpit-npm-jest--test-project-command)

(cl-defmethod test-cockpit--test-module-command ((_obj test-cockpit-npm-jest--engine))
  "Implement test-cockpit--test-module-command." 'test-cockpit-npm-jest--test-module-command)

(cl-defmethod test-cockpit--test-function-command ((_obj test-cockpit-npm-jest--engine))
  "Implement test-cockpit--test-function-command." 'test-cockpit-npm-jest--test-function-command)

(cl-defmethod test-cockpit--transient-infix ((_obj test-cockpit-npm-jest--engine))
"Implement test-cockpit--test-infix."
    (test-cockpit-npm-jest--infix))

(cl-defmethod test-cockpit--engine-current-module-string ((_obj test-cockpit-npm-jest--engine))
  "Implement test-cockpit--engine-current-module-string."
  (test-cockpit-npm-jest--choose-module))

(cl-defmethod test-cockpit--engine-current-function-string ((_obj test-cockpit-npm-jest--engine))
  "Implement test-cockpit--engine-current-function-string."
  (test-cockpit-npm-jest--find-current-test))


(test-cockpit-register-project-type 'npm 'test-cockpit-npm-jest--engine)


(defun test-cockpit-npm-jest--infix ()
  "Setup project type specific swich menu."
  [["Switches"
    ("-f" "only lastly failed tests" "--onlyFailures")
    ("-C" "only lastly changed tests" "--onlyChanged")]
   ["Output"
    ("-c" "print coverage report" "--coverage")]])

(defun test-cockpit-npm-jest--test-project-command (_ switches)
  "Make the command to test the project with SWITCHES."
  (concat "npm test -- --color " (string-join switches " ")))

(defun test-cockpit-npm-jest--test-module-command (_ switches)
  "Make the command to test a module with SWITCHES."
  (concat "npm test -- --color "
          "--testPathPattern '"(regexp-quote (test-cockpit-npm-jest--choose-module))"' "
          (string-join switches " ")))

(defun test-cockpit-npm-jest--test-function-command (_ switches)
  "Make the command to run one test with SWITCHES."
  (concat "npm test -- --color "
          "--testPathPattern '"(regexp-quote (test-cockpit-npm-jest--choose-module))"' "
          "--testNamePattern '"(regexp-quote (test-cockpit-npm-jest--find-current-test))"' "
          (string-join switches " ")))

(defconst test-cockpit-npm-jest--test-match-regex-list
  '("\\(test\\|spec\\).[jt]sx?"))

(defconst test-cockpit-npm-jest--test-modifier-regexp "\\(\\.[[:word:]]+\\)*")

(defun test-cockpit-npm-jest--choose-module ()
  "Find the test module at point.

This is the current buffer file name if the file name matches
`test-cockpit-npm-jest--test-match-regex-list'."
  (let ((candidate (buffer-file-name)))
    (when (and candidate
               (cl-find-if
                (lambda (pattern) (string-match-p pattern candidate))
            test-cockpit-npm-jest--test-match-regex-list))
      candidate)))

(defun test-cockpit-npm-jest--find-current-test ()
  "Find the current test at point.

Depending on the point this can be a single test or a test group.
Return a string that is understood by the --testNamePattern switch of jest."
  (save-excursion
    (let ((desc (test-cockpit-npm-jest--find-current-desc))
          (it (test-cockpit-npm-jest--find-current-it)))
     (when (or desc it)
       (string-trim
        (string-join `(,(cadr desc)
                       ,(if (or (not desc)
                                (and it (> (car it) (car desc))))
                            (cadr it)))
                     " "))))))

(defun test-cockpit-npm-jest--find-current-it ()
  "Find the current test function defined by (it ... or (test ..."
  (test-cockpit-npm-jest--find-marker "\\(test\\|it\\)"))

(defun test-cockpit-npm-jest--find-current-desc ()
  "Find the current test group defined by (describe ..."
  (save-excursion
    (test-cockpit-npm-jest--find-marker "describe")))

(defun test-cockpit-npm-jest--find-marker (marker-regexp)
  "Find the next marker defined by MARKER-REGEXP.

A marker is a marker for a test or a test group understood by
--testNamePattern of jest."
  (let ((forward-sexp-function nil))
    (when-let ((start-pos (test-cockpit-npm-jest--goto-initial-marker marker-regexp)))
      (test-cockpit-npm-jest--skip-potential-each-table)
      (when-let ((result-string (test-cockpit-npm-jest--unqote-test-name-sexp
                                 (test-cockpit-npm-jest--test-name-sexp))))
         `(,start-pos ,result-string)))))

(defun test-cockpit-npm-jest--goto-initial-marker (marker-regexp)
  "Go to the initial test or test group marker defined by MARKER-REGEXP.

Return the beginning of the marker."
  (when (search-backward-regexp (concat
                                   "^\s*"
                                   marker-regexp
                                   test-cockpit-npm-jest--test-modifier-regexp
                                   "[^[:alnum:]]")
                                 nil t )
      (goto-char (match-end 0))
      (match-beginning 0)))

(defun test-cockpit-npm-jest--skip-potential-each-table ()
  "Jump past a test parameter table if there is one."
  (when (string-match-p "each" (match-string-no-properties 0))
    (forward-sexp) (forward-char)))

(defun test-cockpit-npm-jest--test-name-sexp ()
  "Return the sexp of the current thing to be tested."
  (let ((endpoint (save-excursion (forward-sexp) (forward-char) (point))))
    (buffer-substring (point) endpoint)))

(defun test-cockpit-npm-jest--unqote-test-name-sexp (test-sexp)
  "Strip the quotes from TEST-SEXP."
  (string-match "\\(['`\"]\\)\\(.*\\)\\1" test-sexp)
  (match-string-no-properties 2 test-sexp))



(provide 'test-cockpit-npm-jest)

;;; test-cockpit-npm-jest.el ends here
