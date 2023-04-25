;;; test-cockpit-npm-jest.el --- The package to test ts-jest projects in test-cockpit

;; Author: Johannes Mueller <github@johannes-mueller.org>
;; URL: https://github.com/johannes-mueller/test-cockpit.el
;; Version: 0.1.0
;; License: GPLv3
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; This is the package to test TypeScript/Jest projects in test-cockpit.

;;; Code:

(require 'test-cockpit)

(defvar test-cockpit-npm-jest--test-match-regex-list
  '("\\(test\\|spec\\).[jt]sx?"))

(defclass test-cockpit-npm-jest--engine (test-cockpit--engine) ())

(cl-defmethod test-cockpit--test-project-command ((obj test-cockpit-npm-jest--engine))
  'test-cockpit-npm-jest--test-project-command)

(cl-defmethod test-cockpit--test-module-command ((obj test-cockpit-npm-jest--engine))
  'test-cockpit-npm-jest--test-module-command)

(cl-defmethod test-cockpit--engine-current-module-string ((obj test-cockpit-npm-jest--engine))
  (test-cockpit-npm-jest--choose-module))

(cl-defmethod test-cockpit--test-function-command ((obj test-cockpit-npm-jest--engine))
  'test-cockpit-npm-jest--test-function-command)

(cl-defmethod test-cockpit--engine-current-function-string ((obj test-cockpit-npm-jest--engine))
  (test-cockpit-npm-jest--find-current-test))

(cl-defmethod test-cockpit--transient-infix ((obj test-cockpit-npm-jest--engine))
  (test-cockpit-npm-jest--infix))

(test-cockpit-register-project-type 'npm 'test-cockpit-npm-jest--engine)


(defun test-cockpit-npm-jest--infix ()
  [["Switches"
    ("-f" "only lastly failed tests" "--onlyFailures")
    ("-C" "only lastly changed tests" "--onlyChanged")]
   ["Output"
    ("-c" "print coverage report" "--coverage")]])

(defun test-cockpit-npm-jest--test-project-command (_ switches)
  (concat "npm test -- --color " (string-join switches " ")))

(defun test-cockpit-npm-jest--test-module-command (_ switches)
  (concat "npm test -- --color "
          "--testPathPattern '"(regexp-quote (test-cockpit-npm-jest--choose-module))"' "
          (string-join switches " ")))

(defun test-cockpit-npm-jest--test-function-command (_ switches)
  (concat "npm test -- --color "
          "--testPathPattern '"(regexp-quote (test-cockpit-npm-jest--choose-module))"' "
          "--testNamePattern '"(regexp-quote (test-cockpit-npm-jest--find-current-test))"' "
          (string-join switches " ")))

(defun test-cockpit-npm-jest--choose-module ()
  (let ((candidate (buffer-file-name)))
    (when (and candidate
               (cl-find-if
                (lambda (pattern) (string-match-p pattern candidate))
            test-cockpit-npm-jest--test-match-regex-list))
      candidate)))

(defconst test-cockpit-npm-jest--test-modifier-regexp "\\(\\.[[:word:]]+\\)*")

(defun test-cockpit-npm-jest--find-current-it ()
  (test-cockpit-npm-jest--find-marker "\\(test\\|it\\)"))

(defun test-cockpit-npm-jest--find-current-desc ()
  (save-excursion
    (test-cockpit-npm-jest--find-marker "describe")))

(defun test-cockpit-npm-jest--goto-initial-marker (marker-regexp)
    (when (search-backward-regexp (concat
                                   "^\s*"
                                   marker-regexp
                                   test-cockpit-npm-jest--test-modifier-regexp
                                   "[^[:alnum:]]")
                                 nil t )
      (goto-char (match-end 0))
      (match-beginning 0)))

(defun test-cockpit-npm-jest--skip-potential-each-table ()
  (when (string-match-p "each" (match-string-no-properties 0))
    (forward-sexp) (forward-char)))

(defun test-cockpit-npm-jest--test-name-sexp ()
  (let ((endpoint (save-excursion (forward-sexp) (forward-char) (point))))
    (buffer-substring (point) endpoint)))

(defun test-cockpit-npm-jest--unqote-test-name-sexp (test-sexp)
  (string-match "\\(['`\"]\\)\\(.*\\)\\1" test-sexp)
  (match-string-no-properties 2 test-sexp))

(defun test-cockpit-npm-jest--find-marker (marker-regexp)
  (let ((forward-sexp-function nil))
    (when-let ((start-pos (test-cockpit-npm-jest--goto-initial-marker marker-regexp)))
      (test-cockpit-npm-jest--skip-potential-each-table)
      (when-let ((result-string (test-cockpit-npm-jest--unqote-test-name-sexp
                                 (test-cockpit-npm-jest--test-name-sexp))))
         `(,start-pos ,result-string)))))

(defun test-cockpit-npm-jest--find-current-test ()
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

(provide 'test-cockpit-npm-jest)

;;; test-cockpit-npm-jest.el ends here
