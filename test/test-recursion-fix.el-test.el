;;; test-recursion-fix.el-test.el --- Tests demonstrating recursion fix -*- lexical-binding: t; -*-

;; This file demonstrates the recursion issue that occurs when using
;; `transient-args' in predicate functions like `:inapt-if-not' with
;; `:refresh-suffixes t'.

;;; Commentary:

;; The problem: When a transient prefix has `:refresh-suffixes t' and
;; a suffix uses `:inapt-if-not' with a predicate that calls
;; `transient-args', it creates infinite recursion:
;;
;; 1. Transient tries to refresh suffixes
;; 2. To determine if suffix is inapt, it calls the predicate
;; 3. The predicate calls `transient-args' which tries to get current args
;; 4. This triggers suffix refresh again (due to :refresh-suffixes t)
;; 5. Goto step 2 -> infinite recursion
;;
;; The solution: Use `transient-get-value' instead of `transient-args'
;; in predicate functions. `transient-get-value' is designed to work
;; safely within the transient menu context, while `transient-args'
;; is meant for suffix command bodies.

;;; Code:

(require 'ert)
(require 'transient)

;; BROKEN: This causes recursion
;; Uncomment to see the recursion error:
;;
;; (defun use-extended-p-broken ()
;;   "Check if 'use-extended' option is enabled (BROKEN VERSION)."
;;   (let ((args (transient-args 'extended-option-prefix-broken)))
;;     (member "use-extended" args)))
;;
;; (transient-define-prefix extended-option-prefix-broken ()
;;   :refresh-suffixes t
;;   ["Options"
;;    ("u" "use extended options" "use-extended")
;;    ("e" "extension" "some-extension" :inapt-if-not use-extended-p-broken)])

;; FIXED: This works correctly
(defun use-extended-p-fixed ()
  "Check if 'use-extended' option is enabled (FIXED VERSION).
Uses `transient-get-value' instead of `transient-args' to avoid recursion."
  (let ((args (transient-get-value 'extended-option-prefix-fixed)))
    (member "use-extended" args)))

(transient-define-prefix extended-option-prefix-fixed ()
  "Transient prefix demonstrating the correct way to avoid recursion."
  :refresh-suffixes t
  ["Options"
   ("u" "use extended options" "use-extended")
   ("e" "extension" "some-extension" :inapt-if-not use-extended-p-fixed)])

(ert-deftest test-recursion-fix--predicate-without-use-extended ()
  "Test that predicate correctly returns nil when 'use-extended' is not set."
  ;; Initialize the transient with no value
  (let ((transient-values (list (cons 'extended-option-prefix-fixed nil))))
    (should-not (use-extended-p-fixed))))

(ert-deftest test-recursion-fix--predicate-with-use-extended ()
  "Test that predicate correctly returns non-nil when 'use-extended' is set."
  ;; Initialize the transient with 'use-extended' value
  (let ((transient-values (list (cons 'extended-option-prefix-fixed '("use-extended")))))
    (should (use-extended-p-fixed))))

(ert-deftest test-recursion-fix--predicate-with-multiple-args ()
  "Test that predicate correctly identifies 'use-extended' among multiple args."
  ;; Initialize the transient with multiple values including 'use-extended'
  (let ((transient-values (list (cons 'extended-option-prefix-fixed '("other-arg" "use-extended" "another-arg")))))
    (should (use-extended-p-fixed))))

(ert-deftest test-recursion-fix--predicate-without-target-in-multiple-args ()
  "Test that predicate returns nil when 'use-extended' is not in the list."
  ;; Initialize the transient with multiple values but not 'use-extended'
  (let ((transient-values (list (cons 'extended-option-prefix-fixed '("other-arg" "another-arg")))))
    (should-not (use-extended-p-fixed))))

;;; test-recursion-fix.el-test.el ends here
