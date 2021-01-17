;;; test-cockpit-cargo.el --- The package to test cargo projects in test-cockpit

;; Author: Johannes Mueller <github@johannes-mueller.org
;; URL: https://github.com/johannes-mueller/test-cockpit.el
;; License: GPLv2

;;; Commentary:

;; This is the package to test Rust/Cargo projects in test-cockpit.

;; Specific switches and settings

;; * Select kind of tests (--tests, --doc, --benchmarks, --examples)
;; * Include or test only tests markt to ingnore (--ignored, --include-ignored)
;; * Dump stdin and stdout to the *compilation* buffer (--nocapture)
;; * Discover cargo features and let the user toggle them

;; Badly missing features:

;; * workspaces

;;; Code:

(require 'test-cockpit)
(require 'test-cockpit-transient)

(test-cockpit-register-project-type 'rust-cargo
				    'test-cockpit--cargo--test-project-command
				    'test-cockpit--cargo--test-module-command
				    'test-cockpit--cargo--test-function-command
				    'test-cockpit--cargo--infix)

(defun test-cockpit--cargo--test-project-command (args)
  "Setup the command to test the project with ARGS."
  (concat (test-cockpit--cargo--command-with-inserted-switches args)
	  (test-cockpit--cargo--append-test-switches args)))

(defun test-cockpit--cargo--test-module-command (args)
  "Setup the command to test the module at point with ARGS."
  (concat (test-cockpit--cargo--command-with-inserted-switches args)
	  " "
	  (concat (or (test-cockpit--cargo--build-module-path)
		      (file-name-base (buffer-file-name))) "::")
	  (test-cockpit--cargo--append-test-switches args)))

(defun test-cockpit--cargo--test-function-command (args)
  "Setup the command to test the function at point with ARGS."
  (concat (test-cockpit--cargo--command-with-inserted-switches args)
	  " "
	  (test-cockpit--cargo--build-test-fn-path)
	  (test-cockpit--cargo--append-test-switches args)))

(defconst test-cockpit---cargo---mod-regexp
  "^\s*mod\s+\\([[:word:][:multibyte:]_][[:word:][:multibyte:]_[:digit:]]*\\)\s*{")

(defun test-cockpit--cargo--track-module-path (initial-point)
  "Recursively find all modules in the current buffer before INITIAL-POINT."
  (set-match-data nil)
  (save-excursion
    (if-let ((mod
		(progn
		  (search-backward-regexp test-cockpit---cargo---mod-regexp nil t)
		  (match-string 1))))
      (let ((next-search-start-pos (match-beginning 0)))
	(search-forward "{")
	(backward-char 1)
	(forward-sexp)
	(concat (save-excursion
		  (goto-char next-search-start-pos)
		  (test-cockpit--cargo--track-module-path initial-point))
		(when (> (point) initial-point)
		  (concat "::" mod))))
      "")))

(defun test-cockpit--cargo--build-module-path ()
  "Determine the qualified module module path at point."
  (let ((relative-path
	 (replace-regexp-in-string
	  "\\(::mod\\|^src::lib\\|^src::main\\)?\\.rs$" ""
	  (replace-regexp-in-string
	   "/" "::"
	   (substring (buffer-file-name) (length (projectile-project-root)) nil)))))
    (let ((mod (replace-regexp-in-string
		"^\\(src\\)?::" ""
		(concat relative-path (test-cockpit--cargo--track-module-path (point))))))
      (unless (eq mod "") mod))))

(defconst test-cockpit--cargo--test-fn-regexp
  "#\\[test\\][[:space:]\n]\\([[:space:]\n]*#\\[[^[]*\\][[:space:]\n]\\)*[[:space:]\n]*\\(async\s*\\)?\s*fn \\([[:alpha:]][[:word:]_]*\\)")

(defun test-cockpit--cargo--find-test-fn-name ()
  "Find the function marked as test in the at point."
  (save-excursion
    (search-backward-regexp test-cockpit--cargo--test-fn-regexp nil t)
    (match-string 3)))

(defun test-cockpit--cargo--build-test-fn-path ()
  "Determine the qualified function path at point."
  (let ((mod (test-cockpit--cargo--build-module-path))
	(fn (test-cockpit--cargo--find-test-fn-name) ))
    (when fn
      (if mod (concat mod "::" fn) fn))))

(defconst test-cockpit--cargo--insertable-switches
  '("--tests"
    "--benches"
    "--examples"))

(defconst test-cockpit--cargo--appendable-switches
  '("--ignored"
    "--include-ignored"
    "--nocapture"))

(defun test-cockpit--cargo--insert-test-switches (switches)
  "Extract the insertable switches (before --) of SWITCHES."
  (if switches
      (if (member "--doc" switches)
	  "--doc"
	(test-cockpit--join-filter-switches switches
					    test-cockpit--cargo--insertable-switches))
    ""))

(defun test-cockpit--cargo--append-test-switches (switches)
  "Extract the appendable switches (after --) of SWITCHES."
  (if-let ((result-string
	    (let ((might-be-empty
		   (test-cockpit--join-filter-switches switches
						       test-cockpit--cargo--appendable-switches)))
	      (if (equal might-be-empty "") nil might-be-empty))))
      (concat " -- " result-string)
    ""))

(defvar test-cockpit--cargo--enabled-features nil
  "The currently enabled cargo features.")

(defun test-cockpit--cargo--features-switch ()
  "Setup the --features switch from the currently enabled features"
  (if test-cockpit--cargo--enabled-features
      (string-join (cons "--features" test-cockpit--cargo--enabled-features) " ")
    ""))

(defun test-cockpit--cargo--command-with-inserted-switches (args)
  "Setup the prefix for the cargo test command."
  (string-trim-right
   (string-join (seq-filter (lambda (s) (string> s ""))
			    `("cargo test"
			      ,(test-cockpit--cargo--insert-test-switches args)
			      ,(test-cockpit--cargo--features-switch)))
		" ")))

(defun test-cockpit--cargo--read-crate-features ()
  "Read the features available in the current crate."
  (let ((cargo-toml-data
	 (toml:read-from-file (concat (projectile-project-root) "Cargo.toml"))))
    (seq-map (lambda (key-val) (car key-val))
	     (cdr (seq-find
		   (lambda (group-kv) (string= (car group-kv) "features"))
		   cargo-toml-data)))))

(transient-define-infix test-cockpit--cargo--toggle-feature ()
  :class 'test-cockpit--transient--selection
  :variable 'test-cockpit--cargo--enabled-features
  :prompt "feature: "
  :choices 'test-cockpit--cargo--read-crate-features
  :description "features")

(defun test-cockpit--cargo--infix ()
  "The infix array for cargo projects."
  [["Targets"
    ("-t" "tests" "--tests")
    ("-b" "with benchmarks" "--benches")
    ("-x" "with examples" "--examples")
    ("-d" "only doctests" "--doc")
    ("-f" test-cockpit--cargo--toggle-feature)]
   ["Switches"
    ("-I" "only ignored tests" "--ignored")
    ("-i" "include ignored tests" "--include-ignored")
    ("-n" "print output" "--nocapture")]])


(provide 'test-cockpit-cargo)
;;; test-cockpit-cargo.el ends here
