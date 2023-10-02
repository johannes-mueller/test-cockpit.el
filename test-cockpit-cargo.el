;;; test-cockpit-cargo.el --- The package to test cargo projects in test-cockpit -*- lexical-binding: t; -*-

;; Author: Johannes Mueller <github@johannes-mueller.org>
;; URL: https://github.com/johannes-mueller/test-cockpit.el
;; Version: 0.1.0
;; License: GPLv3
;; SPDX-License-Identifier: GPL-3.0-only

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
(require 'toml)

(defclass test-cockpit-cargo-engine (test-cockpit--engine) ())

(cl-defmethod test-cockpit--test-project-command ((_obj test-cockpit-cargo-engine))
  "Implement test-cockpit--test-project-command." 'test-cockpit-cargo--test-project-command)

(cl-defmethod test-cockpit--test-module-command ((_obj test-cockpit-cargo-engine))
  "Implement test-cockpit--test-module-command." 'test-cockpit-cargo--test-module-command )

(cl-defmethod test-cockpit--test-function-command ((_obj test-cockpit-cargo-engine))
  "Implement test-cockpit--test-function-command." 'test-cockpit-cargo--test-function-command)

(cl-defmethod test-cockpit--transient-infix ((_obj test-cockpit-cargo-engine))
  "Implement test-cockpit--test-infix."
  (test-cockpit-cargo--infix))

(cl-defmethod test-cockpit--engine-current-module-string ((_obj test-cockpit-cargo-engine))
  "Implement test-cockpit--engine-current-module-string."
  (test-cockpit-cargo--build-module-path-or-file-path-fallback))

(cl-defmethod test-cockpit--engine-current-function-string ((_obj test-cockpit-cargo-engine))
  "Implement test-cockpit--engine-current-function-string."
  (test-cockpit-cargo--build-test-fn-path))


(test-cockpit-register-project-type 'rust-cargo 'test-cockpit-cargo-engine)

(defun test-cockpit-cargo--test-project-command (_ args)
  "Setup the command to test the project with ARGS."
  (concat (test-cockpit-cargo--command-with-inserted-switches args)
          (test-cockpit-cargo--append-test-switches args)))

(defun test-cockpit-cargo--test-module-command (module-string args)
  "Setup the command to test the MODULE-STRING with ARGS."
  (concat (test-cockpit-cargo--command-with-inserted-switches args)
          " "
          module-string
          (test-cockpit-cargo--append-test-switches args)))

(defun test-cockpit-cargo--test-function-command (function-string args)
  "Setup the command to test FUNCTION-STRING with ARGS."
  (concat (test-cockpit-cargo--command-with-inserted-switches args)
          " "
          function-string
          (test-cockpit-cargo--append-test-switches args)))

(defconst test-cockpit-cargo--mod-regexp
  "^\s*mod\s+\\([[:word:][:multibyte:]_][[:word:][:multibyte:]_[:digit:]]*\\)\s*{")

(defun test-cockpit-cargo--build-module-path ()
  "Determine the qualified module path at point, if any."
  (when-let* ((file-name (buffer-file-name))
              (relative-path (test-cockpit-cargo--relative-module-path file-name))
              (module (test-cockpit-cargo--strip-leading-src relative-path))
              ((string> module "")))
    module))

(defun test-cockpit-cargo--relative-module-path (file-name)
  "Make the path of FILE-NAME relative to the project root with :: as delimiter."
  (replace-regexp-in-string
   "\\(::mod\\|^src::lib\\|^src::main\\)?\\.rs$" ""
   (replace-regexp-in-string
    "/" "::"
    (test-cockpit--strip-project-root file-name))))

(defun test-cockpit-cargo--strip-leading-src (module-path)
  "Drop the leading `src' of MODULE-PATH."
  (replace-regexp-in-string
   "^\\(src\\)?::" ""
   (test-cockpit-cargo--add-buffer-relative-module-path module-path)))

(defun test-cockpit-cargo--add-buffer-relative-module-path (file-module-path)
  "Add the buffer relative module path to FILE-MODULE-PATH."
  (concat file-module-path (test-cockpit-cargo--track-module-path (point))))

(defun test-cockpit-cargo--track-module-path (initial-point)
  "Recursively build the module path to the module before INITIAL-POINT."
  (set-match-data nil)
  (save-excursion
    (search-backward-regexp test-cockpit-cargo--mod-regexp nil t)
    (if-let ((mod (match-string 1)))
        (concat (save-excursion
                  (goto-char (match-beginning 0))
                  (test-cockpit-cargo--track-module-path initial-point))
                (test-cockpit-cargo--current-module initial-point mod))
      "")))

(defun test-cockpit-cargo--goto-end-of-module ()
  "Goto the end of the current module assuming being at its beginning."
  (search-forward "{")
  (backward-char 1)
  (forward-sexp))

(defun test-cockpit-cargo--current-module (initial-point mod)
  "Return ::MOD if INITIAL-POINT is before the end of the current module."
  (test-cockpit-cargo--goto-end-of-module)
  (when (> (point) initial-point) (concat "::" mod)))

(defun test-cockpit-cargo--build-module-path-or-file-path-fallback ()
  "Return the qualified module path at point or if not available the filename base."
  (when-let ((file-name (buffer-file-name)))
    (concat (or (test-cockpit-cargo--build-module-path)
                (file-name-base (buffer-file-name)))
            "::")))

(defconst test-cockpit-cargo--test-fn-regexp
  "#\\[test\\][[:space:]\n]\\([[:space:]\n]*#\\[[^[]*\\][[:space:]\n]\\)*[[:space:]\n]*\\(async\s*\\)?\s*fn \\([[:alpha:]][[:word:]_]*\\)")

(defun test-cockpit-cargo--build-test-fn-path ()
  "Determine the qualified function path at point."
  (when-let ((fn (test-cockpit-cargo--find-test-fn-name)))
    (if-let ((mod (test-cockpit-cargo--build-module-path)))
        (concat mod "::" fn)
      fn)))

(defun test-cockpit-cargo--find-test-fn-name ()
  "Find the function marked as test at point."
  (save-excursion
    (search-backward-regexp test-cockpit-cargo--test-fn-regexp nil t)
    (match-string 3)))

(defconst test-cockpit-cargo--insertable-switches
  '("--tests"
    "--benches"
    "--examples"))

(defconst test-cockpit-cargo--appendable-switches
  '("--ignored"
    "--include-ignored"
    "--nocapture"))

(defun test-cockpit-cargo--insert-test-switches (switches)
  "Extract the insertable switches (before --) of SWITCHES."
  (or (car (member "--doc" switches))
      (test-cockpit--join-filter-switches
       switches
       test-cockpit-cargo--insertable-switches)))

(defun test-cockpit-cargo--append-test-switches (switches)
  "Extract the appendable switches (after --) of SWITCHES."
  (concat
   ""
   (when-let*
       ((result-string (test-cockpit--join-filter-switches
                        switches
                        test-cockpit-cargo--appendable-switches))
        ((string> result-string "")))
     (concat " -- " result-string))))

(defvar test-cockpit-cargo--enabled-features nil
  "The currently enabled cargo features.")

(defun test-cockpit-cargo--features-switch ()
  "Setup the --features switch from the currently enabled features."
  (concat ""
          (when test-cockpit-cargo--enabled-features
            (string-join
             (cons "--features" test-cockpit-cargo--enabled-features) " "))))

(defun test-cockpit-cargo--command-with-inserted-switches (args)
  "Setup the prefix for the cargo test command based on ARGS."
  (string-trim-right
   (string-join (seq-filter (lambda (s) (string> s ""))
                            `("cargo test --color=always"
                              ,(test-cockpit-cargo--insert-test-switches args)
                              ,(test-cockpit-cargo--features-switch)))
                " ")))

(defun test-cockpit-cargo--read-crate-features ()
  "Read the features available in the current crate."
  (let ((cargo-toml-data (test-cockpit-cargo--crate-data)))
    (seq-map (lambda (key-val) (car key-val))
             (cdr (seq-find
                   (lambda (group-kv) (string= (car group-kv) "features"))
                   cargo-toml-data)))))

(defun test-cockpit-cargo--crate-data ()
  "Read the data for the crate from Cargo.toml."
  (toml:read-from-file (concat (projectile-project-root) "Cargo.toml")))

(transient-define-infix test-cockpit-cargo--toggle-feature ()
  :class 'test-cockpit--transient-selection
  :variable 'test-cockpit-cargo--enabled-features
  :prompt "feature: "
  :choices 'test-cockpit-cargo--read-crate-features
  :description "features")

(defun test-cockpit-cargo--infix ()
  "The switch menu for cargo projects."
  [["Targets"
    ("-t" "tests" "--tests")
    ("-b" "with benchmarks" "--benches")
    ("-x" "with examples" "--examples")
    ("-d" "only doctests" "--doc")
    ("-f" test-cockpit-cargo--toggle-feature)]
   ["Switches"
    ("-I" "only ignored tests" "--ignored")
    ("-i" "include ignored tests" "--include-ignored")
    ("-n" "print output" "--nocapture")]])


(provide 'test-cockpit-cargo)
;;; test-cockpit-cargo.el ends here
