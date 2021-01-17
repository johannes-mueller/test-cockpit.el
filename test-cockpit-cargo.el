
(require 'test-cockpit)
(require 'test-cockpit-transient)

(test-cockpit-register-project-type 'rust-cargo
				    'test-cockpit--cargo--test-project-command
				    'test-cockpit--cargo--test-module-command
				    'test-cockpit--cargo--test-function-command
				    'test-cockpit--cargo--infix)

(defun test-cockpit--cargo--test-project-command (args)
  (concat (test-cockpit--cargo--command-with-inserted-switches args)
	  (test-cockpit--cargo--append-test-switches args)))

(defun test-cockpit--cargo--test-module-command (args)
  (concat (test-cockpit--cargo--command-with-inserted-switches args)
	  " "
	  (concat (or (test-cockpit--cargo--build-module-path)
		      (file-name-base (buffer-file-name))) "::")
	  (test-cockpit--cargo--append-test-switches args)))

(defun test-cockpit--cargo--test-function-command (args)
  (concat (test-cockpit--cargo--command-with-inserted-switches args)
	  " "
	  (test-cockpit--cargo--build-test-fn-path)
	  (test-cockpit--cargo--append-test-switches args)))

(defconst test-cockpit---cargo---mod-regexp
  "^\s*mod\s+\\([[:word:][:multibyte:]_][[:word:][:multibyte:]_[:digit:]]*\\)\s*{")

(defun test-cockpit--cargo--track-module-path (initial-point)
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
  (save-excursion
    (search-backward-regexp test-cockpit--cargo--test-fn-regexp nil t)
    (match-string 3)))

(defun test-cockpit--cargo--build-test-fn-path ()
  (let ((mod (test-cockpit--cargo--build-module-path))
	(fn (test-cockpit--cargo--find-test-fn-name) ))
    (when fn
      (if mod (concat mod "::" fn) fn))))

(defun test-cockpit--cargo--insert-test-switches (switches)
  (if switches
      (if (member "--doc" switches)
	  "--doc"
	(test-cockpit--join-filter-switches switches
						  '("--tests" "--benches" "--examples")))
    ""))

(defun test-cockpit--cargo--append-test-switches (switches)
  (if-let ((result-string
	    (let ((might-be-empty
		   (test-cockpit--join-filter-switches switches
							      '("--ignored"
								"--include-ignored"
								"--nocapture"))))
	      (if (equal might-be-empty "") nil might-be-empty))))
      (concat " -- " result-string)
    ""))

(defun test-cockpit--cargo--features-switch ()
  (if test-cockpit--cargo--enabled-features
      (string-join (cons "--features" test-cockpit--cargo--enabled-features) " ")
    ""))

(defun test-cockpit--cargo--command-with-inserted-switches (args)
  (string-trim-right
   (string-join (seq-filter (lambda (s) (string> s ""))
			    `("cargo test"
			      ,(test-cockpit--cargo--insert-test-switches args)
			      ,(test-cockpit--cargo--features-switch)))
		" ")))

(defun test-cockpit--cargo--read-crate-features ()
  (let ((cargo-toml-data
	 (toml:read-from-file (concat (projectile-project-root) "Cargo.toml"))))
    (seq-map (lambda (key-val) (car key-val))
	     (cdr (seq-find
		   (lambda (group-kv) (string= (car group-kv) "features"))
		   cargo-toml-data)))))

(defvar test-cockpit--cargo--enabled-features nil)

(transient-define-infix test-cockpit--cargo--toggle-feature ()
  :class 'test-cockpit--transient--selection
  :variable 'test-cockpit--cargo--enabled-features
  :prompt "feature: "
  :choices 'test-cockpit--cargo--read-crate-features
  :description "features")

(defun test-cockpit--cargo--infix ()
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
