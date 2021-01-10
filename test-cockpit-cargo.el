
(require 'test-cockpit)

(test-cockpit-register-project-type 'rust-cargo
				    'test-cockpit--cargo--test-project-command
				    'test-cockpit--cargo--test-module-command
				    'test-cockpit--cargo--test-function-command
				    'test-cockpit--cargo--infix)

(defun test-cockpit--cargo--test-project-command (args) "cargo test")

(defun test-cockpit--cargo--test-module-command (args)
  (concat "cargo test " (concat (or (test-cockpit--cargo--build-module-path)
				    (file-name-base (buffer-file-name)))) "::"))

(defun test-cockpit--cargo--test-function-command (args)
  (concat "cargo test " (test-cockpit--cargo--build-test-fn-path)))
(provide 'test-cockpit-cargo)

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

(defun test-cockpit--cargo--join-filter-switches (candidates allowed)
  (mapconcat 'identity
	     (delete 'exclude
		     (mapcar (lambda (sw) (if (member sw candidates) sw 'exclude))
			     allowed))
	     " "))

(defun test-cockpit--cargo--insert-test-switches (switches)
  (if switches
      (if (member "--doc" switches)
	  "--doc"
	(test-cockpit--cargo--join-filter-switches switches
						  '("--tests" "--benches" "--examples")))
    ""))

(defun test-cockpit--cargo--infix ()
  ["Targets"
   ("-t" "tests" "--tests")
   ("-b" "with benchmarks" "--benches")
   ("-x" "with examples" "--examples")
   ("-d" "only doctests" "--doc")])
;;; test-cockpit-cargo.el ends here
