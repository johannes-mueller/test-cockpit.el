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

(defcustom test-cockpit-python-uv-switches '("--all-extras" "--all-groups")
  "Additional switches to be applied to `uv run'."
  :group 'test-cockpit
  :type '(list string))

(defvar test-cockpit-python-build-ext-command "pip install -e . --no-deps"
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

(cl-defmethod test-cockpit--engine-dape-last-test-config ((_obj test-cockpit-python-engine))
  "Implement the invokation of dape for the last test."
  (let* ((last-cmd (test-cockpit--last-interactive-test-command))
         (args (vconcat (pcase last-cmd
                          ('test-cockpit-test-project [])
                          ('test-cockpit-test-module `[,(test-cockpit--last-module-string)])
                          ('test-cockpit-test-function `[,(test-cockpit--last-function-string)]))
                        (oref (test-cockpit--retrieve-engine) last-args))))
  `(command "python"
    command-args ("-m" "debugpy.adapter" "--host" "127.0.0.1" "--port" :autoport)
    port :autoport :request "launch" :type "python" :module "pytest"
    :cwd ,(projectile-project-root)
    :args ,args
    :justMyCode nil :console "integratedTerminal" :showReturnValue t :stopOnEntry nil)))

(test-cockpit-register-project-type 'python-pip 'test-cockpit-python-engine)
(test-cockpit-register-project-type-alias 'python-pkg 'python-pip)
(test-cockpit-register-project-type-alias 'python-tox 'python-pip)
(test-cockpit-register-project-type-alias 'python-toml 'python-pip)
(test-cockpit-register-project-type-alias 'python-uv 'python-pip)


(defun test-cockpit-python--test-project-command (_ args)
  "Make the test project command from ARGS."
  (concat (test-cockpit-python--common-switches args)))

(defun test-cockpit-python--test-module-command (string args)
  "Make the test module command from STRING and ARGS."
  (concat (test-cockpit-python--common-switches args) " " string))

(defun test-cockpit-python--choose-module ()
  "Find the current test module."
  (when-let* ((file-name-path (buffer-file-name))
             ((string-prefix-p "test_" (file-name-nondirectory file-name-path))))
    (test-cockpit--strip-project-root file-name-path)))

(defun test-cockpit-python--test-function-command (string args)
  "Make the test function command from STRING and ARGS."
  (concat (test-cockpit-python--common-switches args) " " string))


(defconst test-cockpit-python--builtin-markers
  '("skip" "skipif" "xfail" "parametrize" "usefixtures" "filterwarnings"
    "tryfirst" "trylast")
  "Markers provided by pytest itself or by its builtin plugins.")

(defconst test-cockpit-python--marker-keywords '("and" "or" "not" "(" ")")
  "Keywords and parens permitted in a marker expression.")

(defconst test-cockpit-python--marker-separator "[ \t]+\\|("
  "Regexp determining what separates two terms in a marker expression.")

(defun test-cockpit-python--parse-markers (output)
  "Extract the marker names from the `pytest --markers' output OUTPUT."
  (seq-uniq
   (seq-remove
    (lambda (marker) (or (not marker)
                         (member marker test-cockpit-python--builtin-markers)))
    (seq-map
     (lambda (line)
       (and (string-match "^@pytest\\.mark\\.\\([[:alnum:]_]+\\)" line)
            (match-string 1 line)))
     (split-string output "\n")))))

(defconst test-cockpit--python-uv-prepend-args '("--python=" "--with="))

(defun test-cockpit-python--arg-uv-prepended-p (arg)
  "Non-nil if ARG is a uv prepended argument."
  (seq-find (lambda (cand) (string-prefix-p cand arg)) test-cockpit--python-uv-prepend-args))

(defun test-cockpit-python--cmd-prefix (args)
  "Add command prefix command if ARGS demands it."
  (cond ((member "build_ext" args) (concat test-cockpit-python-build-ext-command " && "))
        ((member "use-uv" args)
         (let ((prepended (append test-cockpit-python-uv-switches
                                  (seq-filter 'test-cockpit-python--arg-uv-prepended-p args))))
           (replace-regexp-in-string " +" " " (concat (string-join `("uv run" . ,prepended) " ") " "))))
        (t "")))

(defun test-cockpit-python--common-switches (args)
  "Extract the common python switches from ARGS."
  (concat (test-cockpit-python--cmd-prefix args)
          "pytest --color=yes"
          (test-cockpit--add-leading-space-to-switches
           (string-join (seq-map 'test-cockpit-python--quote-marker-arg
                                 (seq-remove
                                  (lambda (elt)
                                    (or (member elt '("use-uv" "build_ext"))
                                        (test-cockpit-python--arg-uv-prepended-p elt)))
                                  args))
                        " "))))

(transient-define-argument test-cockpit-python--restrict-substring ()
  :description "Restrict to tests matching string"
  :class 'transient-option
  :key "-k"
  :argument "-k")

(transient-define-argument test-cockpit-python--python-version ()
  :description "use python version (uv)"
  :class 'transient-option
  :argument "--python=")

(transient-define-argument test-cockpit-python--with-package-directive ()
  :description "specify version of python package (uv)"
  :class 'transient-option
  :argument "--with=")

(defun test-cockpit-python--markers-command ()
  "The command to list the markers available in this project."
  (let ((switches (test-cockpit--last-switches)))
    (if (member "use-uv" switches)
        (concat (test-cockpit-python--cmd-prefix
                 (cons "use-uv" (seq-filter 'test-cockpit-python--arg-uv-prepended-p
                                            switches)))
                "pytest --markers")
      "pytest --markers")))

(defun test-cockpit-python--run-markers ()
  "Run `pytest --markers' in the project root and return its raw output."
  (projectile-with-default-dir (projectile-project-root)
    (shell-command-to-string (test-cockpit-python--markers-command))))

(defun test-cockpit-python--available-markers ()
  "The markers that can be used in a marker expression."
  (let ((output (test-cockpit-python--run-markers)))
    (when output
      (test-cockpit-python--parse-markers output))))

(defun test-cockpit-python--marker-candidates (markers)
  "Make the completion candidates from the MARKERS."
  (append test-cockpit-python--marker-keywords
          markers
          (seq-map (lambda (marker) (concat "not " marker)) markers)))

(defun test-cockpit-python--marker-explode (expr)
  "Split the marker expression EXPR into its tokens."
  (split-string
   (concat " " (replace-regexp-in-string ")" " ) " (replace-regexp-in-string "(" " ( " expr)))))

(defun test-cockpit-python--marker-parse-not (tokens)
  "Parse the factor of a marker expression from TOKENS.
Return the yet unparsed tokens on success, an error message string otherwise.
The factor is either a `not' term, a parenthesized expression or a marker name."
  (cond
   ((not tokens) "incomplete marker expression")
   ((equal (car tokens) "not") (test-cockpit-python--marker-parse-not (cdr tokens)))
   ((equal (car tokens) "(")
    (let ((rest (test-cockpit-python--marker-parse-or (cdr tokens))))
      (if (consp rest)
          (if (equal (car rest) ")")
              (cdr rest)
            (format "missing \")\" at \"%s\"" (car rest)))
        (format "unmatched \"(\" at \"%s\"" (or (car rest) "")))))
   ((member (car tokens) '("and" "or"))
    (format "expected a marker term, got \"%s\"" (car tokens)))
   (t (cdr tokens))))

(defun test-cockpit-python--marker-parse-and (tokens)
  "Parse the `and'-joined terms of a marker expression from TOKENS.
Return the yet unparsed tokens on success, an error message string otherwise."
  (let ((rest (test-cockpit-python--marker-parse-not tokens)))
    (while (and (listp rest) (equal (car rest) "and"))
      (setq rest (test-cockpit-python--marker-parse-not (cdr rest))))
    rest))

(defun test-cockpit-python--marker-parse-or (tokens)
  "Parse the `or'-joined terms of a marker expression from TOKENS.
Return the yet unparsed tokens on success, an error message string otherwise."
  (let ((rest (test-cockpit-python--marker-parse-and tokens)))
    (while (and (listp rest) (equal (car rest) "or"))
      (setq rest (test-cockpit-python--marker-parse-and (cdr rest))))
    rest))

(defun test-cockpit-python--marker-expression-error (expr)
  "Return an error message if EXPR is an invalid pytest marker expression.
Return nil if EXPR is blank or a valid marker expression."
  (unless (string-empty-p (string-trim expr))
    (let ((rest (test-cockpit-python--marker-parse-or (test-cockpit-python--marker-explode expr))))
      (cond
       ((stringp rest) rest)
       ((consp rest) (format "unexpected token \"%s\"" (car rest)))
       (t nil)))))

(defun test-cockpit-python--lazy (fn)
  "Return a function that calls FN once and memoizes its result."
  (let (cache)
    (lambda ()
      (or cache (setq cache (funcall fn))))))

(defface test-cockpit-python-invalid-marker
  '((t :inherit error))
  "Face marking an invalid marker selection input in the minibuffer."
  :group 'test-cockpit)

(defvar-local test-cockpit-python--marker-input-overlay nil
  "The overlay highlighting an invalid marker input.")

(defun test-cockpit-python--all-marker-candidates ()
  "The markers of this project, enriched with `not' terms and keywords."
  (test-cockpit-python--marker-candidates
   (or (test-cockpit-python--available-markers) nil)))

(defun test-cockpit-python--marker-prompt (reason)
  "Make the marker expression prompt named by REASON."
  (concat "Marker expression"
          (if reason (format " (Error: %s)" reason) "")
          " (TAB completes markers): "))

(defun test-cockpit-python--marker-completion-table (lazy-candidates)
  "Return a completion table completing the last term of the input.
LAZY-CANDIDATES provides the candidates once they are needed."
  (lambda (string predicate flag)
    (let ((beg 0))
      (while (string-match test-cockpit-python--marker-separator string beg)
        (setq beg (match-end 0)))
      (completion-table-with-context
       (substring string 0 beg)
       (funcall lazy-candidates)
       (substring string beg)
       predicate
       flag))))

(defun test-cockpit-python--highlight-marker-input (_beg _end _len)
  "Validate the marker input and highlight it red if invalid."
  (let ((input (buffer-substring (minibuffer-prompt-end) (point-max))))
    (if (test-cockpit-python--marker-expression-error input)
        (unless test-cockpit-python--marker-input-overlay
          (setq-local test-cockpit-python--marker-input-overlay
                      (make-overlay (minibuffer-prompt-end) (point-max)))
          (overlay-put test-cockpit-python--marker-input-overlay
                       'face 'test-cockpit-python-invalid-marker))
      (when test-cockpit-python--marker-input-overlay
        (delete-overlay test-cockpit-python--marker-input-overlay))
      (setq-local test-cockpit-python--marker-input-overlay nil))))

(defun test-cockpit-python--setup-marker-minibuffer ()
  "Set up the minibuffer to read a marker expression."
  (setq-local completion-list-insert-choice-function
              #'test-cockpit-python--insert-marker-choice)
  (add-hook 'choose-completion-string-functions
            #'test-cockpit-python--choose-marker-completion nil t)
  (add-hook 'after-change-functions
            #'test-cockpit-python--highlight-marker-input nil t)
  (test-cockpit-python--highlight-marker-input (minibuffer-prompt-end)
                                               (point-max) 0))

(defun test-cockpit-python--read-marker (lazy-candidates reason input history)
  "Read the marker expression with the LAZY-CANDIDATES.
REASON is the reason why the current INPUT is invalid.  HISTORY is
for the minibuffer history."
  (minibuffer-with-setup-hook #'test-cockpit-python--setup-marker-minibuffer
    (completing-read (test-cockpit-python--marker-prompt reason)
                     (test-cockpit-python--marker-completion-table lazy-candidates)
                     nil nil input history)))

(defun test-cockpit-python--read-marker-expression (_prompt initial-input history)
  "Read a marker selection expression.
PROMPT, INITIAL-INPUT and HISTORY are provided by the transient infix.
The initial input is re-prompted until it is valid; empty input is valid."
  (let ((lazy-candidates
         (test-cockpit-python--lazy #'test-cockpit-python--all-marker-candidates))
        (input initial-input)
        (reason (and initial-input (not (string-empty-p initial-input))
                     (test-cockpit-python--marker-expression-error initial-input))))
    (catch 'test-cockpit-python--marker-read-done
      (while t
        (setq input (test-cockpit-python--read-marker
                     lazy-candidates reason input history))
        (if (string-empty-p (string-trim input))
            (throw 'test-cockpit-python--marker-read-done "")
          (setq reason (test-cockpit-python--marker-expression-error input))
          (unless reason
            (throw 'test-cockpit-python--marker-read-done input)))))
    input))

(defun test-cockpit-python--insert-marker-choice (start end choice)
  "Insert the completion CHOICE into the marker expression.
START and END are the positions or terms before and after the
completed term in the input, as documented by `choose-completion'."
  (if (and (stringp start) (stringp end))
      (let* ((beg (save-excursion
                    (goto-char (minibuffer-prompt-end))
                    (or (search-forward start nil t)
                        (search-forward-regexp test-cockpit-python--marker-separator nil t)
                        (minibuffer-prompt-end))))
             (end (save-excursion
                    (goto-char (point-max))
                    (or (search-backward end nil t)
                        (progn
                          (goto-char beg)
                          (search-forward-regexp test-cockpit-python--marker-separator nil t))
                        (point-max)))))
        (completion--replace beg end choice))
    (completion--replace start end choice)))

(defun test-cockpit-python--choose-marker-completion (choice buffer base-position &rest _args)
  "Choose the completion CHOICE without exiting the marker input.
BUFFER and BASE-POSITION have the meaning as the argumens of
`choose-completion'."
  (let ((completion-no-auto-exit t)
        (choose-completion-string-functions nil))
    (choose-completion choice buffer base-position)
    t))

(defun test-cockpit-python--quote-marker-arg (arg)
  "Quote the marker expression of the -m ARG to make it shell safe."
  (if (and (string-prefix-p "-m" arg) (length> arg 2))
      (concat "-m" (shell-quote-argument (substring arg 2)))
    arg))



(transient-define-argument test-cockpit-python--marker-switch ()
  :description "marker expression"
  :class 'transient-option
  :key "-m"
  :argument "-m"
  :always-read t
  :reader 'test-cockpit-python--read-marker-expression)

(transient-define-infix test-cockpit-python--choose-loglevel ()
  :class 'transient-switches
  :key "-l"
  :argument-format "--log-level=%s -rA"
  :argument-regexp "\\(--log-level=\\(debug\\|info\\|warn\\|error\\) -rA\\)"
  :choices '("debug" "info" "warn" "error")
  :description "log level")

(transient-define-infix test-cockpit-python--choose-coverage-report ()
  :class 'transient-switches
  :key "-c"
  :argument-format "--cov --cov-report=%s"
  :argument-regexp "\\(--cov --cov-report=\\(term-missing\\|term\\|html\\)\\)"
  :choices '("term-missing" "term" "html")
  :description "log level")

(transient-define-infix test-cockpit-python--choose-verbose-level ()
  :class 'transient-switches
  :key "-v"
  :argument-format "%s"
  :argument-regexp "\\(-v\\|-vv\\|-vvv\\)$"
  :choices '("-v" "-vv" "-vvv")
  :description "verbosity level")

(transient-define-infix test-cockpit-python--choose-traceback ()
  :class 'transient-switches
  :key "-t"
  :argument-format "--tb=%s"
  :argument-regexp "--tb=\\(long\\|short\\|line\\|native\\|no\\)"
  :choices '("long" "short" "line" "native" "no")
  :description "show traceback style")

(defvar test-cockpit-python--using-uv nil)

(defun test-cockpit--use-uv-on-p ()
  "Non-nil if uv is not to be used."
  (member "use-uv" (transient-get-value)))

(defun test-cockpit-python--uv-available ()
  "Non-nil if uv is available."
  (executable-find "uv"))

(defun test-cockpit-python--infix ()
  "Setup the pytest specific test switches."
  (setq test-cockpit-python--using-uv (member "use-uv" (test-cockpit--last-switches)))
  [["Switches"
    ("-k" test-cockpit-python--restrict-substring)
    ("-f" "only lastly failed tests" "--last-failed")
    ("-x" "exit after first fail" "--exitfirst")
    ("-b" "build extensions before testing" "build_ext")
    ("-u" "use `uv run' to run pytest" "use-uv" :if test-cockpit-python--uv-available)
    ("-p" test-cockpit-python--python-version
     :if test-cockpit-python--uv-available
     :inapt-if-not test-cockpit--use-uv-on-p)
    ("-W" test-cockpit-python--with-package-directive
     :if test-cockpit-python--uv-available
     :inapt-if-not test-cockpit--use-uv-on-p)
    ("-m" test-cockpit-python--marker-switch)
    ("-M" "test type hints" "--mypy")]
   ["Output"
    (test-cockpit-python--choose-verbose-level)
    (test-cockpit-python--choose-loglevel)
    (test-cockpit-python--choose-coverage-report)
    ("-r" "report output of passed tests" "-rFP")
    ("-w" "don't output warnings" "--disable-warnings")
    ("-n" "don't capture output" "--capture=no")
    ("-L" "show locals in tracebacks" "--showlocals")
    (test-cockpit-python--choose-traceback)]])

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
  (when-let* ((test-class (test-cockpit-python--class-in-line line)))
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
            (when-let* ((test-function (test-cockpit-python--find-current-test)))
              (concat "::" test-function)))))

(provide 'test-cockpit-python)

;;; test-cockpit-python.el ends here
