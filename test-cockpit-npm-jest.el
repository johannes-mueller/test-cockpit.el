(require 'test-cockpit)

(defvar test-cockpit--npm-jest--test-match-regex-list
  '("\\(test\\|spec\\).[jt]sx?"))

(defclass test-cockpit--npm-jest--engine (test-cockpit--engine) ())

(cl-defmethod test-cockpit--test-project-command ((obj test-cockpit--npm-jest--engine))
  'test-cockpit--npm-jest--test-project-command)

(cl-defmethod test-cockpit--test-module-command ((obj test-cockpit--npm-jest--engine))
  'test-cockpit--npm-jest--test-module-command)

(cl-defmethod test-cockpit--engine-current-module-string ((obj test-cockpit--npm-jest--engine))
  (test-cockpit--npm-jest--choose-module))

(cl-defmethod test-cockpit--test-function-command ((obj test-cockpit--npm-jest--engine))
  'test-cockpit--npm-jest--test-function-command)

(cl-defmethod test-cockpit--engine-current-function-string ((obj test-cockpit--npm-jest--engine))
  (test-cockpit--npm-jest--find-current-test))

(cl-defmethod test-cockpit--transient-infix ((obj test-cockpit--npm-jest--engine))
  (test-cockpit--npm-jest--infix))

(test-cockpit-register-project-type 'npm 'test-cockpit--npm-jest--engine)


(defun test-cockpit--npm-jest--infix ()
  [["Switches"
    ("-f" "only lastly failed tests" "--onlyFailures")
    ("-C" "only lastly changed tests" "--onlyChanged")]
   ["Output"
    ("-c" "print coverage report" "--coverage")]])

(defun test-cockpit--npm-jest--test-project-command (_ switches)
  (concat "npm test -- --color " (string-join switches)))

(defun test-cockpit--npm-jest--test-module-command (_ switches)
  (concat "npm test -- --color "
	  "--testPathPattern '"(regexp-quote (test-cockpit--npm-jest--choose-module))"' "
	  (string-join switches " ")))

(defun test-cockpit--npm-jest--test-function-command (_ switches)
  (concat "npm test -- --color "
	  "--testPathPattern '"(regexp-quote (test-cockpit--npm-jest--choose-module))"' "
	  "--testNamePattern '"(regexp-quote (test-cockpit--npm-jest--find-current-test))"' "
	  (string-join switches " ")))

(defun test-cockpit--npm-jest--choose-module ()
  (let ((candidate (buffer-file-name)))
    (when (and candidate
	       (cl-find-if
		(lambda (pattern) (string-match-p pattern candidate))
	    test-cockpit--npm-jest--test-match-regex-list))
      candidate)))

(defun test-cockpit--npm-jest--find-current-it ()
  (when (search-backward-regexp "\\(test\\|it\\)(\\(['`\"]\\)\\(.*\\)\\2" nil t)
    `(,(match-beginning 0) ,(match-string 3))))

(defun test-cockpit--npm-jest--find-current-desc ()
  (save-excursion
    (when (search-backward-regexp "describe(\\(['`\"]\\)\\(.*\\)\\1" nil t)
      `(,(match-beginning 0) ,(concat (match-string 2) " ")))))

(defun test-cockpit--npm-jest--find-current-test ()
  (save-excursion
    (let ((desc (test-cockpit--npm-jest--find-current-desc))
	 (it (test-cockpit--npm-jest--find-current-it)))
     (when (or desc it)
       (concat (cadr desc)
	       (if (or (not desc)
		       (and it (> (car it) (car desc))))
		   (cadr it)))))))

(provide 'test-cockpit-npm-jest)

;;; test-cockpit-npm-jest.el ends here
