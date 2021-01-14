
(require 'transient)
(require 'projectile)
(require 'subr-x)

(defvar test-cockpit-project-types nil)
(defvar test-cockpit-last-command nil)

(defun test-cockpit-register-project-type (project-type
					   test-project-command
					   test-module-command
					   test-function-command
					   transient-infix)
  (let ((type-plist (list :test-project-command test-project-command
			  :test-module-command test-module-command
			  :test-function-command test-function-command
			  :infix transient-infix)))
    (setq test-cockpit-project-types
	  (cons `(,project-type . ,type-plist) test-cockpit-project-types))))

(defun test-cockpit-register-project-type-alias (alias project-type)
  (setq test-cockpit-project-types
	(cons `(,alias . ,(alist-get project-type test-cockpit-project-types)) test-cockpit-project-types)))

(defun test-cockpit--test-struct ()
  (alist-get (projectile-project-type) test-cockpit-project-types)
  )

(defun test-cockpit-test-project-command (args)
  (funcall (plist-get (test-cockpit--test-struct) :test-project-command) args))

(defun test-cockpit-test-module-command (args)
  (funcall (plist-get (test-cockpit--test-struct) :test-module-command) args))

(defun test-cockpit-test-function-command (args)
  (funcall (plist-get (test-cockpit--test-struct) :test-function-command) args))

(defun test-cockpit-infix ()
  (if-let (infix-func (plist-get (test-cockpit--test-struct) :infix))
      (funcall infix-func)))

(defun test-cockpit--insert-infix ()
  (unless (equal (aref (transient-get-suffix 'test-cockpit-prefix '(0)) 2) '(:description "Run test"))
    (transient-remove-suffix 'test-cockpit-prefix '(0)))
  (if-let (infix (test-cockpit-infix))
      (transient-insert-suffix 'test-cockpit-prefix '(0) infix)))

(defun test-cockpit--run-test (command)
  (setq test-cockpit-last-command command)
  (projectile-with-default-dir (projectile-acquire-root)
    (compile command)))

(defun test-cockpit-test-project (&optional args)
  (interactive
   (list (transient-args 'test-cockpit-prefix)))
  (test-cockpit--run-test (test-cockpit-test-project-command args)))

(defun test-cockpit-test-module (&optional args)
  (interactive
   (list (transient-args 'test-cockpit-prefix)))
  (test-cockpit--run-test (test-cockpit-test-module-command args)))

(defun test-cockpit-test-function (&optional args)
  (interactive
   (list (transient-args 'test-cockpit-prefix)))
  (test-cockpit--run-test (test-cockpit-test-function-command args)))

(defun test-cockpit-repeat-test (&optional args)
  (interactive
   (list (transient-args 'test-cockpit-prefix)))
  (test-cockpit--run-test test-cockpit-last-command))

(transient-define-prefix test-cockpit-prefix
  "Test the project"
  ["Run test"
   ("p" "project" test-cockpit-test-project)
   ("m" "module" test-cockpit-test-module)
   ("f" "function" test-cockpit-test-function)
   ("r" "repeat" test-cockpit-repeat-test)])

(defun test-cockpit-dispatch ()
  (interactive)
  (test-cockpit--insert-infix)
  (test-cockpit-prefix)
  )

(defun test-cockpit--join-filter-switches (candidates allowed)
  (mapconcat 'identity
	     (delete 'exclude
		     (mapcar (lambda (sw) (if (member sw candidates) sw 'exclude))
			     allowed))
	     " "))

(provide 'test-cockpit)

;;; test-cockpit.el ends here
