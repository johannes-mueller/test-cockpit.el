
(require 'test-cockpit)

(defclass test-cockpit--mix-engine (test-cockpit--engine) ())

(cl-defmethod test-cockpit--test-project-command ((obj test-cockpit--mix-engine))
  'test-cockpit--mix--test-project-command)
(cl-defmethod test-cockpit--test-module-command ((obj test-cockpit--mix-engine))
  'test-cockpit--mix--test-module-command)
(cl-defmethod test-cockpit--test-function-command ((obj test-cockpit--mix-engine))
  'test-cockpit--mix--test-function-command)
(cl-defmethod test-cockpit--transient-infix ((obj test-cockpit--mix-engine))
  (test-cockpit--mix--infix))
(cl-defmethod test-cockpit--engine-current-module-string ((obj test-cockpit--mix-engine))
  (buffer-file-name))
(cl-defmethod test-cockpit--engine-current-function-string ((obj test-cockpit--mix-engine))
  (let ((current-file (buffer-file-name)))
    (when current-file
      (concat current-file ":" (number-to-string (line-number-at-pos))))))


(test-cockpit-register-project-type 'elixir 'test-cockpit--mix-engine)

(defun test-cockpit--mix--apply-switch (command switch)
  (cond ((equal switch "reset") (concat "MIX_ENV=test mix ecto.reset && " command))
	((equal switch "debuglog") (concat "MIX_TEST_LOGLEVEL=debug " command))
	(t (concat command " " switch))))

(defun test-cockpit--mix--apply-switches(command switches)
  (if switches
      (let ((command (test-cockpit--mix--apply-switch command (car switches))))
	(test-cockpit--mix--apply-switches command (cdr switches)))
    command))

(defun test-cockpit--mix--test-project-command (_ switches)
  (test-cockpit--mix--apply-switches "mix test" switches))

(defun test-cockpit--mix--test-module-command (module switches)
  (test-cockpit--mix--apply-switches (concat "mix test " module) switches))

(defun test-cockpit--mix--test-function-command (func switches)
  (test-cockpit--mix--apply-switches (concat "mix test " func) switches))

(defun test-cockpit--mix--infix ()
  ["Mix specific switches"
   ("-r" "Reset Ecto before test" "reset")
   ("-f" "Only lastly failed tests" "--failed")
   ("-d" "Set loglevel to \"debug\"" "debuglog")
   ("-t" "Output trace" "--trace")])

(provide 'test-cockpit-mix)

;;; test-cockpit-mix.el ends here
