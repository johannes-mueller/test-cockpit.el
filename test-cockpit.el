;;; test-cockpit.el --- A command center to run tests of a software project

;; Author: Johannes Mueller <github@johannes-mueller.org
;; URL: https://github.com/johannes-mueller/test-cockpit.el
;; License: GPLv3

;;; Commentary:

;; test-cockpit aims to be a unified user interface for test runners of different
;; programming languages resp. their testing tools.  There are excellent user interfaces
;; for running tests like python-pytest, but thy are usually special solutions for a
;; specific programming language resp testing tool.  People working with multiple
;; programming languages in their various projects have to deal with different user
;; interfaces to run tests.  That can be annoying.

;; test-cockpit uses transient.el to provide a user interface like the well known git
;; frontend magit does.  There are general commands to run tests that are the same for
;; all programming languages.  Furthermore there are switches or settings, that are
;; specific to some programming language.  test-cockpit uses projectile to guess the
;; type of project and chooses the user interface variant for the specific programming
;; language accordingly.  That way the basic testing commands can be called by the same
;; keybindings for all the supported project types.

;; In this early stage the following programming environments are planned:

;; * Emacs Lisp – cask / ert: basics work
;; * Python – pytest: basics work
;; * Rust – Cargo: basics and feature discovery work
;; * Elixir – mix: planned

;; Each language has its own package to implement the testing of a project.
;; Such packages can be added locally, i. e. without modifying the code of
;; test-cockpit.el.

;;; Code:

(require 'transient)
(require 'projectile)
(require 'subr-x)

(defvar test-cockpit--project-types nil
  "List of known project types.
Project types can be added to the list using
`test-cockpit-register-project-type'.")

(defvar test-cockpit--project-engines nil
  "List of already started engines.
Usually there is one such engine per project that has been
visited during the current session.  An engine is an instance of
a derived class of `test-cockpit--engine'.")

(defclass test-cockpit--engine ()
  ((last-command :initarg :last-command
                 :initform nil)
   (last-switches :initarg :last-switches
                  :initform nil)
   (last-build-command :initarg :last-build-command
                       :initform nil)
   (last-test-command :initarg :last-test-command
                       :initform nil)
   (last-module-string :initarg :last-module-string
                       :initform nil)
   (last-function-string :initarg :last-function-string
                         :initform nil)
   (last-args :initarg :last-args
              :initform nil)
   (is-dummy-engine :initarg :is-dummy-engine
                    :initform nil))
  "The base class for a test-cockpit engine.
For every project type supported by `test-cockpit.el' a derived
class is needed which implements the methods of the base class.")

(cl-defmethod test-cockpit--test-project-command ((obj test-cockpit--engine))
  "Supply the function to be called when the whole project is to be tested.
The function has to have the signature (defun fun (_ ARGS)) where
ARGS is the argument list passed to the test frame work."
  nil)

(cl-defmethod test-cockpit--test-module-command ((obj test-cockpit--engine))
  "Supply the function to be called when a module is to be tested.
The function has to have the signature (defun fun (STRING ARGS))
where STRING is identifies the module, like returned by the
method `test-cockpit--engine-current-module-string' and ARGS is
the argument list passed to the test frame work."
  nil)

(cl-defmethod test-cockpit--test-function-command ((obj test-cockpit--engine))
  "Supply the function to be called when a function is to be tested.
The function has to have the signature (defun fun (STRING ARGS))
where STRING is identifies the function, like returned by the
method `test-cockpit--engine-current-function-string' and ARGS is
the argument list passed to the test frame work."
  nil)

(cl-defmethod test-cockpit--transient-infix ((obj test-cockpit--engine))
  "Supply the transient infix for the project type specific switches."
  nil)

(cl-defmethod test-cockpit--engine-current-module-string ((obj test-cockpit--engine))
  "Supply the string identifying the current module at point."
  nil)

(cl-defmethod test-cockpit--engine-current-function-string ((obj test-cockpit--engine))
  "Supply the string identifying the current function at point."
  nil)

(defun test-cockpit-register-project-type (project-type engine-class)
  "Register a language testing package.
PROJECT-TYPE is the type given by `pojectile-project-type' and
ENGINE-CLASS is a derived class of `test-cockpit-engine'."
  (setq test-cockpit--project-types
        (cons `(,project-type . (lambda () (make-instance ,engine-class)))
              test-cockpit--project-types)))

(defun test-cockpit-register-project-type-alias (alias project-type)
  "Register an alias for a known project type.
Some project types are similar in a way that they can be tested
by the same commands, yet they are different for projectile.  In
those cases the already registered PROJECT-TYPE can be registered
again as ALIAS."
  (setq test-cockpit--project-types
        (cons `(,alias . ,(alist-get project-type test-cockpit--project-types))
              test-cockpit--project-types)))

(defun test-cockpit--make-dummy-engine ()
  "Make a dummy for the case that the project type is not supported.
In those cases we fall back to `porjectile-test-project'."
  (make-instance 'test-cockpit--engine :is-dummy-engine t))

(defun test-cockpit--make-engine ()
  "Make a new engine for the current project type.
If the current project type is not supported a dummy engine is
returned."
  (if-let* ((engine-factory (alist-get (projectile-project-type) test-cockpit--project-types))
            (real-engine (funcall engine-factory)))
      real-engine
    (test-cockpit--make-dummy-engine)))

(defmacro test-cockpit--engine-for-current-project ()
  "Provide the engine for the current project."
  `(alist-get (projectile-project-root) test-cockpit--project-engines nil nil 'equal))

(defun test-cockpit--retrieve-engine ()
  "Retrieve the engine valid for the current project.
If no engine is yet started for the project, it will be started."
  (if-let ((existing-engine (test-cockpit--engine-for-current-project)))
      existing-engine
    (let ((new-engine (test-cockpit--make-engine)))
      (setf (test-cockpit--engine-for-current-project) new-engine)
      new-engine)))

(defun test-cockpit--real-engine-or-error ()
  "Retrieve the engine for the project; error if the project type is unsupported."
  (let ((engine (test-cockpit--retrieve-engine)))
    (if (oref engine is-dummy-engine)
        (signal "Project type %s not supported by test-cockpit or engine not installed" (projectile-project-type))
      engine)))

(defun test-cockpit--make-test-command (method thing args)
  "Setup the test command to be issued.
The supplied METHOD for the current engine is called to get the
actual function to setup the call.  To this function the THING to
be tested as well as additional ARGS are passed.  The resulting
trimmed string is then returned."
  (string-trim (funcall
                (funcall method (test-cockpit--retrieve-engine))
                thing args)))

(defun test-cockpit--update-last-commands (args)
  "Update the attributes for the engine's last test command.
The current module and function are determined automatically.
The additional arguments are shipped as ARGS."
  (let ((engine (test-cockpit--retrieve-engine)))
    (when-let ((last-module (test-cockpit--current-module-string)))
      (oset engine last-module-string last-module))
    (when-let ((last-function (test-cockpit--current-function-string)))
      (oset engine last-function-string last-function))
    (oset engine last-args args)))

(defun test-cockpit--make-test-project-command (project-string args)
  "Call the test-project-command function with ARGS of the current project type.
PROJECT-STRING is usually nil.  The argument is here to make the function
signature compatible with `test-cockpit--make-test-module-command' and
`test-cockpit--make-test-function-command'."
  (test-cockpit--make-test-command 'test-cockpit--test-project-command project-string args))

(defun test-cockpit--make-test-module-command (module-string args)
  "Call the test-module-command function with ARGS of the current project type.
MODULE-STRING is the string determining the module to test."
  (test-cockpit--make-test-command 'test-cockpit--test-module-command module-string args))

(defun test-cockpit--make-test-function-command (func-string args)
  "Call the test-function-command function with ARGS of the current project type.
FUNC-STRING is the string determining the function to test."
  (test-cockpit--make-test-command 'test-cockpit--test-function-command func-string args))

(defun test-cockpit-infix ()
  "Call the infix function of the current project type and return the infix array."
  (test-cockpit--transient-infix (funcall (alist-get (projectile-project-type) test-cockpit--project-types))))

(defun test-cockpit--insert-infix ()
  "Insert the infix array into the transient-prefix."
  (unless (equal (aref (transient-get-suffix 'test-cockpit-prefix '(0)) 2) '(:description "Run test"))
    (transient-remove-suffix 'test-cockpit-prefix '(0)))
  (if-let (infix (test-cockpit-infix))
      (transient-insert-suffix 'test-cockpit-prefix '(0) infix)))

(defun test-cockpit--run-test (command)
  "Run the test command COMMAND and remembers for the case the test is repeated."
  (oset (test-cockpit--retrieve-engine) last-command command)
  (test-cockpit--issue-compile-command command))

(defun test-cockpit--issue-compile-command (command)
  "Issue the compile COMMAND with current project root dir as working dir."
  (projectile-with-default-dir (projectile-acquire-root)
    (compile command)))

(defun test-cockpit--command (func thing args)
  "Setup the compile command to be issued with `compile'.
FUNC is the engine function responsible to setup the
command.  THING is the thing to be tested.  ARGS the additional
arguments."
  (let ((command (funcall func thing args)))
    (oset (test-cockpit--retrieve-engine) last-switches args)
    command))

;;;###autoload
(defun test-cockpit-test-project (&optional args)
  "Test the whole project.
ARGS is the UI state for language specific settings."
  (interactive
   (list (transient-args 'test-cockpit-prefix)))
  (test-cockpit--run-test
   (test-cockpit--command 'test-cockpit--make-test-project-command nil args))
  (test-cockpit--update-last-commands args))

;;;###autoload
(defun test-cockpit-test-module (&optional args)
  "Test the module of the current buffer.
The exact determination of the model is done by the language specific package.
ARGS is the UI state for language specific settings."
  (interactive
   (list (transient-args 'test-cockpit-prefix)))

  (if-let ((module-string (or (test-cockpit--current-module-string)
                              (test-cockpit--last-module-string))))
      (progn (test-cockpit--run-test
              (test-cockpit--command 'test-cockpit--make-test-module-command
                                     module-string
                                     args))
             (test-cockpit--update-last-commands args))
    (message "Not in a unit test module file")))

;;;###autoload
(defun test-cockpit-test-function (&optional args)
  "Run the test function at point.
The exact determination of the function is done by the language
specific package.  ARGS is the UI state for language specific
settings."
  (interactive
   (list (transient-args 'test-cockpit-prefix)))
  (if-let ((function-string (or (test-cockpit--current-function-string)
                              (test-cockpit--last-function-string))))
      (progn (test-cockpit--run-test (test-cockpit--command
                           'test-cockpit--make-test-function-command
                           function-string
                           args))
             (test-cockpit--update-last-commands args))
    (message "Not in a unit test module file")))

;;;###autoload
(defun test-cockpit-repeat-module ()
  "Repeat the module at point when the last test run has been called.
This is useful when you test a certain function, jump out of the
test to fix the issue and then want to run the whole module.
Using this function you can do that without jumping back to the
test code.  If there is no last test module to call, the main
dispatch dialog is invoked."
  (interactive)
  (test-cockpit--do-repeat-module (oref (test-cockpit--retrieve-engine) last-args)))

(defun test-cockpit--do-repeat-module (args)
  "Internal version of `test-cockpit-repeat-module'.
This is called from the transient dialog and accepts ARGS from
there rather than than from the `last-args' attributes of the
engine as when called by `test-cockpit-repeat-module'.  That way
the user can repeat the last module test with different ARGS."
  (interactive
   (list (transient-args 'test-cockpit-prefix)))
  (if (test-cockpit--last-module-string)
      (test-cockpit--run-test (test-cockpit--command
                               'test-cockpit--make-test-module-command
                               (test-cockpit--last-module-string)
                               args)))
  (test-cockpit-dispatch))

;;;###autoload
(defun test-cockpit-repeat-function ()
  "Repeat the function at point when the last test run has been called.
If there is no last test function to call, the main dispatch
dialog is invoked."
  (interactive)
  (test-cockpit--do-repeat-function (oref (test-cockpit--retrieve-engine) last-args)))

;;;###autoload
(defun test-cockpit-custom-test-command ()
  "Run `compile' command interactively to allow a custom test command.
The command run is then stored in as last command of the project
and thus can be repeated using `test-cockpit-repeat-test'."
  (interactive)
  (call-interactively 'compile)
  (oset (test-cockpit--retrieve-engine) last-command compile-command))

(defun test-cockpit--do-repeat-function (args)
  "Internal version of `test-cockpit-repeat-function'.
This is called from the transient dialog and accepts ARGS from
there rather than than from the `last-args' attributes of the
engine as when called by `test-cockpit-repeat-function'.  That way
the user can repeat the last function test with different ARGS."
  (interactive
   (list (transient-args 'test-cockpit-prefix)))
  (if (test-cockpit--last-function-string)
      (test-cockpit--run-test (test-cockpit--command
                               'test-cockpit--make-test-function-command
                               (test-cockpit--last-function-string)
                               args)))
  (test-cockpit-dispatch))

(defun test-cockpit-repeat-test (&optional _args)
  "Repeat the last test if the current project had last test.
If the for the project no test has been run during the current
session, the main dispatch dialog is invoked."
  (interactive
   (list (transient-args 'test-cockpit-prefix)))
  (if-let (last-command (oref (test-cockpit--real-engine-or-error) last-command))
      (test-cockpit--run-test last-command)
    (test-cockpit-dispatch)))

(defun test-cockpit-test-or-projectile-build ()
  "Test or build the project depending on if the project type is supported.
If the project type is supported, test-cockpit-repeat-test is
run.  Otherwise the project build is launched by calling projectile-compile-project."
  (interactive)
  (if (oref (test-cockpit--retrieve-engine) is-dummy-engine)
      (test-cockpit--projectile-build)
    (test-cockpit-dispatch)))

(defun test-cockpit-repeat-test-or-projectile-build ()
  "Repeat the last test or build action (native or projectile).
If the project type is supported, test-cockpit-repeat test is
invoked.  That means, the last test action is repeated or if the
project has not seen a test action during the session, the test
menu is shown.  If the project type is unknown the last build
command is repeated by projectile-build-project a prompt to type a
build command is shown."
  (interactive)
  (if (oref (test-cockpit--retrieve-engine) is-dummy-engine)
      (test-cockpit--repeat-projectile-build)
    (test-cockpit-repeat-test)))

(defun test-cockpit-test-or-projectile-test ()
  "Test the project falling back projectile if project type is not supported.
If the project type is supported, test-cockpit-repeat-test is
run.  Otherwise the project tested calling
projectile-test-project."
  (interactive)
  (if (oref (test-cockpit--retrieve-engine) is-dummy-engine)
      (test-cockpit--projectile-test)
    (test-cockpit-dispatch)))

(defun test-cockpit-repeat-test-or-projectile-test ()
  "Repeat the last test action (native or projectile).
If the project type is supported, test-cockpit-repeat test is
invoked.  That means, the last test action is repeated or if the
project has not seen a test action during the session, the test
menu is shown.  If the project type is unknown the last test
command is repeated by projectile-test-project a prompt to type a
test command is shown."
  (interactive)
  (if (oref (test-cockpit--retrieve-engine) is-dummy-engine)
      (test-cockpit--repeat-projectile-test)
    (test-cockpit-repeat-test)))

(defun test-cockpit--projectile-build (&optional last-command)
  "Launch a projectile driven build process.
If last executed command LAST-COMMAND is given the command is
repeated as is.  Otherwise `projectile-compile-project' is called
interactively and the actual compile command is stored for
repetition."
  (if last-command
      (test-cockpit--issue-compile-command last-command)
    (projectile-compile-project nil)
    (oset (test-cockpit--retrieve-engine) last-build-command compile-command)))

(defun test-cockpit--repeat-projectile-build ()
  "Repeat the last projectile driven build process."
  (test-cockpit--projectile-build (test-cockpit--last-build-command)))

(defun test-cockpit--projectile-test (&optional last-command)
  "Launch a projectile driven test process.
If last executed command LAST-COMMAND is given the command is
repeated as is.  Otherwise `projectile-test-project' is called
interactively and the actual test command is stored for
repetition."
  (if last-command
      (test-cockpit--issue-compile-command last-command)
    (projectile-test-project nil)
    (oset (test-cockpit--retrieve-engine) last-test-command compile-command)))

(defun test-cockpit--repeat-projectile-test ()
  "Repeat the last projectile driven test process."
  (test-cockpit--projectile-test (test-cockpit--last-test-command)))

(defun test-cockpit--current-module-string ()
  "Ask the current engine for the current module string."
  (test-cockpit--engine-current-module-string (test-cockpit--retrieve-engine)))

(defun test-cockpit--last-module-string ()
  "Get the last module string stored in the current engine."
  (oref (test-cockpit--retrieve-engine) last-module-string))

(defun test-cockpit--current-function-string ()
  "Ask the current engine for the current function string."
  (test-cockpit--engine-current-function-string (test-cockpit--retrieve-engine)))

(defun test-cockpit--last-function-string ()
  "Get the last function string stored in the current engine."
  (oref (test-cockpit--retrieve-engine) last-function-string))

(defun test-cockpit--last-build-command ()
  "Get the last build command stored in the current engine."
  (oref (test-cockpit--retrieve-engine) last-build-command))

(defun test-cockpit--last-test-command ()
  "Get the last test command stored in the current engine."
  (oref (test-cockpit--retrieve-engine) last-test-command))

(defun test-cockpit--last-switches ()
  "Get the last switches stored in the current engine."
  (oref (test-cockpit--retrieve-engine) last-switches))

(transient-define-prefix test-cockpit-prefix ()
  "Test the project"
  :value 'test-cockpit--last-switches
  [])

(defun test-cockpit--main-suffix ()
  (let ((module-string (or (test-cockpit--current-module-string) (test-cockpit--last-module-string)))
        (function-string (or (test-cockpit--current-function-string) (test-cockpit--last-function-string)))
        (last-command (oref (test-cockpit--real-engine-or-error) last-command)))
    (vconcat (remove nil (append `("Run tests"
                                   ("p" "project" test-cockpit-test-project)
                                   ,(if module-string
                                        `("m"
                                          ,(format "module: %s" (test-cockpit--strip-project-root module-string))
                                          test-cockpit-test-module))
                                   ,(if function-string
                                        `("f"
                                          ,(format "function: %s" (test-cockpit--strip-project-root function-string))
                                          test-cockpit-test-function))
                                   ("c" "custom" test-cockpit-custom-test-command)
                                   ,(if last-command
                                        `("r" "repeat" test-cockpit-repeat-test))))))))

(defun test-cockpit--strip-project-root (path)
  "Strip the project root path from a given PATH."
  (string-remove-prefix (file-name-as-directory (projectile-project-root)) path))

(defun test-cockpit--transient-suffix-for-repeat ()
  "Setup transient suffix for repeat commands if possible."
  (let ((module-string (test-cockpit--last-module-string))
        (function-string (test-cockpit--last-function-string)))
    (if (or module-string function-string)
        (vconcat (remove nil (append `("Repeat tests"
                                       ,(if module-string
                                            `("M"
                                              ,(format "last module: %s" (test-cockpit--strip-project-root module-string))
                                              test-cockpit--do-repeat-module))
                                       ,(if function-string
                                            `("F"
                                              ,(format "last function: %s" (test-cockpit--strip-project-root function-string))
                                              test-cockpit--do-repeat-function)))))))))

(defun test-cockpit--append-repeat-suffix ()
  "Append the repeat suffix to the transient prefix if possible.
If possible the suffix is returned if not nil."
  (if-let ((repeat-suffix (test-cockpit--transient-suffix-for-repeat)))
      (transient-append-suffix 'test-cockpit-prefix '(-1) repeat-suffix)
    nil))

(defun test-cockpit-dispatch ()
  "Invoke the user interface of to setup and run tests.
If the repeat suffix has been appended it is removed afterwards
as will be no longer valid and we don't want the suffixes to
accumulate."
  (interactive)
  (transient-append-suffix 'test-cockpit-prefix '(-1) (test-cockpit--main-suffix))
  (test-cockpit--real-engine-or-error)
  (test-cockpit--insert-infix)
  (let ((appended-suffix-must-be-removed (test-cockpit--append-repeat-suffix)))
    (test-cockpit-prefix)
    (if appended-suffix-must-be-removed
        (transient-remove-suffix 'test-cockpit-prefix '(-1)))
    (transient-remove-suffix 'test-cockpit-prefix '(-1))))


(defun test-cockpit--join-filter-switches (candidates allowed)
  "Join the list of strings CANDIDATES together.
Candidates not in ALLOWED are excluded.  The items are separated
with a space."
  (string-join
   (delete 'exclude
           (mapcar (lambda (candidate)
                     (if (cl-find-if
                          (lambda (allowed-prefix) (string-prefix-p allowed-prefix candidate)) allowed)
                         candidate
                       'exclude))
                   candidates))
   " "))

(defun test-cockpit--add-leading-space-to-switches (switches)
  "Concat the SWITCHES and add a leading space to the resulting string."
  (if (string-empty-p switches)
      ""
    (concat " " switches)))

(provide 'test-cockpit)

;;; test-cockpit.el ends here
