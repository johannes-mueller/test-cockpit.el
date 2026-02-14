;;; test-cockpit.el-test.el --- Tests for test-cockpit.el -*- lexical-binding: t; -*-

(require 'mocker)
(require 'test-cockpit)

(defclass test-cockpit--foo-engine (test-cockpit--engine)
  ((current-module-string :initarg :current-module-string
                         :initform nil)
   (current-function-string :initarg :current-function-string
                            :initform nil)))

(cl-defmethod test-cockpit--test-project-command ((obj test-cockpit--foo-engine))
  (lambda (_project args) (concat "test project" " " (string-join args " "))))
(cl-defmethod test-cockpit--test-module-command ((obj test-cockpit--foo-engine))
  (lambda (module args) (concat "test module" " " module " " (string-join args " "))))
(cl-defmethod test-cockpit--test-function-command ((obj test-cockpit--foo-engine))
  (lambda (func args) (concat "test function" " " func " " (string-join args " "))))
(cl-defmethod test-cockpit--transient-infix ((obj test-cockpit--foo-engine))
  ["Foo" ("-f" "foo" "--foo")])
(cl-defmethod test-cockpit--engine-current-module-string ((obj test-cockpit--foo-engine))
  (oref obj current-module-string))
(cl-defmethod test-cockpit--engine-current-function-string ((obj test-cockpit--foo-engine))
  (oref obj current-function-string))
(cl-defmethod test-cockpit--engine-switch-filter ((_obj test-cockpit--engine))
  '("filtered=.*"))

(defmacro project-fixture (test-string &rest body)
  (declare (indent 1))
  `(let* ((project-type (intern (concat ,test-string "-project-type")))
          (test-cockpit--project-engines nil)
          (test-cockpit--project-types nil)
          (test-cockpit--project-type-custom-actions
           (assoc-delete-all project-type test-cockpit--project-type-custom-actions))
          (test-cockpit--additional-switch-config nil))
     (test-cockpit-register-project-type project-type 'test-cockpit--foo-engine)
     ,@body))

(defmacro project-fixture-mock (test-string &rest body)
  (declare (indent 1))
  `(let* ((project-type (intern (concat ,test-string "-project-type")))
          (project-root (concat ,test-string "-project")))
     (project-fixture ,test-string
       (mocker-let ((projectile-project-type () ((:output project-type)))
                    (projectile-project-root (&optional dir) ((:input-matcher (lambda (_dir) t) :output project-root :min-occur 0))))
         ,@body))))

(defmacro project-fixture-context (test-string &rest body)
  (declare (indent 1))
  `(project-fixture-mock ,test-string
     (oset (test-cockpit--retrieve-engine) current-module-string (concat ,test-string "-module-string"))
     (oset (test-cockpit--retrieve-engine) current-function-string (concat ,test-string "-function-string"))
     ,@body))

(ert-deftest test-register-project-type-primary ()
  (project-fixture-mock "foo"
    (should (equal (oref (test-cockpit--retrieve-engine) project-type)
                   "foo-project-type"))))

(ert-deftest test-register-project-type-alias ()
  (project-fixture "foo"
    (test-cockpit-register-project-type-alias 'foo-project-type-alias 'foo-project-type)
    (mocker-let ((projectile-project-type () ((:output 'foo-project-type-alias))))
      (should (equal (oref (test-cockpit--retrieve-engine) project-type)
                     "foo-project-type")))))

(ert-deftest test-primary-project-type-not-found ()
  (mocker-let ((projectile-project-type () ((:output 'unknown-project-type))))
    (should-not (test-cockpit--primary-project-type))))

(ert-deftest test-primary-project-type-found-directly ()
  (project-fixture "foo"
    (mocker-let ((projectile-project-type () ((:output 'foo-project-type))))
     (should (equal (test-cockpit--primary-project-type) 'foo-project-type)))))

(ert-deftest test-primary-project-type-found-by-alias ()
  (project-fixture "foo"
    (test-cockpit-register-project-type-alias 'foo-project-type-alias 'foo-project-type)
    (mocker-let ((projectile-project-type () ((:output 'foo-project-type-alias))))
      (should (equal (test-cockpit--primary-project-type) 'foo-project-type)))))

(defclass test-cockpit--dape-engine (test-cockpit--engine)
  ((current-module-string :initarg :current-module-string
                         :initform nil)
   (current-function-string :initarg :current-function-string
                            :initform nil)))

(cl-defmethod test-cockpit--test-project-command ((obj test-cockpit--dape-engine))
  (lambda (_project args) (concat "test project" " " (string-join args " "))))
(cl-defmethod test-cockpit--test-module-command ((obj test-cockpit--dape-engine))
  (lambda (module args) (concat "test module" " " module " " (string-join args " "))))
(cl-defmethod test-cockpit--test-function-command ((obj test-cockpit--dape-engine))
  (lambda (func args) (concat "test function" " " func " " (string-join args " "))))
(cl-defmethod test-cockpit--transient-infix ((obj test-cockpit--dape-engine))
  ["Dape" ("-f" "dape" "--dape")])
(cl-defmethod test-cockpit--engine-current-module-string ((obj test-cockpit--dape-engine))
  (oref obj current-module-string))
(cl-defmethod test-cockpit--engine-current-function-string ((obj test-cockpit--dape-engine))
  (oref obj current-function-string))
(cl-defmethod test-cockpit--engine-dape-last-test-config ((obj test-cockpit--dape-engine))
  'dape-foo-config)

(defun tc--register-dape-project (test-string)
  (setq test-cockpit--project-engines nil)
  (test-cockpit-register-project-type 'dape-project-type 'test-cockpit--dape-engine)
  (mocker-let ((projectile-project-type () ((:output 'dape-project-type :min-occur 0)))
               (projectile-project-root (&optional dir) ((:input-matcher (lambda (_dir) t) :output "dape-project" :min-occur 0))))
    (oset (test-cockpit--retrieve-engine) current-module-string
          (when test-string (concat test-string "-module-string")))
    (oset (test-cockpit--retrieve-engine) current-function-string
          (when test-string (concat test-string "-function-string")))))


(ert-deftest test-current-module-string-dummy ()
  (project-fixture "foo"
    (mocker-let ((projectile-project-type () ((:output 'bar-project-type)))
                 (projectile-project-root (&optional dir) ((:input-matcher (lambda (_dir) t) :output "bar-project"))))
      (should (eq (test-cockpit--current-module-string) nil)))))

(ert-deftest test-current-module-string-foo ()
  (project-fixture-context "foo"
    (should (equal (test-cockpit--current-module-string) "foo-module-string"))))

(ert-deftest test-repeat-module-no-last-module ()
  (project-fixture-mock "foo"
    (mocker-let ((test-cockpit-dispatch () ((:occur 1))))
     (test-cockpit-repeat-module))))

(ert-deftest test-test-last-strings-module-called ()
  (project-fixture-context "foo"
    (mocker-let ((compile (command) ((:input '("test module foo-module-string") :output 'success))))
     (should (equal (test-cockpit--last-module-string) nil))
     (should (equal (test-cockpit--last-function-string) nil))
     (test-cockpit-test-module)
     (should (equal (test-cockpit--last-module-string) "foo-module-string"))
     (should (equal (test-cockpit--last-function-string) "foo-function-string")))))


(ert-deftest test-module-no-current-module-no-last-module ()
  (project-fixture-mock nil
    (mocker-let ((message (msg) ((:input '("Not in a unit test module file") :output 'success))))
      (test-cockpit-test-module))))

(ert-deftest test-module-no-current-module-but-last-module ()
  (project-fixture-mock "bar"
    (mocker-let ((compile (command) ((:input '("test module bar-module-string") :output 'success))))
      (oset (test-cockpit--retrieve-engine) last-module-string "bar-module-string")
      (test-cockpit-test-module)
      (should (equal (test-cockpit--last-module-string) "bar-module-string")))))

(ert-deftest test-function-no-current-function-no-last-function ()
  (project-fixture nil
    (mocker-let ((message (msg) ((:input '("Not in a unit test module file") :output 'success))))
      (test-cockpit-test-function))))

(ert-deftest test-function-no-current-function-but-last-function ()
  (project-fixture-mock "bar"
    (mocker-let ((compile (command) ((:input '("test function bar-function-string") :output 'success))))
      (oset (test-cockpit--retrieve-engine) last-function-string "bar-function-string")
      (test-cockpit-test-function)
      (should (equal (test-cockpit--last-function-string) "bar-function-string")))))


(ert-deftest test-last-module-and-function-remain-after-no-current ()
  (project-fixture-mock "foo"
    (mocker-let ((compile (command) ((:input '("test project") :output 'success))))
      (oset (test-cockpit--retrieve-engine) last-module-string "bar-module-string")
      (oset (test-cockpit--retrieve-engine) last-function-string "bar-function-string")
      (test-cockpit-test-project)
      (should (equal (test-cockpit--last-function-string) "bar-function-string"))
      (should (equal (test-cockpit--last-module-string) "bar-module-string")))))


(ert-deftest test-test-last-strings-module-repeat-called ()
  (project-fixture-context "foo"
    (mocker-let ((compile (command) ((:input '("test module foo-module-string") :output 'success))))
     (test-cockpit-test-module)
     (oset (test-cockpit--retrieve-engine) current-function-string "marker went ...")
     (oset (test-cockpit--retrieve-engine) current-module-string "... somewhere else")
     (test-cockpit-repeat-module)
     (should (equal (test-cockpit--last-function-string) "foo-function-string"))
     (should (equal (test-cockpit--last-module-string) "foo-module-string")))))

(ert-deftest test-current-function-string-dummy ()
  (setq test-cockpit--project-engines nil)
  (mocker-let ((projectile-project-type () ((:output 'bar-project-type)))
               (projectile-project-root (&optional dir) ((:input-matcher (lambda (_dir) t) :output "bar-project"))))
    (should (eq (test-cockpit--current-function-string) nil))))

(ert-deftest test-current-function-string-foo ()
  (project-fixture-context "foo"
    (should (equal (test-cockpit--current-function-string) "foo-function-string"))))

(ert-deftest test-last-function-string-default-nil ()
  (should (eq (test-cockpit--last-function-string) nil)))

(ert-deftest test-repeat-function-no-last-function ()
  (project-fixture-mock "foo"
      (mocker-let ((test-cockpit-dispatch () ((:occur 1))))
        (test-cockpit-repeat-function))))

(ert-deftest test-test-last-strings-function-called ()
  (project-fixture-context "foo"
      (mocker-let ((compile (command) ((:input '("test function foo-function-string") :output 'success))))
        (should (equal (test-cockpit--last-module-string) nil))
        (should (equal (test-cockpit--last-function-string) nil))
        (test-cockpit-test-function)
        (should (equal (test-cockpit--last-module-string) "foo-module-string"))
        (should (equal (test-cockpit--last-function-string) "foo-function-string")))))

(ert-deftest test-test-last-strings-function-repeat-called ()
  (project-fixture-context "foo"
    (mocker-let ((compile (command) ((:input '("test function foo-function-string") :output 'success))))
      (test-cockpit-test-function)
      (oset (test-cockpit--retrieve-engine) current-function-string "marker went ...")
      (oset (test-cockpit--retrieve-engine) current-module-string "... somewhere else")
      (test-cockpit-repeat-function)
      (should (equal (test-cockpit--last-module-string) "foo-module-string"))
      (should (equal (test-cockpit--last-function-string) "foo-function-string")))))

(ert-deftest test-test-last-strings-project-called ()
  (project-fixture-context "foo"
    (mocker-let ((compile (command) ((:input '("test project") :output 'success))))
      (should (equal (test-cockpit--last-module-string) nil))
      (should (equal (test-cockpit--last-function-string) nil))
      (test-cockpit-test-project)
      (should (equal (test-cockpit--last-module-string) "foo-module-string"))
      (should (equal (test-cockpit--last-function-string) "foo-function-string")))))


(ert-deftest test-test-project-no-args-no-working-directory ()
  (project-fixture-mock "foo"
    (mocker-let ((compile (command) ((:input '("test project") :output 'success))))
      (test-cockpit-test-project))))


(ert-deftest test-test-project-no-args-working-directory ()
  (project-fixture-mock "foo"
      (cl-letf (((symbol-function 'compile)
              (lambda (_cmd)
                (should (equal default-directory "foo-project")))))
     (test-cockpit-test-project))))

(ert-deftest test-test-project-cached ()
  (project-fixture-mock "foo"
    (mocker-let ((compile (command) ((:input '("test project") :output 'success))))
      (test-cockpit-test-project)
      (test-cockpit-test-project))))

(ert-deftest test-test-project-with-args ()
  (project-fixture-mock "foo"
      (mocker-let ((compile (command) ((:input '("test project foo bar") :output 'success))))
        (test-cockpit-test-project '("foo" "bar"))
        (should (equal (test-cockpit--last-switches) '("foo" "bar"))))))

(ert-deftest test-test-project-with-filtered-args ()
  (project-fixture-mock "foo"
      (mocker-let ((compile (command) ((:input '("test project foo bar filtered=foo") :output 'success))))
        (test-cockpit-test-project '("foo" "bar" "filtered=foo"))
        (should (equal (test-cockpit--last-switches) '("foo" "bar"))))))

(ert-deftest test-test-module-no-args ()
  (project-fixture-context "foo"
    (mocker-let ((compile (command) ((:input '("test module foo-module-string") :output 'success))))
      (test-cockpit-test-module))))

(ert-deftest test-test-module-with-args ()
  (project-fixture-context "foo"
    (mocker-let ((compile (command) ((:input '("test module foo-module-string foo bar") :output 'success))))
      (test-cockpit-test-module '("foo" "bar"))
      (should (equal (test-cockpit--last-switches) '("foo" "bar"))))))

(ert-deftest test-test-function-no-args ()
  (project-fixture-context "foo"
    (mocker-let ((compile (command) ((:input '("test function foo-function-string") :output 'success))))
      (test-cockpit-test-function))))

(ert-deftest test-test-function-with-args ()
  (project-fixture-context "foo"
    (mocker-let ((compile (command) ((:input '("test function foo-function-string foo bar") :output 'success))))
      (test-cockpit-test-function '("foo" "bar"))
      (should (equal (test-cockpit--last-switches) '("foo" "bar"))))))

(ert-deftest test-transient-repeat-command-no-last-command ()
  (project-fixture-mock "foo"
    (should (eq (test-cockpit--last-interactive-test-command) nil))))


(ert-deftest test-transient-repeat-command-last-cmd-project ()
  (project-fixture-mock "foo"
    (mocker-let ((compile (command) ((:input '("test project") :output 'success))))
      (test-cockpit-test-project)
      (should (equal (test-cockpit--last-interactive-test-command) 'test-cockpit-test-project)))))


(ert-deftest test-transient-repeat-command-last-cmd-module ()
  (project-fixture-context "foo"
    (mocker-let ((compile (command) ((:input '("test module foo-module-string") :output 'success))))
      (test-cockpit-test-module)
      (should (equal (test-cockpit--last-interactive-test-command) 'test-cockpit-test-module)))))

(ert-deftest test-transient-repeat-command-last-cmd-function ()
  (project-fixture-context "foo"
      (mocker-let ((compile (command) ((:input '("test function foo-function-string") :output 'success))))
        (test-cockpit-test-function)
        (should (equal (test-cockpit--last-interactive-test-command) 'test-cockpit-test-function)))))


(ert-deftest test-repeat-test-foo-engine ()
  (project-fixture-context "foo"
    (mocker-let ((compile (command) ((:input '("test project") :output 'success :occur 2)
                                     (:input '("test module foo-module-string") :output 'success :occur 3))))
      (test-cockpit-test-project)
      (test-cockpit-repeat-test)
      (test-cockpit-test-module)
      (test-cockpit-repeat-test)
      (test-cockpit-repeat-test)
      (should t))))

(ert-deftest test-repeat-module-no-last-plain ()
  (project-fixture-mock "foo"
    (mocker-let ((projectile-project-root (&optional dir) ((:input-matcher (lambda (_dir) t) :output "foo-project"))))
     (test-cockpit-repeat-module))))

(ert-deftest test-repeat-module-with-last-no-args ()
  (project-fixture-context "foo"
    (mocker-let ((compile (command) ((:input '("test project") :output 'success :occur 1)
                                     (:input '("test module foo-module-string foo bar") :output 'success :occur 1))))
      (test-cockpit-test-project)
      (test-cockpit--do-repeat-module '("foo" "bar")))))

(ert-deftest test-repeat-module-with-last-with-args ()
  (project-fixture-context "foo"
    (mocker-let ((compile (command) ((:input '("test function foo-function-string") :output 'success :occur 1)
                                     (:input '("test module foo-module-string") :output 'success :occur 1))))
      (test-cockpit-test-function)
      (test-cockpit-repeat-module))))

(ert-deftest test-repeat-module-with-last-with-last-args ()
  (project-fixture-context "foo"
    (mocker-let ((compile (command) ((:input '("test function foo-function-string foo bar") :output 'success :occur 1)
                                     (:input '("test module foo-module-string foo bar") :output 'success :occur 1))))
      (test-cockpit-test-function '("foo bar"))
      (test-cockpit-repeat-module))))

(ert-deftest test-do-repeat-module-with-last-with-no-args ()
  (project-fixture-context "foo"
    (mocker-let ((compile (command) ((:input '("test function foo-function-string foo bar") :output 'success :occur 1)
                                     (:input '("test module foo-module-string") :output 'success :occur 1))))
      (test-cockpit-test-function '("foo bar"))
      (test-cockpit--do-repeat-module nil))))

(ert-deftest test-repeat-function-no-last ()
  (project-fixture-context "foo"
      (test-cockpit-repeat-function)))

(ert-deftest test-repeat-function-with-last-no-args ()
  (project-fixture-context "foo"
      (mocker-let ((compile (command) ((:input '("test project") :output 'success :occur 1)
                                       (:input '("test function foo-function-string") :output 'success :occur 1))))
        (test-cockpit-test-project)
        (test-cockpit-repeat-function))))

(ert-deftest test-repeat-function-with-last-with-args ()
  (project-fixture-context "foo"
    (mocker-let ((compile (command) ((:input '("test module foo-module-string") :output 'success :occur 1)
                                     (:input '("test function foo-function-string bar foo") :output 'success :occur 1))))
      (test-cockpit-test-module)
      (test-cockpit--do-repeat-function '("bar" "foo")))))

(ert-deftest test-repeat-function-with-last-with-last-args ()
  (project-fixture-context "foo"
    (mocker-let ((compile (command) ((:input '("test module foo-module-string bar foo") :output 'success :occur 1)
                                     (:input '("test function foo-function-string bar foo") :output 'success :occur 1))))
      (test-cockpit-test-module '("bar" "foo"))
      (test-cockpit-repeat-function))))

(ert-deftest test-do-repeat-function-with-last-with-no-args ()
  (project-fixture-context "foo"
    (mocker-let ((compile (command) ((:input '("test module foo-module-string bar foo") :output 'success :occur 1)
                                     (:input '("test function foo-function-string") :output 'success :occur 1))))
      (test-cockpit-test-module '("bar" "foo"))
      (test-cockpit--do-repeat-function nil))))

(ert-deftest test-dape-debug-repeat-test--not-available ()
  (project-fixture-context "foo"
    (should-error (test-cockpit-dape-debug-repeat-test))))

(ert-deftest test-dape-debug-repeat-test--available ()
  (tc--register-dape-project "dape")
  (mocker-let ((projectile-project-type () ((:output 'dape-project-type)))
               (dape (config) ((:input '(dape-foo-config) :output 'success))))
    (test-cockpit-dape-debug-repeat-test)))

(ert-deftest test-dape-debug-repeat-test--repeat ()
  (tc--register-dape-project "dape")
  (mocker-let ((projectile-project-type () ((:output 'dape-project-type)))
               (dape (config) ((:input '(dape-foo-config) :output 'success :occur 2))))
    (test-cockpit-dape-debug-repeat-test)
    (test-cockpit-repeat-test)))


(ert-deftest test-custom-action-simple ()
  (mocker-let ((projectile-project-root (&optional dir) ((:input-matcher (lambda (_dir) t) :output "/path/to/project")))
               (buffer-file-name () ((:output "/path/to/project/some/file.el")))
               (compile (command) ((:input '("custom test command") :output 'success))))
    (test-cockpit-dynamic-custom-test-command "custom test command")))


(ert-deftest test-custom-action-repeat ()
  (project-fixture-context "foo"
    (mocker-let ((buffer-file-name () ((:output "/path/to/project/some/file.el")))
                 (compile (command) ((:input '("other custom test action") :output 'success :occur 2))))
      (test-cockpit-dynamic-custom-test-command "other custom test action")
      (test-cockpit-repeat-test))))


(ert-deftest test-custom-action-replace-project-root ()
  (project-fixture-mock "foo"
    (mocker-let ((projectile-project-root (&optional dir) ((:input-matcher (lambda (_dir) t) :output "/path/to/project")))
                 (buffer-file-name () ((:output "/path/to/project/some/file.el")))
                 (compile (command) ((:input '("command /path/to/project") :output 'success :occur 2))))
      (test-cockpit-dynamic-custom-test-command "command %P")
      (test-cockpit-repeat-test))))

(ert-deftest test-custom-action-no-replace-project-root ()
  (project-fixture-mock "foo"
    (mocker-let ((projectile-project-root (&optional dir) ((:input-matcher (lambda (_dir) t) :output "/path/to/project")))
                 (buffer-file-name () ((:output "/path/to/project/some/file.el")))
                 (compile (command) ((:input '("command %Project") :output 'success :occur 2))))
      (test-cockpit-dynamic-custom-test-command "command %%Project")
      (test-cockpit-repeat-test))))


(ert-deftest test-custom-action-replace-absolute-file ()
  (project-fixture-mock "foo"
      (mocker-let ((projectile-project-root (&optional dir) ((:input-matcher (lambda (_dir) t) :output "/path/to/project/")))
                   (buffer-file-name () ((:output "/path/to/project/some/file.el")))
                   (compile (command) ((:input '("command /path/to/project/some/file.el") :output 'success :occur 2))))
        (test-cockpit-dynamic-custom-test-command "command %F")
        (test-cockpit-repeat-test))))


(ert-deftest test-custom-action-no-replace-absolute-file ()
  (project-fixture-mock "foo"
    (mocker-let ((projectile-project-root (&optional dir) ((:input-matcher (lambda (_dir) t) :output "/path/to/project/")))
                 (buffer-file-name () ((:output "/path/to/project/some/file.el")))
                 (compile (command) ((:input '("command %Foo") :output 'success :occur 2))))
      (test-cockpit-dynamic-custom-test-command "command %%Foo")
      (test-cockpit-repeat-test))))


(ert-deftest test-custom-action-replace-relative-file ()
  (project-fixture-mock "foo"
    (mocker-let ((projectile-project-root (&optional dir) ((:input-matcher (lambda (_dir) t) :output "/path/to/project/")))
                 (buffer-file-name () ((:output "/path/to/project/some/file.el")))
                 (compile (command) ((:input '("command some/file.el") :output 'success :occur 2))))
      (test-cockpit-dynamic-custom-test-command "command %f")
      (test-cockpit-repeat-test))))


(ert-deftest test-main-suffix--all-nil ()
  (project-fixture "foo"
    (mocker-let ((test-cockpit--current-module-string () ((:output nil)))
                 (test-cockpit--current-function-string () ((:output nil)))
                 (test-cockpit--last-module-string () ((:output nil)))
                 (test-cockpit--last-function-string () ((:output nil))))
      (should (equal (test-cockpit--main-suffix)
                     ["Run tests"
                      ("p" "project" test-cockpit-test-project)
                      ("c" "custom" test-cockpit-custom-test-command)])))))


(ert-deftest test-main-suffix--one-custom-actions-added ()
  (project-fixture-mock "foo"
      (test-cockpit-add-custom-action
       'foo-project-type "C" "some custom action" "some_custom_action --foo")
    (mocker-let ((test-cockpit--current-module-string () ((:output nil)))
                 (test-cockpit--current-function-string () ((:output nil)))
                 (test-cockpit--last-module-string () ((:output nil)))
                 (test-cockpit--last-function-string () ((:output nil))))
      (should (equal (test-cockpit--main-suffix)
                     [["Run tests"
                       ("p" "project" test-cockpit-test-project)
                       ("c" "custom" test-cockpit-custom-test-command)]
                      ["Custom actions"
                       ("C" "some custom action" (lambda () (interactive)
                                                   (test-cockpit--run-test "some_custom_action --foo")))]])))))


(ert-deftest test-main-suffix--two-custom-actions-added ()
  (project-fixture-mock "foo"
    (test-cockpit-add-custom-action
     'foo-project-type "C" "some strange action" "some_strange_action --foo")
    (test-cockpit-add-custom-action
     'foo-project-type "O" "another custom action" #'some-action-function)
    (mocker-let ((test-cockpit--current-module-string () ((:output nil)))
                 (test-cockpit--current-function-string () ((:output nil)))
                 (test-cockpit--last-module-string () ((:output nil)))
                 (test-cockpit--last-function-string () ((:output nil))))
      (should (equal (test-cockpit--main-suffix)
                     [["Run tests"
                       ("p" "project" test-cockpit-test-project)
                       ("c" "custom" test-cockpit-custom-test-command)]
                      ["Custom actions"
                       ("C" "some strange action" (lambda () (interactive) (test-cockpit--run-test "some_strange_action --foo")))
                       ("O" "another custom action" some-action-function)]])))))


(ert-deftest test-main-suffix--dynamic-custom-actions-added ()
  (project-fixture-mock "foo"
    (test-cockpit-add-dynamic-custom-action
     'foo-project-type "C" "some custom action" "some_custom_action %f --foo")
    (mocker-let ((test-cockpit--current-module-string () ((:output nil)))
                 (test-cockpit--current-function-string () ((:output nil)))
                 (test-cockpit--last-module-string () ((:output nil)))
                 (test-cockpit--last-function-string () ((:output nil))))
      (should (equal (test-cockpit--main-suffix)
                     [["Run tests"
                       ("p" "project" test-cockpit-test-project)
                       ("c" "custom" test-cockpit-custom-test-command)]
                      ["Custom actions"
                       ("C" "some custom action" (lambda () (interactive)
                                                    (test-cockpit-dynamic-custom-test-command "some_custom_action %f --foo")))]])))))


(ert-deftest test-main-suffix--current-module ()
  (project-fixture-mock "foo"
    (mocker-let ((test-cockpit--current-module-string () ((:output "foo-project/some-module")))
                 (test-cockpit--current-function-string () ((:output nil))))
      (should (equal (test-cockpit--main-suffix)
                     ["Run tests"
                      ("p" "project" test-cockpit-test-project)
                      ("m" "module: some-module" test-cockpit-test-module)
                      ("c" "custom" test-cockpit-custom-test-command)])))))


(ert-deftest test-main-suffix--current-function ()
  (project-fixture-mock "foo"
    (mocker-let ((test-cockpit--current-module-string () ((:output nil)))
                 (test-cockpit--current-function-string () ((:output "foo-project/some-function"))))
      (should (equal (test-cockpit--main-suffix)
                     ["Run tests"
                      ("p" "project" test-cockpit-test-project)
                      ("f" "function: some-function" test-cockpit-test-function)
                      ("c" "custom" test-cockpit-custom-test-command)])))))


(ert-deftest test-main-suffix--last-module ()
  (project-fixture-mock "foo"
    (mocker-let ((test-cockpit--current-module-string () ((:output nil)))
                 (test-cockpit--current-function-string () ((:output nil)))
                 (test-cockpit--last-module-string () ((:output "foo-project/some-last-module")))
                 (test-cockpit--last-function-string () ((:output nil))))
      (should (equal (test-cockpit--main-suffix)
                     ["Run tests"
                      ("p" "project" test-cockpit-test-project)
                      ("m" "module: some-last-module" test-cockpit-test-module)
                      ("c" "custom" test-cockpit-custom-test-command)])))))

(ert-deftest test-main-suffix--last-function ()
  (project-fixture-mock "foo"
    (mocker-let ((test-cockpit--current-module-string () ((:output nil)))
                 (test-cockpit--current-function-string () ((:output nil)))
                 (test-cockpit--last-module-string () ((:output nil)))
                 (test-cockpit--last-function-string () ((:output "foo-project/some-last-function"))))
      (should (equal (test-cockpit--main-suffix)
                     ["Run tests"
                      ("p" "project" test-cockpit-test-project)
                      ("f" "function: some-last-function" test-cockpit-test-function)
                      ("c" "custom" test-cockpit-custom-test-command)])))))

(ert-deftest test-main-suffix--repeat-last-project ()
  (project-fixture-mock "foo"
    (mocker-let ((projectile-project-root (&optional dir) ((:input-matcher (lambda (_dir) t) :output "some-project")))
                 (test-cockpit--last-interactive-test-command () ((:output 'test-cockpit-test-project))))
      (should (equal (test-cockpit--main-suffix)
                     ["Run tests"
                      ("p" "project" test-cockpit-test-project)
                      ("c" "custom" test-cockpit-custom-test-command)
                      ("r" "repeat project" test-cockpit-repeat-interactive-test)])))))

(ert-deftest test-main-suffix--repeat-last-module ()
  (project-fixture-mock "foo"
    (mocker-let ((projectile-project-root (&optional dir) ((:input-matcher (lambda (_dir) t) :output "some-project")))
                 (test-cockpit--last-interactive-test-command () ((:output 'test-cockpit-test-module)))
                 (test-cockpit--last-module-string () ((:output "some-project/some-last-module"))))
      (should (equal (test-cockpit--main-suffix)
                     ["Run tests"
                      ("p" "project" test-cockpit-test-project)
                      ("m" "module: some-last-module" test-cockpit-test-module)
                      ("c" "custom" test-cockpit-custom-test-command)
                      ("r" "repeat module: some-last-module" test-cockpit-repeat-interactive-test)])))))

(ert-deftest test-main-suffix--repeat-last-function ()
  (project-fixture-mock "foo"
    (mocker-let ((projectile-project-root (&optional dir) ((:input-matcher (lambda (_dir) t) :output "some-project")))
                 (test-cockpit--last-interactive-test-command () ((:output 'test-cockpit-test-function)))
                 (test-cockpit--last-function-string () ((:output "some-project/some-last-function"))))
      (should (equal (test-cockpit--main-suffix)
                     ["Run tests"
                      ("p" "project" test-cockpit-test-project)
                      ("f" "function: some-last-function" test-cockpit-test-function)
                      ("c" "custom" test-cockpit-custom-test-command)
                      ("r" "repeat function: some-last-function" test-cockpit-repeat-interactive-test)])))))

(ert-deftest test-main-suffix-dape-debug-no-last-test ()
  (tc--register-dape-project "dape")
  (mocker-let ((projectile-project-root (&optional dir) ((:input-matcher (lambda (_dir) t) :output "dape-project")))
               (test-cockpit--current-module-string () ((:output nil)))
               (test-cockpit--current-function-string () ((:output nil)))
               (test-cockpit--last-module-string () ((:output nil)))
               (test-cockpit--last-function-string () ((:output "dape-project/some-last-function")))
               (test-cockpit--last-interactive-test-command () ((:output nil))))
    (should (equal (test-cockpit--main-suffix)
                   ["Run tests"
                    ("p" "project" test-cockpit-test-project)
                    ("f" "function: some-last-function" test-cockpit-test-function)
                    ("c" "custom" test-cockpit-custom-test-command)]))))

(ert-deftest test-main-suffix-dape-debug-with-last-test ()
  (tc--register-dape-project "dape")
  (mocker-let ((projectile-project-root (&optional dir) ((:input-matcher (lambda (_dir) t) :output "dape-project")))
               (test-cockpit--current-module-string () ((:output nil)))
               (test-cockpit--current-function-string () ((:output nil)))
               (test-cockpit--last-module-string () ((:output nil)))
               (test-cockpit--last-function-string () ((:output "dape-project/some-last-function")))
               (test-cockpit--last-interactive-test-command () ((:output 'test-cockpit-test-function))))
    (should (equal (test-cockpit--main-suffix)
                   ["Run tests"
                    ("p" "project" test-cockpit-test-project)
                    ("f" "function: some-last-function" test-cockpit-test-function)
                    ("d" "dape debug repeat" test-cockpit-dape-debug-repeat-test)
                    ("c" "custom" test-cockpit-custom-test-command)
                    ("r" "repeat function: some-last-function" test-cockpit-repeat-interactive-test)]))))

(ert-deftest test-repeat-transient-suffix-nil ()
  (project-fixture "foo"
    (should (eq (test-cockpit--transient-suffix-for-repeat) nil))))


(ert-deftest test-repeat-transient-suffix-non-nil ()
  (project-fixture-mock "foo"
    (mocker-let ((projectile-project-root (&optional dir) ((:input-matcher (lambda (_dir) t) :output "/home/user/projects/foo-project")))
                 (compile (command) ((:input '("test project") :output 'success))))
      (oset (test-cockpit--retrieve-engine) current-module-string "/home/user/projects/foo-project/foo-module")
      (oset (test-cockpit--retrieve-engine) current-function-string "/home/user/projects/foo-project/foo-module::foo-function")
      (should (eq (test-cockpit--transient-suffix-for-repeat) nil))
      (test-cockpit-test-project)
      (should (equal (test-cockpit--transient-suffix-for-repeat)
                     ["Repeat tests"
                      ("M" "last module: foo-module" test-cockpit--do-repeat-module)
                      ("F" "last function: foo-module::foo-function" test-cockpit--do-repeat-function)]))
      (oset (test-cockpit--retrieve-engine) last-module-string nil)
      (should (equal (test-cockpit--transient-suffix-for-repeat)
                     ["Repeat tests"
                      ("F" "last function: foo-module::foo-function" test-cockpit--do-repeat-function)]))
      (test-cockpit-test-project)
      (oset (test-cockpit--retrieve-engine) last-function-string nil)
      (should (equal (test-cockpit--transient-suffix-for-repeat)
                     ["Repeat tests"
                      ("M" "last module: foo-module" test-cockpit--do-repeat-module)])))))

(ert-deftest test-test-or-projectile-build-known-project-type ()
  (project-fixture-mock "foo"
    (mocker-let ((test-cockpit-dispatch () ((:occur 1)))
                 (compile (command) ((:input '("test project foo bar") :output 'success :occur 1))))
      (test-cockpit-test-or-projectile-build)
      (test-cockpit-test-project '("foo" "bar")))
    (mocker-let ((test-cockpit-dispatch () ((:occur 1))))
      (test-cockpit-test-or-projectile-build))))

(ert-deftest test-repeat-test-or-projectile-build-known-project-type ()
  (project-fixture-mock "foo"
    (mocker-let ((test-cockpit-dispatch () ((:occur 1)))
                 (compile (command) ((:input '("test project foo bar") :output 'success :occur 1))))
      (test-cockpit-repeat-test-or-projectile-build)
      (test-cockpit-test-project '("foo" "bar")))
    (mocker-let ((compile (command) ((:input '("test project foo bar") :output 'success :occur 1))))
      (test-cockpit-repeat-test-or-projectile-build))))

(ert-deftest test-test-or-projectile-build-unknown-project-type ()
  (project-fixture "foo"
    (mocker-let ((projectile-project-type () ((:output 'bar-project-type)))
                 (projectile-project-root (&optional dir) ((:input-matcher (lambda (_dir) t) :output "bar-project")))
                 (test-cockpit-dispatch () ((:occur 0)))
                 (projectile-compile-project (arg) ((:input '(nil) :output 'success :occur 2))))
      (let ((compile-command "make all"))
        (should (equal (test-cockpit--last-build-command) nil))
        (test-cockpit-test-or-projectile-build)
        (should (equal (test-cockpit--last-build-command) "make all")))
      (let ((compile-command "make special"))
        (test-cockpit-test-or-projectile-build)
        (should (equal (test-cockpit--last-build-command) "make special"))))))

(ert-deftest test-repeat-test-or-projectile-build-unknown-project-type ()
  (project-fixture "foo"
    (mocker-let ((projectile-project-type () ((:output 'bar-project-type)))
                 (projectile-project-root (&optional dir) ((:input-matcher (lambda (_dir) t) :output "bar-project")))
                 (test-cockpit-dispatch () ((:occur 0)))
                 (projectile-compile-project (arg) ((:input '(nil) :output 'success :max-occur 1)))
                 (compile (command) ((:input '("make all") :output 'success :occur 1))))
      (let ((compile-command "make all"))
        (test-cockpit-test-or-projectile-build)
        (test-cockpit-repeat-test-or-projectile-build)))))

(ert-deftest test-test-or-projectile-test-known-project-type ()
  (project-fixture-mock "foo"
    (mocker-let ((test-cockpit-dispatch () ((:occur 1)))
                 (compile (command) ((:input '("test project foo bar") :output 'success :occur 1))))
      (test-cockpit-test-or-projectile-test)
      (test-cockpit-test-project '("foo" "bar")))
    (mocker-let ((test-cockpit-dispatch () ((:occur 1))))
      (test-cockpit-test-or-projectile-test))))

(ert-deftest test-repeat-test-or-projectile-test-known-project-type ()
  (project-fixture-mock "foo"
    (mocker-let ((test-cockpit-dispatch () ((:occur 1)))
                 (compile (command) ((:input '("test project foo bar") :output 'success :occur 1))))
      (test-cockpit-repeat-test-or-projectile-test)
      (test-cockpit-test-project '("foo" "bar")))
    (mocker-let ((compile (command) ((:input '("test project foo bar") :output 'success :occur 1))))
      (test-cockpit-repeat-test-or-projectile-test))))

(ert-deftest test-test-or-projectile-test-unknown-project-type ()
  (project-fixture "foo"
    (mocker-let ((projectile-project-type () ((:output 'bar-project-type)))
                 (projectile-project-root (&optional dir) ((:input-matcher (lambda (_dir) t) :output "bar-project")))
                 (test-cockpit-dispatch () ((:occur 0)))
                 (projectile-test-project (arg) ((:input '(nil) :output 'success :occur 2))))
      (let ((compile-command "make all"))
        (should (equal (test-cockpit--last-test-command) nil))
        (test-cockpit-test-or-projectile-test)
        (should (equal (test-cockpit--last-test-command) "make all")))
      (let ((compile-command "make special"))
        (test-cockpit-test-or-projectile-test)
        (should (equal (test-cockpit--last-test-command) "make special"))))))

(ert-deftest test-repeat-test-or-projectile-test-unknown-project-type ()
  (mocker-let ((projectile-project-type () ((:output 'bar-project-type :max-occur 1)))
               (projectile-project-root (&optional dir) ((:input-matcher (lambda (_dir) t) :output "bar-project")))
               (test-cockpit-dispatch () ((:occur 0)))
               (projectile-test-project (arg) ((:input '(nil) :output 'success :occur 1)))
               (compile (command) ((:input '("make test") :output 'success :occur 1))))
    (let ((compile-command "make test"))
      (test-cockpit-test-or-projectile-test)
      (test-cockpit-repeat-test-or-projectile-test))))

(ert-deftest test-custom-test-command-compile-command ()
  (project-fixture-mock "foo"
    (mocker-let ((call-interactively (func) ((:input `(compile) :output 'success :occur 1)))
                 (compile (command)
                          ((:input '("some custom command") :output 'success :occur 1))))
      (let ((compile-command "some custom command"))
        (test-cockpit-custom-test-command)
        (test-cockpit-repeat-test)))))

(ert-deftest test-custom-test-memorized-command-called ()
  (project-fixture-mock "foo"
    (mocker-let ((call-interactively (func) ((:input `(compile) :output 'success :occur 1)))
                 (compile (command)
                          ((:input '("memorized command") :output 'success :occur 1)))
                 (test-cockpit--last-custom-command () ((:output "memorized command"))))
      (let ((compile-command "some custom command"))
        (test-cockpit-custom-test-command)
        (test-cockpit-repeat-test)
        (should (equal compile-command "memorized command"))))))

(ert-deftest test-custom-test-memorized-simple ()
  (project-fixture-mock "foo"
      (mocker-let ((call-interactively (func) ((:input `(compile) :output 'success :occur 1)))
                   (compile (command)
                            ((:input '("some custom command") :output 'success :occur 1))))
        (let ((compile-command "some custom command"))
          (test-cockpit-custom-test-command)
          (test-cockpit-repeat-test)
          (should (equal (test-cockpit--last-custom-command) "some custom command"))))))

(ert-deftest test-custom-test-memorized-different-projects ()
  (defvar current-project-root nil)
  (cl-letf (((symbol-function 'current-project-func) (lambda (_cmd) current-project-root)))
    (project-fixture-mock "foo"
      (mocker-let ((projectile-project-root (&optional dir)
                                            ((:input-matcher (lambda (_dir) t) :output-generator 'current-project-func)))
                   (call-interactively (func) ((:input `(compile) :output 'success :occur 4)))
                   (compile (command)
                            ((:input '("some custom command") :output 'success :occur 1)
                             (:input '("other custom command") :output 'success :occur 1)
                             (:input '("other custom command") :output 'success :occur 1)
                             (:input '("some custom command") :output 'success :occur 1))))
        (let ((compile-command "some custom command")
              (current-project-root "foo-project-1"))
          (test-cockpit-custom-test-command)
          (test-cockpit-repeat-test)
          (should (equal (test-cockpit--last-custom-command) "some custom command")))
        (let ((compile-command "other custom command")
              (current-project-root "foo-project-2"))
          (test-cockpit-custom-test-command)
          (test-cockpit-repeat-test)
          (should (equal (test-cockpit--last-custom-command) "other custom command")))
        (let ((current-project-root "foo-project-2"))
          (test-cockpit-custom-test-command)
          (test-cockpit-repeat-test)
          (should (equal (test-cockpit--last-custom-command) "other custom command")))
        (let ((current-project-root "foo-project-1"))
          (test-cockpit-custom-test-command)
          (test-cockpit-repeat-test)
          (should (equal (test-cockpit--last-custom-command) "some custom command")))))))

(ert-deftest test-interactive-repeat-test-no-last-cmd ()
  (project-fixture-mock "foo"
    (mocker-let ((compile (command) ((:input '(_dir) :output nil :occur 0))))
      (test-cockpit-repeat-interactive-test '()))))

(ert-deftest test-interactive-repeat-test-project ()
  (project-fixture-mock "foo"
    (mocker-let ((compile (command) ((:input '("test project foo bar") :output 'success :occur 1)
                                     (:input '("test project other args") :output 'success :occur 1)
                                     (:input '("test project even other args") :output 'success :occur 1))))
      (test-cockpit-test-project '("foo" "bar"))
      (test-cockpit-repeat-interactive-test '("other" "args"))
      (test-cockpit-repeat-interactive-test '("even" "other" "args")))))

(ert-deftest test-interactive-repeat-test-module ()
  (project-fixture-context "foo"
    (mocker-let ((compile (command) ((:input '("test module foo-module-string foo bar") :output 'success :occur 1)
                                     (:input '("test module foo-module-string other args") :output 'success :occur 1)
                                     (:input '("test module foo-module-string even other args") :output 'success :occur 1))))
      (test-cockpit-test-module '("foo" "bar"))
      (test-cockpit-repeat-interactive-test '("other" "args"))
      (test-cockpit-repeat-interactive-test '("even" "other" "args")))))

(ert-deftest test-interactive-repeat-test-function ()
  (project-fixture-context "foo"
    (mocker-let ((compile (command) ((:input '("test function foo-function-string foo bar") :output 'success :occur 1)
                                     (:input '("test function foo-function-string other args") :output 'success :occur 1)
                                     (:input '("test function foo-function-string even other args") :output 'success :occur 1))))
      (test-cockpit-test-function '("foo" "bar"))
      (test-cockpit-repeat-interactive-test '("other" "args"))
      (test-cockpit-repeat-interactive-test '("even" "other" "args")))))

(ert-deftest test-custom-test-command-default-directory ()
  (project-fixture-mock "foo"
    (cl-letf (((symbol-function 'call-interactively)
               (lambda (_cmd) (should (equal default-directory "foo-project")))))
      (test-cockpit-custom-test-command))))

(defun suffix-ref (suffix ref)
  "Hack to make tests work with transient <9 and >=9."
  (let ((offset (if (or (not (boundp 'transient-version))
                        (string-version-lessp transient-version "0.9.0"))
                    1
                  0)))
    (aref suffix (+ ref offset))))

(ert-deftest test-set-infix ()
  (project-fixture-mock "foo"
    (transient-append-suffix 'test-cockpit-prefix '(-1) (test-cockpit--main-suffix))
    (test-cockpit--insert-infix)
    (should (equal
             (suffix-ref (transient-get-suffix 'test-cockpit-prefix '(0)) 1)
             '(:description "Foo")))
    (should (equal
             (suffix-ref (transient-get-suffix 'test-cockpit-prefix '(1)) 1)
             '(:description "Run tests")))))

(defclass test-cockpit--no-infix-engine (test-cockpit--engine) ())

(ert-deftest test-set-nil-infix ()
  (test-cockpit-register-project-type 'noinfix-project-type 'test-cockpit--no-infix-engine)
  (mocker-let ((projectile-project-type () ((:output 'noinfix-project-type)))
               (transient-insert-suffix (prefix loc infix) ((:min-occur 0 :max-occur 0))))
    (transient-append-suffix 'test-cockpit-prefix '(-1) (test-cockpit--main-suffix))
    (test-cockpit--insert-infix)
    (should (equal
             (suffix-ref (transient-get-suffix 'test-cockpit-prefix '(0)) 1)
             '(:description "Run tests")))))

(ert-deftest test-set-infix-project-type-alias ()
  (project-fixture "foo"
    (test-cockpit-register-project-type-alias 'foo-project-type-alias 'foo-project-type)
    (mocker-let ((projectile-project-type () ((:output 'foo-project-type-alias))))
      (should (test-cockpit--infix)))))

(ert-deftest test-join-filter-switches ()
  (let ((allowed '("foo" "bar")))
    (should (equal (test-cockpit--join-filter-switches '( "foo" "bar") allowed) "foo bar"))
    (should (equal (test-cockpit--join-filter-switches '("bar" "boing") allowed) "bar"))))

(ert-deftest test-join-filter-options ()
  (let ((allowed '("-f" "--bar=")))
    (should (equal (test-cockpit--join-filter-switches '("-foo" "--bar=bar" "--baz=baz") allowed)
                   "-foo --bar=bar"))))

(ert-deftest test-add-leading-space-to-switches ()
  (should (equal (test-cockpit--add-leading-space-to-switches "") ""))
  (should (equal (test-cockpit--add-leading-space-to-switches "--foo") " --foo")))

(ert-deftest test-last-test-no-engine-at-first ()
  (project-fixture-mock "foo"
    (mocker-let ((test-cockpit-dispatch () ((:min-occur 1))))
      (test-cockpit-repeat-test))))

(ert-deftest test-last-test-command-no-engine-after-project-switch ()
  (project-fixture-mock "foo"
    (mocker-let ((compile (command) ((:input '("test project foo bar") :output 'success))))
      (test-cockpit-test-project '("foo" "bar"))
      (should (equal (test-cockpit--last-switches) '("foo" "bar")))
      (should (eq (length test-cockpit--project-engines) 1)))
    (mocker-let ((projectile-project-type () ((:output 'foo-project-type)))
                 (projectile-project-root (&optional dir) ((:input-matcher (lambda (_dir) t) :output "other-foo-project")))
                 (test-cockpit-dispatch () ((:min-occur 1))))
      (should (eq (test-cockpit--last-switches) nil))
      (test-cockpit-repeat-test)
      (should (eq (length test-cockpit--project-engines) 2)))
    (should (equal (test-cockpit--last-switches) '("foo" "bar")))
    (should (eq (length test-cockpit--project-engines) 2))))

(ert-deftest test-repeat-test-dummy-engine ()
  (project-fixture "foo"
    (mocker-let ((projectile-project-type () ((:output 'bar-project-type)))
                 (projectile-project-root (&optional dir) ((:input-matcher (lambda (_dir) t) :output "bar-project"))))
      (should-error (test-cockpit-repeat-test)))))

(ert-deftest test-dispatch-dummy-engine ()
  (project-fixture "foo"
    (mocker-let ((projectile-project-type () ((:output 'bar-project-type)))
                 (projectile-project-root (&optional dir) ((:input-matcher (lambda (_dir) t) :output "bar-project"))))
      (should-error (test-cockpit-dispatch)))))

(ert-deftest test-additional-no-additional-switches ()
  (project-fixture "foo"
    (should-not (test-cockpit--additional-switches))))

(ert-deftest test-additional-one-additional-switch ()
  (project-fixture "foo"
    (setq test-cockpit--additional-switch-config nil)
    (test-cockpit-add-additional-switch 'foo-project-type "--some-switch")
    (mocker-let ((projectile-project-type () ((:output 'foo-project-type))))
      (should (equal (test-cockpit--additional-switches) '("--some-switch"))))))

(ert-deftest test-additional-two-additional-switches ()
  (project-fixture "foo"
    (setq test-cockpit--additional-switch-config nil)
    (test-cockpit-add-additional-switch 'foo-project-type "--first-switch")
    (test-cockpit-add-additional-switch 'foo-project-type "--second-switch")
    (mocker-let ((projectile-project-type () ((:output 'foo-project-type))))
      (should (equal (test-cockpit--additional-switches) '("--first-switch" "--second-switch"))))))

(ert-deftest test-additional-on-project-type-alias ()
  (project-fixture "foo"
    (test-cockpit-register-project-type-alias 'foo-project-type-alias 'foo-project-type)
    (test-cockpit-add-additional-switch 'foo-project-type"--some-switch")
    (mocker-let ((projectile-project-type () ((:output 'foo-project-type-alias))))
      (should (equal (test-cockpit--additional-switches) '("--some-switch"))))))

(ert-deftest test-addition-switches-test-project ()
  (project-fixture "foo"
    (test-cockpit-add-additional-switch 'foo-project-type "--some-switch")
    (mocker-let ((projectile-project-type () ((:output 'foo-project-type)))
                 (projectile-project-root (&optional dir) ((:input-matcher (lambda (_dir) t) :output "foo-project")))
                 (compile (command) ((:input '("test project foo bar --some-switch") :output 'success))))
      (test-cockpit-test-project '("foo" "bar")))))

;;; test-cockpit.el-test.el ends here
