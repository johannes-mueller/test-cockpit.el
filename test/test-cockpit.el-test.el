;;; test-cockpit.el-test.el --- Tests for test-cockpit.el

(require 'mocker)
(require 'test-cockpit)

(defclass test-cockpit--foo-engine (test-cockpit--engine) ())

(cl-defmethod test-cockpit--test-project-command ((obj test-cockpit--foo-engine))
  (lambda (args) (concat "test project" " " (string-join args " "))))
(cl-defmethod test-cockpit--test-module-command ((obj test-cockpit--foo-engine))
  (lambda (args) (concat "test module" " " (string-join args " "))))
(cl-defmethod test-cockpit--test-function-command ((obj test-cockpit--foo-engine))
  (lambda (args) (concat "test function" " " (string-join args " "))))
(cl-defmethod test-cockpit--transient-infix ((obj test-cockpit--foo-engine))
  (lambda () ["Foo" ("-f" "foo" "--foo")]))


(defun tc--register-foo-project ()
  (setq test-cockpit--project-engines nil)
  (test-cockpit-register-project-type 'foo-project-type 'test-cockpit--foo-engine))

(ert-deftest test-register-project-type-primary ()
  (tc--register-foo-project)
  (should (alist-get 'foo-project-type test-cockpit--project-types)))

(ert-deftest test-register-project-type-alias ()
  (tc--register-foo-project)
  (test-cockpit-register-project-type-alias 'foo-project-type-alias 'foo-project-type)
  (should (eq (alist-get 'foo-project-type test-cockpit--project-types)
	      (alist-get 'foo-project-type-alias test-cockpit--project-types))))

(ert-deftest test-test-project-no-args ()
  (tc--register-foo-project)
  (mocker-let ((projectile-project-type () ((:output 'foo-project-type)))
	       (projectile-project-root (&optional dir) ((:input-matcher (lambda (_) t) :output "foo-project")))
	       (compile (command) ((:input '("test project") :output 'success))))
    (test-cockpit-test-project)
    ))

(ert-deftest test-test-project-cached ()
  (tc--register-foo-project)
  (mocker-let ((projectile-project-type () ((:output 'foo-project-type :max-occur 1)))
	       (projectile-project-root (&optional dir) ((:input-matcher (lambda (_) t) :output "foo-project")))
	       (compile (command) ((:input '("test project") :output 'success))))
    (test-cockpit-test-project)
    (test-cockpit-test-project)
    ))

(ert-deftest test-test-project-with-args ()
  (tc--register-foo-project)
  (mocker-let ((projectile-project-type () ((:output 'foo-project-type)))
	       (projectile-project-root (&optional _dir) ((:input-matcher (lambda (_) t) :output "foo-project")))
	       (compile (command) ((:input '("test project foo bar") :output 'success))))
    (test-cockpit-test-project '("foo" "bar"))
    (should (equal (test-cockpit--last-switches) '("foo" "bar")))))

(ert-deftest test-test-module-no-args ()
  (tc--register-foo-project)
  (mocker-let ((projectile-project-type () ((:output 'foo-project-type)))
	       (projectile-project-root (&optional _dir) ((:input-matcher (lambda (_) t) :output "foo-project")))
	       (compile (command) ((:input '("test module") :output 'success))))
    (test-cockpit-test-module)
    ))

(ert-deftest test-test-module-with-args ()
  (tc--register-foo-project)
  (mocker-let ((projectile-project-type () ((:output 'foo-project-type)))
	       (projectile-project-root (&optional _dir) ((:input-matcher (lambda (_) t) :output "foo-project")))
	       (compile (command) ((:input '("test module foo bar") :output 'success))))
    (test-cockpit-test-module '("foo" "bar"))
    (should (equal (test-cockpit--last-switches) '("foo" "bar")))))

(ert-deftest test-test-function-no-args ()
  (tc--register-foo-project)
  (mocker-let ((projectile-project-type () ((:output 'foo-project-type)))
	       (projectile-project-root (&optional _dir) ((:input-matcher (lambda (_) t) :output "foo-project")))
	       (compile (command) ((:input '("test function") :output 'success))))
    (test-cockpit-test-function)
    ))

(ert-deftest test-test-function-with-args ()
  (tc--register-foo-project)
  (mocker-let ((projectile-project-type () ((:output 'foo-project-type)))
	       (projectile-project-root (&optional _dir) ((:input-matcher (lambda (_) t) :output "foo-project")))
	       (compile (command) ((:input '("test function foo bar") :output 'success))))
    (test-cockpit-test-function '("foo" "bar"))
    (should (equal (test-cockpit--last-switches) '("foo" "bar")))))

(ert-deftest test-repeat-test ()
  (tc--register-foo-project)
  (mocker-let ((projectile-project-type () ((:output 'foo-project-type)))
	       (projectile-project-root (&optional _dir) ((:input-matcher (lambda (_) t) :output "foo-project")))
	       (compile (command) ((:input '("test project") :output 'success :occur 2)
				   (:input '("test module") :output 'success :occur 3))))
    (test-cockpit-test-project)
    (test-cockpit-repeat-test)
    (test-cockpit-test-module)
    (test-cockpit-repeat-test)
    (test-cockpit-repeat-test)
    (should t)))

(ert-deftest test-test-or-build-known-project-type ()
  (tc--register-foo-project)
  (mocker-let ((projectile-project-type () ((:output 'foo-project-type)))
	       (projectile-project-root (&optional _dir) ((:input-matcher (lambda (_) t) :output "foo-project")))
	       (test-cockpit-dispatch () ((:occur 1)))
	       (compile (command) ((:input '("test project foo bar") :output 'success :occur 1))))
    (test-cockpit-test-or-build)
    (test-cockpit-test-project '("foo" "bar")))
  (mocker-let ((projectile-project-root (&optional _dir) ((:input-matcher (lambda (_) t) :output "foo-project")))
	       (compile (command) ((:input '("test project foo bar") :output 'success :occur 1))))
    (test-cockpit-test-or-build)))

(ert-deftest test-test-or-build-unknown-project-type ()
  (tc--register-foo-project)
  (mocker-let ((projectile-project-type () ((:output 'bar-project-type)))
	       (projectile-project-root (&optional _dir) ((:input-matcher (lambda (_) t) :output "bar-project")))
	       (test-cockpit-dispatch () ((:occur 0)))
	       (projectile-compile-project () ((:input nil :output 'success :occur 1))))
    (test-cockpit-test-or-build)))

(ert-deftest test-set-infix ()
  (tc--register-foo-project)
  (mocker-let ((projectile-project-type () ((:output 'foo-project-type))))
    (test-cockpit--insert-infix)
    (should (equal
	     (aref (transient-get-suffix 'test-cockpit-prefix '(0)) 2)
	     '(:description "Foo")))
    (should (equal
	     (aref (transient-get-suffix 'test-cockpit-prefix '(1)) 2)
	     '(:description "Run test")))
))

(defclass test-cockpit--no-infix-engine (test-cockpit--engine) ())

(ert-deftest test-set-nil-infix ()
  (test-cockpit-register-project-type 'noinfix-project-type 'test-cockpit--no-infix-engine)
  (mocker-let ((projectile-project-type () ((:output 'noinfix-project-type)))
	       (transient-insert-suffix (prefix loc infix) ((:min-occur 0 :max-occur 0))))
    (test-cockpit--insert-infix)
    (should (equal
	     (aref (transient-get-suffix 'test-cockpit-prefix '(0)) 2)
	     '(:description "Run test")))))

(ert-deftest test-join-filter-switches ()
  (let ((allowed '("foo" "bar")))
    (should (equal (test-cockpit--join-filter-switches '("bar" "foo") allowed) "foo bar"))
    (should (equal (test-cockpit--join-filter-switches '("bar" "boing") allowed) "bar"))))

(ert-deftest test-add-leading-space-to-switches ()
  (should (equal (test-cockpit-add-leading-space-to-switches "") ""))
  (should (equal (test-cockpit-add-leading-space-to-switches "--foo") " --foo")))

(ert-deftest test-last-test-no-engine-at-first ()
  (tc--register-foo-project)
  (mocker-let ((projectile-project-type () ((:output 'foo-project-type)))
	       (projectile-project-root (&optional _dir) ((:input-matcher (lambda (_) t) :output "foo-project")))
	       (test-cockpit-dispatch () ((:min-occur 1))))
    (test-cockpit-repeat-test)))

(ert-deftest test-last-test-command-no-engine-after-project-switch ()
  (tc--register-foo-project)
  (mocker-let ((projectile-project-type () ((:output 'foo-project-type)))
	       (projectile-project-root (&optional _dir) ((:input-matcher (lambda (_) t) :output "foo-project")))
	       (compile (command) ((:input '("test project foo bar") :output 'success))))
    (test-cockpit-test-project '("foo" "bar"))
    (should (equal (test-cockpit--last-switches) '("foo" "bar")))
    (should (eq (length test-cockpit--project-engines) 1)))
  (mocker-let ((projectile-project-type () ((:output 'foo-project-type)))
	       (projectile-project-root (&optional _dir) ((:input-matcher (lambda (_) t) :output "bar-project")))
	       (test-cockpit-dispatch () ((:min-occur 1))))
    (should (eq (test-cockpit--last-switches) nil))
    (test-cockpit-repeat-test)
    (should (eq (length test-cockpit--project-engines) 2)))
  (mocker-let ((projectile-project-root (&optional _dir) ((:input-matcher (lambda (_) t) :output "foo-project"))))
    (should (equal (test-cockpit--last-switches) '("foo" "bar")))
    (should (eq (length test-cockpit--project-engines) 2))))


;;; test-cockpit.el-test.el ends here
