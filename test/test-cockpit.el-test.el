;;; test-cockpit.el-test.el --- Tests for test-cockpit.el

(require 'mocker)
(require 'test-cockpit)

(ert-deftest test-register-project-type ()
  (test-cockpit-register-project-type 'foo-project-type
				      'test-project-foo 'test-module-foo 'test-function-foo
				      'foo-infix)
  (should (alist-get 'foo-project-type test-cockpit-project-types)))

(ert-deftest test-register-project-type-alias ()
  (test-cockpit-register-project-type 'foo-project-type
				      'test-project-foo 'test-module-foo 'test-function-foo
				      'foo-infix)
  (test-cockpit-register-project-type-alias 'foo-project-type-alias 'foo-project-type)
  (should (eq (alist-get 'foo-project-type test-cockpit-project-types)
	      (alist-get 'foo-project-type-alias test-cockpit-project-types))))


(ert-deftest test-test-project ()
  (test-cockpit-register-project-type 'foo-project-type
				      (lambda (args) "test project")
				      'test-module-foo
				      'test-function-foo
				      'foo-infix)
  (mocker-let ((projectile-project-type () ((:output 'foo-project-type)))
	       (compile (command) ((:input '("test project") :output 'success))))
	       (test-cockpit-test-project)
	      ))

(ert-deftest test-test-module ()
  (test-cockpit-register-project-type 'foo-project-type
				      'test-project-foo
				      (lambda (args) "test module")
				      'test-function-foo
				      'foo-infix)
  (mocker-let ((projectile-project-type () ((:output 'foo-project-type)))
	       (compile (command) ((:input '("test module") :output 'success))))
	       (test-cockpit-test-module)
	      ))

(ert-deftest test-test-function ()
  (test-cockpit-register-project-type 'foo-project-type
				      'test-project-foo
				      'test-module-foo
				      (lambda (args) "test function")
				      'foo-infix)
  (mocker-let ((projectile-project-type () ((:output 'foo-project-type)))
	       (compile (command) ((:input '("test function") :output 'success))))
	       (test-cockpit-test-function)
	      ))

(ert-deftest test-repeat-test ()
  (test-cockpit-register-project-type 'foo-project-type
				      (lambda (args) "test project")
				      (lambda (args) "test module")
				      'test-function-foo
				      'foo-infix)
  (mocker-let ((projectile-project-type () ((:output 'foo-project-type)))
	       (compile (command) ((:input '("test project") :output 'success :occur 2)
				   (:input '("test module") :output 'success :occur 3))))
	      (test-cockpit-test-project)
	      (test-cockpit-repeat-test)
	      (test-cockpit-test-module)
	      (test-cockpit-repeat-test)
	      (test-cockpit-repeat-test)
	      (should t)))

(ert-deftest test-set-infix ()
  (test-cockpit-register-project-type 'foo-project-type nil nil nil
				      (lambda () ["Foo" ("-f" "foo" "--foo")]))
  (test-cockpit-register-project-type 'bar-project-type nil nil nil
				      (lambda () ["Bar" ("-b" "bar" "--bar")]))
  (mocker-let ((projectile-project-type () ((:output 'foo-project-type :max-occur 1)
					    (:output 'bar-project-type)
					    )))
	      (test-cockpit--insert-infix)
	      (should (equal
		       (aref (transient-get-suffix 'test-cockpit-prefix '(0)) 2)
		       '(:description "Foo")))
	      (should (equal
		       (aref (transient-get-suffix 'test-cockpit-prefix '(1)) 2)
		       '(:description "Run test")))

	      (test-cockpit--insert-infix)
	      (should (equal
		       (aref (transient-get-suffix 'test-cockpit-prefix '(0)) 2)
		       '(:description "Bar")))
	      (should (equal
		       (aref (transient-get-suffix 'test-cockpit-prefix '(1)) 2)
		       '(:description "Run test")))))

(ert-deftest test-set-nil-infix ()
  (test-cockpit-register-project-type 'noinfix-project-type nil nil nil nil)
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


(ert-deftest test-last-test-command ()
  (let ((test-cockpit-last-command nil))
    (mocker-let ((test-cockpit-dispatch () ((:min-occur 1))))
		(test-cockpit-repeat-test))))
;;; test-cockpit.el-test.el ends here
