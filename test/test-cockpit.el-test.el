;;; test-cockpit.el-test.el --- Tests for test-cockpit.el

(require 'mocker)
(require 'test-cockpit)

(defun tc--register-foo-project ()
  (test-cockpit-register-project-type 'foo-project-type
				      (lambda (args) "test project")
				      (lambda (args) "test module")
				      (lambda (args) "test function")
				      'foo-infix))

(ert-deftest test-register-project-type ()
  (test-cockpit-register-project-type 'foo-project-type
				      'test-project-foo 'test-module-foo 'test-function-foo
				      'foo-infix)
  (should (alist-get 'foo-project-type test-cockpit--project-types)))

(ert-deftest test-register-project-type-alias ()
  (test-cockpit-register-project-type 'foo-project-type
				      'test-project-foo 'test-module-foo 'test-function-foo
				      'foo-infix)
  (test-cockpit-register-project-type-alias 'foo-project-type-alias 'foo-project-type)
  (should (eq (alist-get 'foo-project-type test-cockpit--project-types)
	      (alist-get 'foo-project-type-alias test-cockpit--project-types))))

(ert-deftest test-test-project ()
  (tc--register-foo-project)
  (mocker-let ((projectile-project-type () ((:output 'foo-project-type)))
	       (compile (command) ((:input '("test project") :output 'success))))
    (test-cockpit-test-project)
    ))

(ert-deftest test-test-project-with-args ()
  (tc--register-foo-project)
  (mocker-let ((projectile-project-type () ((:output 'foo-project-type)))
	       (projectile-project-root (&optional _dir) ((:input-matcher (lambda (_) t) :output "foo-project")))
	       (compile (command) ((:input '("test project") :output 'success))))
    (test-cockpit-test-project '("foo" "bar"))
    (should (equal (alist-get "foo-project" test-cockpit--last-switches-alist nil nil 'equal)
		   '("foo" "bar")))))

(ert-deftest test-test-module ()
  (tc--register-foo-project)
  (mocker-let ((projectile-project-type () ((:output 'foo-project-type)))
	       (compile (command) ((:input '("test module") :output 'success))))
    (test-cockpit-test-module)
    ))

(ert-deftest test-test-module-with-args ()
  (tc--register-foo-project)
  (mocker-let ((projectile-project-type () ((:output 'foo-project-type)))
	       (projectile-project-root (&optional _dir) ((:input-matcher (lambda (_) t) :output "foo-project")))
	       (compile (command) ((:input '("test module") :output 'success))))
    (test-cockpit-test-module '("foo" "bar"))
    (should (equal (alist-get "foo-project" test-cockpit--last-switches-alist nil nil 'equal)
		   '("foo" "bar")))))

(ert-deftest test-test-function ()
  (tc--register-foo-project)
  (mocker-let ((projectile-project-type () ((:output 'foo-project-type)))
	       (compile (command) ((:input '("test function") :output 'success))))
    (test-cockpit-test-function)
    ))

(ert-deftest test-test-function-with-args ()
  (tc--register-foo-project)
  (mocker-let ((projectile-project-type () ((:output 'foo-project-type)))
	       (projectile-project-root (&optional _dir) ((:input-matcher (lambda (_) t) :output "foo-project")))
	       (compile (command) ((:input '("test function") :output 'success))))
    (test-cockpit-test-function '("foo" "bar"))
    (should (equal (alist-get "foo-project" test-cockpit--last-switches-alist nil nil 'equal)
		   '("foo" "bar")))))

(ert-deftest test-repeat-test ()
  (tc--register-foo-project)
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

(ert-deftest test-last-test-command-empty-list ()
  (let ((test-cockpit--last-command-alist nil))
    (mocker-let ((test-cockpit-dispatch () ((:min-occur 1))))
      (test-cockpit-repeat-test))))

(ert-deftest test-last-test-command-list-hit ()
  (let ((test-cockpit--last-command-alist '(("foo-project" . "test foo project"))))
    (mocker-let ((projectile-project-root (&optional _dir) ((:input-matcher (lambda (_) t) :output "foo-project")))
		 (compile (command) ((:input '("test foo project") :output 'success :occur 1))))
      (test-cockpit-repeat-test)
      (should t))))

(ert-deftest test-last-test-command-replace ()
  (tc--register-foo-project)
  (let ((test-cockpit--last-command-alist '(("foo-project" . "never to be called"))))
    (mocker-let ((projectile-project-root (&optional _dir) ((:input-matcher (lambda (_) t) :output "foo-project")))
		 (projectile-project-type () ((:output 'foo-project-type)))
		 (compile (command) ((:input '("test project") :output 'success :occur 1))))
      (test-cockpit-test-project)
      (should (eq (length test-cockpit--last-command-alist) 1)))))

(ert-deftest test-last-switches-empty-list ()
  (let ((test-cockpit--last-switches-alist nil))
    (should (eq (test-cockpit--last-switches) nil))))

(ert-deftest test-last-test-switches-list-hit ()
					;(tc--register-foo-project)
  (let ((test-cockpit--last-switches-alist '(("foo-project" . ("foo" "bar")))))
    (mocker-let ((projectile-project-root (&optional _dir) ((:input-matcher (lambda (_) t) :output "foo-project"))))
					;(compile (command) ((:input '("test project") :output 'success)))))
      (should (equal (test-cockpit--last-switches) '("foo" "bar"))))))

(ert-deftest test-last-test-switches-replace ()
  (tc--register-foo-project)
  (let ((test-cockpit--last-switches-alist '(("foo-project" . ("never" "to" "be" "called")))))
    (mocker-let ((projectile-project-root (&optional _dir) ((:input-matcher (lambda (_) t) :output "foo-project")))
		 (projectile-project-type () ((:output 'foo-project-type)))
		 (compile (command) ((:input '("test project") :output 'success :occur 1))))
      (test-cockpit-test-project)
      (should (eq (length test-cockpit--last-switches-alist) 1)))))

;;; test-cockpit.el-test.el ends here
