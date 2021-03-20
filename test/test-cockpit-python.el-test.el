(require 'mocker)
(require 'test-cockpit-python)

(ert-deftest test-project-python-pip-type-available ()
  (should (alist-get 'python-pip test-cockpit-project-types)))

(ert-deftest test-project-python-pkg-type-available ()
  (should (alist-get 'python-pkg test-cockpit-project-types)))

(ert-deftest test-pytest-binary-path-venv ()
  (setq pyvenv-virtual-env "/foo/bar/.venv/")
  (mocker-let
   ((file-executable-p (file) ((:input '("/foo/bar/.venv/bin/pytest") :output t))))
    (should (equal (test-cockpit--python--pytest-binary-path) "/foo/bar/.venv/bin/pytest"))))

(ert-deftest test-pytest-binary-path-no-venv ()
  (setq pyvenv-virtual-env nil)
  (should (equal (test-cockpit--python--pytest-binary-path) "pytest")))

(ert-deftest test-pytest-binary-path-no-pytest-in-venv ()
  (setq pyvenv-virtual-env "/foo/bar/.venv/")
  (mocker-let
   ((file-executable-p (file) ((:input '("/foo/bar/.venv/bin/pytest") :output nil))))
  (should (equal (test-cockpit--python--pytest-binary-path) "pytest"))))

(ert-deftest test-get-python-test-project-command ()
  (mocker-let
   ((projectile-project-type () ((:output 'python-pip)))
    (test-cockpit--python--pytest-binary-path () ((:output "/foo/bin/pytest"))))
   (should (equal (test-cockpit-test-project-command nil)
		  "/foo/bin/pytest --color=yes --cov-report="))))

(ert-deftest test-get-python-test-project-command-switches ()
  (dolist (struct '((("--last-failed" "--baba")
		     "/foo/bin/pytest --color=yes --last-failed --cov-report=")
		    (("--cov-report=term" "--bubu")
		     "/foo/bin/pytest --color=yes --cov-report=term")))
    (let ((arglist (pop struct))
	  (expected (pop struct)))
      (mocker-let
	 ((projectile-project-type () ((:output 'python-pip :occur 1)))
	  (test-cockpit--python--pytest-binary-path () ((:output "/foo/bin/pytest" :occur 1))))
       (should (equal (test-cockpit-test-project-command arglist) expected))))))

(ert-deftest test-get-python-test-module-command ()
  (mocker-let
   ((projectile-project-type () ((:output 'python-pip)))
    (test-cockpit--python--pytest-binary-path () ((:output "pytest")))
    (buffer-file-name () ((:output "foo.py"))))
   (should (equal (test-cockpit-test-module-command nil)
		  "pytest --color=yes --cov-report= -k foo"))))

(ert-deftest test-get-python-test-module-command-switches ()
  (dolist (struct '((("--last-failed" "--baba -k foo")
		     "/foo/bin/pytest --color=yes --last-failed --cov-report= -k foo")
		    (("--cov-report=term" "--bubu -k foo")
		     "/foo/bin/pytest --color=yes --cov-report=term -k foo")))
    (let ((arglist (pop struct))
	  (expected (pop struct)))
      (mocker-let
	 ((projectile-project-type () ((:output 'python-pip :occur 1)))
	  (test-cockpit--python--pytest-binary-path () ((:output "/foo/bin/pytest" :occur 1)))
	  (buffer-file-name () ((:output "foo.py" :occur 1))))
       (should (equal (test-cockpit-test-module-command arglist) expected))))))

(ert-deftest test-get-python-test-function-command ()
  (mocker-let
   ((projectile-project-type () ((:output 'python-pip)))
    (test-cockpit--python--pytest-binary-path () ((:output "pytest")))
    (which-function () ((:output "test_foo"))))
   (should (equal (test-cockpit-test-function-command nil)
		  "pytest --color=yes --cov-report= -k test_foo"))))

(ert-deftest test-get-python-test-function-command-switches ()
  (dolist (struct '((("--last-failed" "--baba -k test_foo")
		     "/foo/bin/pytest --color=yes --last-failed --cov-report= -k test_foo")
		    (("--cov-report=term" "--bubu -k test_foo")
		     "/foo/bin/pytest --color=yes --cov-report=term -k test_foo")))
    (let ((arglist (pop struct))
	  (expected (pop struct)))
      (mocker-let
	 ((projectile-project-type () ((:output 'python-pip :occur 1)))
	  (test-cockpit--python--pytest-binary-path () ((:output "/foo/bin/pytest" :occur 1)))
	  (which-function () ((:output "test_foo" :occur 1))))
       (should (equal (test-cockpit-test-function-command arglist) expected))))))

(ert-deftest test-insert-no-coverage-to-switches ()
  (dolist (struct '((("--last-failed") ("--last-failed" "--cov-report="))
		    (("--last-failed" "--cov-report=term") ("--last-failed" "--cov-report=term"))))
    (let ((arglist (pop struct))
	  (expected (pop struct)))
      (should (equal (test-cockpit--python--insert-no-coverage-to-switches arglist) expected)))))


(ert-deftest test-python-infix ()
  (mocker-let ((projectile-project-type () ((:output 'python-pip))))
    (let ((infix (test-cockpit-infix)))
      (should (equal (aref (aref infix 0) 0) "Switches"))
      (should (equal (aref (aref infix 0) 1) '("-l" "only lastly failed tests" "--last-failed")))
      (should (equal (aref (aref infix 1) 0) "Output"))
      (should (equal (aref (aref infix 1) 1) '("-v" "show single tests" "--verbose")))
      (should (equal (aref (aref infix 1) 2) '("-c" "print coverage report" "--cov-report=term")))
      (should (equal (aref (aref infix 1) 3) '("-r" "report output of passed tests" "-rFP")))
      (should (equal (aref (aref infix 1) 4) '("-w" "don't output warnings" "--disable-warnings"))))))
