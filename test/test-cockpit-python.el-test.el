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
		  "/foo/bin/pytest"))))

(ert-deftest test-get-python-test-module-command ()
  (mocker-let
   ((projectile-project-type () ((:output 'python-pip)))
    (test-cockpit--python--pytest-binary-path () ((:output "pytest")))
    (buffer-file-name () ((:output "foo.py"))))
   (should (equal (test-cockpit-test-module-command nil)
		  "pytest -k foo"))))

(ert-deftest test-get-python-test-function-command ()
  (mocker-let
   ((projectile-project-type () ((:output 'python-pip)))
    (test-cockpit--python--pytest-binary-path () ((:output "pytest")))
    (which-function () ((:output "test_foo"))))
   (should (equal (test-cockpit-test-function-command nil)
		  "pytest -k test_foo"))))
