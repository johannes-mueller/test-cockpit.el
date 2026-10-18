;;; test-cockpit-python.el-test.el --- Tests for test-cockpit.el -*- lexical-binding: t; -*-

(require 'mocker)
(require 'test-cockpit-python)

(ert-deftest test-python-project-python-pip-type-available ()
  (should (test-cockpit--primary-project-type 'python-pip)))

(ert-deftest test-python-project-python-pkg-type-available ()
  (should (test-cockpit--primary-project-type 'python-pkg)))

(ert-deftest test-python-project-python-tox-type-available ()
  (should (test-cockpit--primary-project-type 'python-tox)))

(ert-deftest test-python-project-python-toml-type-available ()
  (should (test-cockpit--primary-project-type 'python-toml)))

(ert-deftest test-python-current-module-string-no-file-buffer-is-nil ()
  (mocker-let ((buffer-file-name () ((:output nil))))
    (let ((engine (make-instance test-cockpit-python-engine)))
      (should (eq (test-cockpit--engine-current-module-string engine) nil)))))

(ert-deftest test-python-current-function-string-no-file-buffer-is-nil ()
  (mocker-let ((buffer-file-name () ((:output nil))))
    (let ((engine (make-instance test-cockpit-python-engine)))
      (should (eq (test-cockpit--engine-current-function-string engine) nil)))))

(ert-deftest test-python-get-python-test-project-command-no-switches ()
  (setq test-cockpit--project-engines nil)
  (mocker-let
   ((projectile-project-type () ((:output 'python-pip :min-occur 0)))
    (projectile-project-root (&optional dir) ((:input-matcher (lambda (_dir) t) :output "foo-project")))
    (buffer-file-name () ((:output "/home/user/project/tests/path/to/test_foo.py")))
    (compile (command) ((:input '("pytest --color=yes") :output 'success :occur 1))))
   (test-cockpit-test-project)))

(ert-deftest test-python-get-python-test-project-command-switches ()
  (setq test-cockpit--project-engines nil)
  (mocker-let
      ((projectile-project-type () ((:output 'python-pip)))
       (projectile-project-root (&optional dir) ((:input-matcher (lambda (_dir) t) :output "foo-project")))
       (buffer-file-name () ((:output "/home/user/project/tests/path/to/test_foo.py"))))
    (dolist (struct '((("--cov --cov-report=term-missing" "-mslow")
                       "pytest --color=yes --cov --cov-report=term-missing -mslow")
                      (("--cov --cov-report=term-missing" "-mnot (slow or fast)")
                       "pytest --color=yes --cov --cov-report=term-missing -mnot\\ \\(slow\\ or\\ fast\\)")))
      (let ((arglist (pop struct))
            (expected (pop struct)))
        (mocker-let
            ((compile (command) ((:input `(,expected) :output 'success :occur 1))))
          (test-cockpit-test-project arglist))))))

(ert-deftest test-python-get-python-test-module-command-no-switches ()
  (setq test-cockpit--project-engines nil)
  (mocker-let
   ((projectile-project-type () ((:output 'python-pip)))
    (projectile-project-root (&optional dir) ((:input-matcher (lambda (_dir) t) :output "/home/user/project")))
    (buffer-file-name () ((:output "/home/user/project/tests/path/to/test_foo.py")))
    (compile (command) ((:input '("pytest --color=yes tests/path/to/test_foo.py") :output 'success :occur 1))))
   (test-cockpit-test-module)))

(ert-deftest test-python-get-python-module-no-pytest-file ()
  (setq test-cockpit--project-engines nil)
  (mocker-let
      ((projectile-project-type () ((:output 'python-pip :min-occur 0)))
       (projectile-project-root (&optional dir) ((:input-matcher (lambda (_dir) t) :output "foo-project")))
       (buffer-file-name () ((:output "/home/user/project/path/to/foo.py"))))
    (should (eq (test-cockpit--current-module-string) nil))))

(ert-deftest test-python-get-python-function-no-pytest-file ()
  (setq test-cockpit--project-engines nil)
  (mocker-let
      ((projectile-project-type () ((:output 'python-pip :min-occur 0)))
       (projectile-project-root (&optional dir) ((:input-matcher (lambda (_dir) t) :output "foo-project")))
       (buffer-file-name () ((:output "/home/user/project/path/to/foo.py"))))
    (should (eq (test-cockpit--current-function-string) nil))))

(ert-deftest test-python-get-python-test-module-command-switches ()
  (setq test-cockpit--project-engines nil)
  (mocker-let
      ((projectile-project-type () ((:output 'python-pip)))
       (projectile-project-root (&optional dir) ((:input-matcher (lambda (_dir) t) :output "bar-project")))
       (buffer-file-name () ((:output "/home/user/project/path/to/test_foo.py"))))
    (dolist (struct '((("--last-failed")
                       "pytest --color=yes --last-failed /home/user/project/path/to/test_foo.py")
                      (("--cov --cov-report=term-missing")
                       "pytest --color=yes --cov --cov-report=term-missing /home/user/project/path/to/test_foo.py")))
      (let ((arglist (pop struct))
            (expected (pop struct)))
        (mocker-let
            ((compile (command) ((:input `(,expected) :output 'success :occur 1))))
          (test-cockpit-test-module arglist))))))

(ert-deftest test-python-get-python-test-function-command-no-switches ()
  (setq test-cockpit--project-engines nil)
  (mocker-let
   ((projectile-project-type () ((:output 'python-pip)))
    (projectile-project-root (&optional dir) ((:input-matcher (lambda (_dir) t) :output "foo-project")))
    (test-cockpit-python--test-function-path () ((:output "test_foo")))
    (buffer-file-name () ((:output "/home/user/project/path/to/foo.py")))
    (compile (command) ((:input '("pytest --color=yes test_foo") :output 'success :occur 1))))
   (test-cockpit-test-function)))

(ert-deftest test-python-get-python-test-function-command-switches ()
  (setq test-cockpit--project-engines nil)
  (mocker-let
      ((projectile-project-type () ((:output 'python-pip)))
       (projectile-project-root (&optional dir) ((:input-matcher (lambda (_dir) t) :output "foo-bar-project")))
       (test-cockpit-python--test-function-path () ((:output "test_foo")))
       (buffer-file-name () ((:output "/home/user/project/path/to/foo.py"))))
    (dolist (struct '((("--last-failed")
                       "pytest --color=yes --last-failed test_foo")
                      (("--cov --cov-report=term-missing")
                       "pytest --color=yes --cov --cov-report=term-missing test_foo")))
      (let ((arglist (pop struct))
            (expected (pop struct)))
        (mocker-let
            ((compile (command) ((:input `(,expected) :output 'success :occur 1))))
          (test-cockpit-test-function arglist))))))

(ert-deftest test-python-python-build-ext-switch-default-command ()
  (setq test-cockpit--project-engines nil)
  (mocker-let
      ((projectile-project-type () ((:output 'python-pip)))
       (projectile-project-root (&optional dir) ((:input-matcher (lambda (_dir) t) :output "foo-project")))
       (buffer-file-name () ((:output "/home/user/project/tests/path/to/test_foo.py")))
       (compile (command) ((:input '("pip install -e . --no-deps && pytest --color=yes --last-failed")
                                   :output 'success :occur 1))))
    (test-cockpit-test-project '("--last-failed" "build_ext"))))

(ert-deftest test-python-python-build-ext-switch-changed-command ()
  (setq test-cockpit--project-engines nil)
  (let ((test-cockpit-python-build-ext-command "foo build-ext command"))
    (mocker-let ((projectile-project-type () ((:output 'python-pip)))
                 (projectile-project-root (&optional dir) ((:input-matcher (lambda (_dir) t) :output "foo-project")))
                 (buffer-file-name () ((:output "/home/user/project/tests/path/to/test_foo.py")))
                 (compile (command) ((:input '("foo build-ext command && pytest --color=yes --last-failed")
                                      :output 'success :occur 1))))
      (test-cockpit-test-project '("--last-failed" "build_ext")))))

(ert-deftest test-python-uv-prefixed-no-python-version ()
  (setq test-cockpit--project-engines nil)
  (mocker-let ((projectile-project-type () ((:output 'python-pip)))
               (projectile-project-root (&optional dir) ((:input-matcher (lambda (_dir) t) :output "foo-project")))
               (buffer-file-name () ((:output "/home/user/project/tests/path/to/test_foo.py")))
               (compile (command) ((:input '("uv run --all-extras --all-groups pytest --color=yes --last-failed")
                                    :output 'success :occur 1))))
    (test-cockpit-test-project '("--last-failed" "use-uv"))))

(ert-deftest test-python-uv-prefixed-python-version ()
  (setq test-cockpit--project-engines nil)
  (mocker-let ((projectile-project-type () ((:output 'python-pip)))
               (projectile-project-root (&optional dir) ((:input-matcher (lambda (_dir) t) :output "foo-project")))
               (buffer-file-name () ((:output "/home/user/project/tests/path/to/test_foo.py")))
               (compile (command) ((:input '("uv run --all-extras --all-groups --python=3.14 pytest --color=yes --last-failed")
                                    :output 'success :occur 1))))
    (test-cockpit-test-project '("--last-failed" "use-uv" "--python=3.14"))))

(ert-deftest test-python-uv-prefixed-with-directive ()
  (setq test-cockpit--project-engines nil)
  (mocker-let ((projectile-project-type () ((:output 'python-pip)))
               (projectile-project-root (&optional dir) ((:input-matcher (lambda (_dir) t) :output "foo-project")))
               (buffer-file-name () ((:output "/home/user/project/tests/path/to/test_foo.py")))
               (compile (command) ((:input '("uv run --all-extras --all-groups --with=pandas==2.3.3 pytest --color=yes --last-failed")
                                    :output 'success :occur 1))))
    (test-cockpit-test-project '("--last-failed" "use-uv" "--with=pandas==2.3.3"))))

(ert-deftest test-python-uv-prefixed-with-both-python-version-and-withdirective ()
  (setq test-cockpit--project-engines nil)
  (mocker-let ((projectile-project-type () ((:output 'python-pip)))
               (projectile-project-root (&optional dir) ((:input-matcher (lambda (_dir) t) :output "foo-project")))
               (buffer-file-name () ((:output "/home/user/project/tests/path/to/test_foo.py")))
               (compile (command) ((:input '("uv run --all-extras --all-groups --python=3.14 --with=pandas==2.3.3 pytest --color=yes --last-failed")
                                    :output 'success :occur 1))))
    (test-cockpit-test-project '("--last-failed" "use-uv" "--python=3.14" "--with=pandas==2.3.3"))))

(ert-deftest test-python-uv-prefixed-modified-switches ()
  (setq test-cockpit--project-engines nil)
  (mocker-let ((projectile-project-type () ((:output 'python-pip)))
               (projectile-project-root (&optional dir) ((:input-matcher (lambda (_dir) t) :output "foo-project")))
               (buffer-file-name () ((:output "/home/user/project/tests/path/to/test_foo.py")))
               (compile (command) ((:input '("uv run --foo --bar pytest --color=yes --last-failed")
                                    :output 'success :occur 1))))
    (let ((test-cockpit-python-uv-switches '("--foo" "--bar")))
      (test-cockpit-test-project '("--last-failed" "use-uv")))))

(ert-deftest test-python-dape-last-test-project ()
  (setq test-cockpit--project-engines nil)
  (mocker-let
      ((projectile-project-type () ((:output 'python-pip)))
       (projectile-project-root (&optional dir) ((:input-matcher (lambda (_dir) t) :output "foo-project")))
       (compile (command) ((:input-matcher (lambda (_dir) t) :output 'success))))
    (test-cockpit-test-project)
    (should (equal (test-cockpit--dape-debug-last-test)
                   '(command "python"
                     command-args ("-m" "debugpy.adapter" "--host" "127.0.0.1" "--port" :autoport)
                     port :autoport :request "launch" :type "python" :module "pytest"
                     :cwd "foo-project"
                     :args []
                     :justMyCode nil :console "integratedTerminal" :showReturnValue t :stopOnEntry nil)))))

(ert-deftest test-python-dape-last-test-module ()
  (setq test-cockpit--project-engines nil)
  (mocker-let
      ((projectile-project-type () ((:output 'python-pip)))
       (projectile-project-root (&optional dir) ((:input-matcher (lambda (_dir) t) :output "/home/user/project")))
       (buffer-file-name () ((:output "/home/user/project/tests/path/to/test_foo.py")))
       (compile (command) ((:input-matcher (lambda (_dir) t) :output 'success))))
    (test-cockpit-test-module)
    (let ((config (test-cockpit--dape-debug-last-test)))
      (should (equal (plist-get config :cwd) "/home/user/project"))
      (should (equal (plist-get config :args) ["tests/path/to/test_foo.py"])))))

(ert-deftest test-python-dape-last-test-function-no-switches ()
  (setq test-cockpit--project-engines nil)
  (mocker-let
      ((projectile-project-type () ((:output 'python-pip)))
       (projectile-project-root (&optional dir) ((:input-matcher (lambda (_dir) t) :output "/home/user/project")))
       (test-cockpit-python--test-function-path () ((:output "test_foo")))
       (compile (command) ((:input-matcher (lambda (_dir) t) :output 'success))))
    (test-cockpit-test-function)
    (let ((config (test-cockpit--dape-debug-last-test)))
      (should (equal (plist-get config :cwd) "/home/user/project"))
      (should (equal (plist-get config :args) ["test_foo"])))))

(ert-deftest test-python-dape-last-test-function-switches ()
  (setq test-cockpit--project-engines nil)
  (mocker-let
      ((projectile-project-type () ((:output 'python-pip)))
       (projectile-project-root (&optional dir) ((:input-matcher (lambda (_dir) t) :output "/home/user/project")))
       (test-cockpit-python--test-function-path () ((:output "test_foo")))
       (compile (command) ((:input-matcher (lambda (_dir) t) :output 'success))))
    (test-cockpit-test-function '("--verbose" "--capture=no"))
    (let ((config (test-cockpit--dape-debug-last-test)))
      (should (equal (plist-get config :cwd) "/home/user/project"))
      (should (equal (plist-get config :args) ["test_foo" "--verbose" "--capture=no"])))))


(ert-deftest test-python-find-test-method-simple ()
  (let ((buffer-contents "
def test_first_outer():
    pass

def no_test_outer():
    pass
"))
    (dolist (struct '((1 nil)
                      (25 "test_first_outer")
                      (60 nil)))
      (let ((init-pos (pop struct))
            (expected-string (pop struct))
            (buf (get-buffer-create "test-buffer")))
        (with-current-buffer buf
          (erase-buffer)
          (insert buffer-contents)
          (goto-char init-pos)
          (should (equal (test-cockpit-python--find-current-test) expected-string)))))))

(ert-deftest test-python-find-test-method-point-in-def ()
  (let ((buffer-contents "
def test_first_2342_outer():
    pass
")
        (init-pos 3)
        (expected-string "test_first_2342_outer")
        (buf (get-buffer-create "test-buffer")))
    (with-current-buffer buf
      (erase-buffer)
      (insert buffer-contents)
      (goto-char init-pos)
      (should (equal (test-cockpit-python--find-current-test) expected-string)))))

(ert-deftest test-python-find-test-method-point-in-def-numeric-start ()
  (let ((buffer-contents "
def test_2342_first_outer():
    pass
")
        (init-pos 3)
        (expected-string "test_2342_first_outer")
        (buf (get-buffer-create "test-buffer")))
    (with-current-buffer buf
      (erase-buffer)
      (insert buffer-contents)
      (goto-char init-pos)
      (should (equal (test-cockpit-python--find-current-test) expected-string)))))


(ert-deftest test-python-find-test-method-constant-pos ()
  (let ((buffer-contents "
def test_first_outer():
    pass

def no_test_outer():
    pass
")
        (init-pos 25)
        (buf (get-buffer-create "test-buffer")))
    (with-current-buffer buf
      (insert buffer-contents)
      (goto-char init-pos)
      (test-cockpit-python--find-current-test)
      (should (eq (point) init-pos)))))


(ert-deftest test-python-find-test-method-nested ()
  (let ((buffer-contents "
def test_first_outer():

    def inner():
        pass

    pass
"))
    (dolist (struct '((1 nil)
                      (25 "test_first_outer")
                      (42 "test_first_outer")))
      (let ((init-pos (pop struct))
            (expected-string (pop struct))
            (buf (get-buffer-create "test-buffer")))
        (with-current-buffer buf
          (erase-buffer)
          (insert buffer-contents)
          (goto-char init-pos)
          (should (equal (test-cockpit-python--find-current-test) expected-string)))))))


(ert-deftest test-python-find-test-method-in-test-class ()
  (let ((buffer-contents "
class TestClass(TestCase):

    def test_in_class(self):
        pass

    pass
"))
    (dolist (struct '((70 "TestClass::test_in_class" )
                      (15 "TestClass")))
      (let ((init-pos (pop struct))
            (expected-string (pop struct))
            (buf (get-buffer-create "test-buffer")))
        (with-current-buffer buf
          (erase-buffer)
          (insert buffer-contents)
          (goto-char init-pos)
          (should (equal (test-cockpit-python--find-current-test) expected-string)))))))


(ert-deftest test-python-find-test-method-in-test-class-numeric-start ()
  (let ((buffer-contents "
class Test2342Class(TestCase):

    def test_in_class(self):
        pass

    pass
"))
    (dolist (struct '((70 "Test2342Class::test_in_class" )
                      (15 "Test2342Class")))
      (let ((init-pos (pop struct))
            (expected-string (pop struct))
            (buf (get-buffer-create "test-buffer")))
        (with-current-buffer buf
          (erase-buffer)
          (insert buffer-contents)
          (goto-char init-pos)
          (should (equal (test-cockpit-python--find-current-test) expected-string)))))))


(ert-deftest test-python-find-test-method-in-no-test-class ()
  (let ((buffer-contents "
class NoTestClass(TestCase):

    def test_in_no_test_class(self):
        pass

    pass
"))
    (dolist (struct '((80 nil)
                      (25 nil)))
      (let ((init-pos (pop struct))
            (expected-string (pop struct))
            (buf (get-buffer-create "test-buffer")))
        (with-current-buffer buf
          (erase-buffer)
          (insert buffer-contents)
          (goto-char init-pos)
          (should (equal (test-cockpit-python--find-current-test) expected-string)))))))

(ert-deftest test-python-find-test-method-in-non-testcase-test-class ()
  (let ((buffer-contents "
class TestClass:

    def test_in_class(self):
        pass

    pass
"))
    (dolist (struct '((70 "TestClass::test_in_class" )
                      (15 "TestClass")))
      (let ((init-pos (pop struct))
            (expected-string (pop struct))
            (buf (get-buffer-create "test-buffer")))
        (with-current-buffer buf
          (erase-buffer)
          (insert buffer-contents)
          (goto-char init-pos)
          (should (equal (test-cockpit-python--find-current-test) expected-string)))))))

(ert-deftest test-python-find-test-method-test-class-with-spaces ()
  (let ((buffer-contents "
class          TestClass:

    def          test_in_class(self):
        pass

    pass
"))
    (dolist (struct '((70 "TestClass::test_in_class" )
                      (15 "TestClass")))
      (let ((init-pos (pop struct))
            (expected-string (pop struct))
            (buf (get-buffer-create "test-buffer")))
        (with-current-buffer buf
          (erase-buffer)
          (insert buffer-contents)
          (goto-char init-pos)
          (should (equal (test-cockpit-python--find-current-test) expected-string)))))))

(ert-deftest test-python-find-test-method-in-test-class-after-teardown ()
  (let ((buffer-contents "
class TestClass(unittest.TestCase):

    def setup(self):
        pass

    def test_in_class(self):
        pass

    def tearDown(self):
        pass
"))
    (dolist (struct '((60 "TestClass")
                      (100 "TestClass::test_in_class")
                      (140 "TestClass")))
      (let ((init-pos (pop struct))
            (expected-string (pop struct))
            (buf (get-buffer-create "test-buffer")))
        (with-current-buffer buf
          (erase-buffer)
          (insert buffer-contents)
          (goto-char init-pos)
          (should (equal (test-cockpit-python--find-current-test) expected-string)))))))

(ert-deftest test-python-find-test-method-point-in-async-def ()
  (let ((buffer-contents "
async def test_first_outer():
    pass
")
        (init-pos 3)
        (expected-string "test_first_outer")
        (buf (get-buffer-create "test-buffer")))
    (with-current-buffer buf
      (erase-buffer)
      (insert buffer-contents)
      (goto-char init-pos)
      (should (equal (test-cockpit-python--find-current-test) expected-string)))))

(ert-deftest test-python-concat-file-path-test-method ()
  (mocker-let ((buffer-file-name () ((:output "/foo/bar/project/path/to/test_file.py")))
               (projectile-project-root () ((:output "/foo/bar/project")))
               (test-cockpit-python--find-current-test () ((:output "test_foo"))))
    (should (equal (test-cockpit-python--test-function-path) "path/to/test_file.py::test_foo"))))

(ert-deftest test-python-concat-file-path-no-test-method ()
  (mocker-let ((buffer-file-name () ((:output "/foo/bar/project/path/to/test_file.py")))
               (projectile-project-root () ((:output "/foo/bar/project")))
               (test-cockpit-python--find-current-test () ((:output nil))))
    (should (equal (test-cockpit-python--test-function-path) "path/to/test_file.py"))))

(ert-deftest test-python-python-infix ()
  (mocker-let ((projectile-project-type () ((:output 'python-pip))))
    (let ((infix (test-cockpit--infix)))
      (should (equal (aref (aref infix 0) 0) "Switches"))
      (should (equal (aref (aref infix 0) 1) '("-k" test-cockpit-python--restrict-substring)))
      (should (equal (aref (aref infix 0) 2) '("-f" "only lastly failed tests" "--last-failed")))
      (should (equal (aref (aref infix 0) 3) '("-x" "exit after first fail" "--exitfirst")))
      (should (equal (aref (aref infix 0) 4) '("-b" "build extensions before testing" "build_ext")))
      (should (equal (aref (aref infix 0) 5) '("-u" "use `uv run' to run pytest" "use-uv"
                                               :if test-cockpit-python--uv-available)))
      (should (equal (aref (aref infix 0) 6) '("-p" test-cockpit-python--python-version
                                               :if test-cockpit-python--uv-available
                                               :inapt-if-not test-cockpit--use-uv-on-p)))
      (should (equal (aref (aref infix 0) 7) '("-W" test-cockpit-python--with-package-directive
                                               :if test-cockpit-python--uv-available
                                               :inapt-if-not test-cockpit--use-uv-on-p)))
      (should (equal (car (aref (aref infix 0) 8)) "-m"))
      (should (equal (aref (aref infix 0) 9) '("-M" "test type hints" "--mypy")))
      (should (equal (aref (aref infix 1) 0) "Output"))
      (should (equal (aref (aref infix 1) 4) '("-r" "report output of passed tests" "-rFP")))
      (should (equal (aref (aref infix 1) 5) '("-w" "don't output warnings" "--disable-warnings")))
      (should (equal (aref (aref infix 1) 6) '("-n" "don't capture output" "--capture=no")))
      (should (equal (aref (aref infix 1) 7) '("-L" "show locals in tracebacks" "--showlocals"))))))

(ert-deftest test-python-parse-markers ()
  (let ((output "
registered markers:
@pytest.mark.slow
@pytest.mark.integration
@pytest.mark.django_db

@pytest.mark.skipif(condition)
    skip the given test if condition is true.
@pytest.mark.parametrize(argnames, argvalues)
    Join a fixture with test parameters.

no markers registered"))
    (should (equal (test-cockpit-python--parse-markers output)
                   '("slow" "integration" "django_db")))
    (should (equal (test-cockpit-python--parse-markers "") nil))))

(ert-deftest test-python-available-markers ()
  (setq test-cockpit--project-engines nil)
  (mocker-let ((test-cockpit--last-switches () ((:output nil :min-occur 0)))
               (projectile-project-root (&optional dir) ((:input-matcher (lambda (_dir) t) :output "foo-project")))
               (shell-command-to-string (command) ((:input '("pytest --markers")
                                                    :output "@pytest.mark.slow\n"))))
    (should (equal (test-cockpit-python--available-markers) '("slow")))))

(ert-deftest test-python-available-markers-fail ()
  (setq test-cockpit--project-engines nil)
  (mocker-let ((test-cockpit--last-switches () ((:output nil :min-occur 0)))
               (projectile-project-root (&optional dir) ((:input-matcher (lambda (_dir) t) :output "foo-project")))
               (shell-command-to-string (command) ((:input '("pytest --markers") :output ""))))
    (should (eq (test-cockpit-python--available-markers) nil))))

(ert-deftest test-python-uv-prefixed-marker-command ()
  (mocker-let ((test-cockpit--last-switches () ((:output '("use-uv" "--python=3.14")))))
    (should (equal (test-cockpit-python--markers-command)
                   "uv run --all-extras --all-groups --python=3.14 pytest --markers"))))

(ert-deftest test-python-marker-candidates ()
  (should (equal (test-cockpit-python--marker-candidates '("slow" "fast"))
                 '("and" "or" "not" "(" ")" "slow" "fast" "not slow" "not fast")))
  (should (equal (test-cockpit-python--marker-candidates nil)
                 '("and" "or" "not" "(" ")"))))

(ert-deftest test-python-quote-marker-arg ()
  (should (equal (test-cockpit-python--quote-marker-arg "-mslow") "-mslow"))
  (should (equal (test-cockpit-python--quote-marker-arg "--last-failed") "--last-failed"))
  (should (equal (test-cockpit-python--quote-marker-arg "-mnot (slow or fast)")
                 "-mnot\\ \\(slow\\ or\\ fast\\)")))

(ert-deftest test-python-marker-expression-error-valid ()
  (dolist (expr '("" " " "slow" "not slow" "slow and fast" "slow and not fast"
                  "slow or fast" "(slow or fast) and not flaky" "not (slow or fast)"
                  "(slow)"))
    (should (eq (test-cockpit-python--marker-expression-error expr) nil))))

(ert-deftest test-python-marker-expression-error-invalid ()
  (dolist (struct '(("slow and" "incomplete marker expression")
                    ("and slow" "expected a marker term, got \"and\"")
                    ("slow or" "incomplete marker expression")
                    ("a b" "unexpected token \"b\"")
                    ("not" "incomplete marker expression")
                    ("()" "unmatched \"(\"")
                    ("(slow" "unmatched \"(\"")
                    ("slow)" "unexpected token \")\"")
                    ("(a b)" "missing \")\" at \"b\"")
                    ("(slow and" "incomplete marker expression")
                    ("(a and" "incomplete marker expression")
                    ("(not" "incomplete marker expression")
                    ("(a or" "incomplete marker expression")
                    ("( slow or (fast and" "incomplete marker expression")))
    (let ((expr (pop struct))
          (expected (pop struct)))
      (should (equal (test-cockpit-python--marker-expression-error expr) expected)))))

(ert-deftest test-python-marker-read-reprompts-on-error ()
  (mocker-let ((completing-read
                (prompt table pred require-match initial-input history)
                ((:input-matcher (lambda (_prompt _table _pred _req init _hist)
                                   (equal init "slow or"))
                 :output "(slow or fast")
                 (:input-matcher (lambda (_prompt _table _pred _req init _hist)
                                   (equal init "(slow or fast"))
                 :output "not (slow or fast)"))))
    (should (equal (test-cockpit-python--read-marker-expression
                    "Marker expression" "slow or" nil)
                   "not (slow or fast)"))))

(ert-deftest test-python-marker-read-accepts-empty-input ()
  (mocker-let ((completing-read
                (prompt table pred require-match initial-input history)
                ((:input-matcher (lambda (_prompt _table _pred _req init _hist)
                                   (equal init "(a b)"))
                 :output ""))))
    (should (equal (test-cockpit-python--read-marker-expression
                    "Marker expression" "(a b)" nil)
                   ""))))

(ert-deftest test-python-marker-read-new-input-works ()
  (mocker-let ((completing-read
                (prompt table pred require-match initial-input history)
                ((:input-matcher (lambda (_prompt _table _pred _req init _hist)
                                   (eq init nil))
                 :output "slow"))))
    (should (equal (test-cockpit-python--read-marker-expression "Marker expression" nil nil)
                   "slow"))))

(ert-deftest test-python-marker-completion-fn ()
  (let* ((candidates '("and" "or" "not" "(" ")" "slow" "fast" "integration" "not slow" "not fast"))
         (table (test-cockpit-python--marker-completion-table (lambda () candidates))))
    (should (equal (funcall table "slow or in" nil nil) "slow or integration"))
    (should (equal (funcall table "not (s" nil nil) "not (slow"))
    (should (equal (funcall table "not sl" nil nil) "not slow"))
    (should (equal (funcall table "slow " nil t) candidates))))

(ert-deftest test-python-marker-completion-fn-lazy ()
  (let* ((count 0)
         (table (test-cockpit-python--marker-completion-table
                 (test-cockpit-python--lazy
                  (lambda () (setq count (1+ count)) '("and" "slow"))))))
    (funcall table "slow " nil t)
    (funcall table "slow " nil t)
    (should (equal count 1))))

(ert-deftest test-python-marker-highlighting ()
  (with-temp-buffer
    (insert "slow and")
    (setq-local test-cockpit-python--marker-input-overlay nil)
    (test-cockpit-python--highlight-marker-input (point-min) (point-max) 0)
    (should test-cockpit-python--marker-input-overlay)
    (should (eq (get-char-property (point-min) 'face)
                'test-cockpit-python-invalid-marker))
    (insert " fast")
    (test-cockpit-python--highlight-marker-input (point-min) (point-max) 0)
    (should (eq test-cockpit-python--marker-input-overlay nil))))

(ert-deftest test-python-marker-highlight-no-double-outline ()
  (with-temp-buffer
    (insert "(slow")
    (setq-local test-cockpit-python--marker-input-overlay nil)
    (test-cockpit-python--highlight-marker-input (point-min) (point-max) 0)
    (let ((first-overlay test-cockpit-python--marker-input-overlay))
      (test-cockpit-python--highlight-marker-input (point-min) (point-max) 0)
      (should (eq test-cockpit-python--marker-input-overlay first-overlay)))))
