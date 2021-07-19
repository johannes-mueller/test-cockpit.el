(require 'mocker)
(require 'test-cockpit-python)

(ert-deftest test-project-python-pip-type-available ()
  (should (alist-get 'python-pip test-cockpit--project-types)))

(ert-deftest test-project-python-pkg-type-available ()
  (should (alist-get 'python-pkg test-cockpit--project-types)))

(ert-deftest test-pytest-binary-path-no-venv ()
  (should (equal (test-cockpit--python--pytest-binary-path) "pytest")))

(ert-deftest test-current-module-string-no-file-buffer-is-nil ()
  (mocker-let ((buffer-file-name () ((:output nil))))
    (let ((engine (make-instance test-cockpit--python-engine)))
      (should (eq (test-cockpit--engine-current-module-string engine) nil)))))

(ert-deftest test-current-function-string-no-file-buffer-is-nil ()
  (mocker-let ((buffer-file-name () ((:output nil))))
    (let ((engine (make-instance test-cockpit--python-engine)))
      (should (eq (test-cockpit--engine-current-function-string engine) nil)))))

(ert-deftest test-get-python-test-project-command-no-switches ()
  (setq test-cockpit--project-engines nil)
  (mocker-let
   ((projectile-project-type () ((:output 'python-pip :min-occur 0)))
    (projectile-project-root (&optional _dir) ((:input-matcher (lambda (_) t) :output "foo-project")))
    (test-cockpit--python--pytest-binary-path () ((:output "/foo/bin/pytest")))
    (buffer-file-name () ((:output "/home/user/project/tests/path/to/test_foo.py")))
    (compile (command) ((:input '("/foo/bin/pytest --color=yes --cov-report=") :output 'success :occur 1))))
   (test-cockpit-test-project)))

(ert-deftest test-get-python-test-project-command-switches ()
  (setq test-cockpit--project-engines nil)
  (mocker-let
      ((projectile-project-type () ((:output 'python-pip)))
       (projectile-project-root (&optional _dir) ((:input-matcher (lambda (_) t) :output "foo-project")))
       (test-cockpit--python--pytest-binary-path () ((:output "/foo/bin/pytest")))
       (buffer-file-name () ((:output "/home/user/project/tests/path/to/test_foo.py"))))
    (dolist (struct '((("--last-failed" "--baba")
		       "/foo/bin/pytest --color=yes --last-failed --cov-report=")
		      (("--cov-report=term" "--bubu")
		       "/foo/bin/pytest --color=yes --cov-report=term")))
      (let ((arglist (pop struct))
	    (expected (pop struct)))
	(mocker-let
	    ((compile (command) ((:input `(,expected) :output 'success :occur 1))))
	  (test-cockpit-test-project arglist))))))

(ert-deftest test-get-python-test-module-command-no-switches ()
  (setq test-cockpit--project-engines nil)
  (mocker-let
   ((projectile-project-type () ((:output 'python-pip)))
    (projectile-project-root (&optional _dir) ((:input-matcher (lambda (_) t) :output "/home/user/project")))
    (test-cockpit--python--pytest-binary-path () ((:output "pytest")))
    (buffer-file-name () ((:output "/home/user/project/tests/path/to/test_foo.py")))
    (compile (command) ((:input '("pytest --color=yes --cov-report= tests/path/to/test_foo.py") :output 'success :occur 1))))
   (test-cockpit-test-module)))

(ert-deftest test-get-python-test-fuzzy-module-command ()
  (setq test-cockpit--project-engines nil)
  (mocker-let
   ((projectile-project-type () ((:output 'python-pip :min-occur 0)))
    (projectile-project-root (&optional _dir) ((:input-matcher (lambda (_) t) :output "foo-project")))
    (test-cockpit--python--pytest-binary-path () ((:output "pytest")))
    (buffer-file-name () ((:output "/home/user/project/path/to/foo.py")))
    (compile (command) ((:input '("pytest --color=yes --cov-report= -k foo") :output 'success :occur 1))))
	     (test-cockpit-test-module)))

(ert-deftest test-get-python-test-module-command-switches ()
  (setq test-cockpit--project-engines nil)
  (mocker-let
      ((projectile-project-type () ((:output 'python-pip)))
       (projectile-project-root (&optional _dir) ((:input-matcher (lambda (_) t) :output "foo-project")))
       (test-cockpit--python--pytest-binary-path () ((:output "/foo/bin/pytest")))
       (buffer-file-name () ((:output "/home/user/project/path/to/foo.py"))))
    (dolist (struct '((("--last-failed" "--baba foo")
		       "/foo/bin/pytest --color=yes --last-failed --cov-report= -k foo")
		      (("--cov-report=term" "--bubu foo")
		       "/foo/bin/pytest --color=yes --cov-report=term -k foo")))
      (let ((arglist (pop struct))
	    (expected (pop struct)))
	(mocker-let
	    ((compile (command) ((:input `(,expected) :output 'success :occur 1))))
	  (test-cockpit-test-module arglist))))))

(ert-deftest test-get-python-test-function-command-no-switches ()
  (setq test-cockpit--project-engines nil)
  (mocker-let
   ((projectile-project-type () ((:output 'python-pip)))
    (projectile-project-root (&optional _dir) ((:input-matcher (lambda (_) t) :output "foo-project")))
    (test-cockpit--python--pytest-binary-path () ((:output "pytest")))
    (test-cockpit--python--test-function-path () ((:output "test_foo")))
    (buffer-file-name () ((:output "/home/user/project/path/to/foo.py")))
    (compile (command) ((:input '("pytest --color=yes --cov-report= test_foo") :output 'success :occur 1))))
   (test-cockpit-test-function)))

(ert-deftest test-get-python-test-function-command-switches ()
  (setq test-cockpit--project-engines nil)
  (mocker-let
      ((projectile-project-type () ((:output 'python-pip :occur 1)))
       (projectile-project-root (&optional _dir) ((:input-matcher (lambda (_) t) :output "foo-project")))
       (test-cockpit--python--pytest-binary-path () ((:output "/foo/bin/pytest")))
       (test-cockpit--python--test-function-path () ((:output "test_foo")))
       (buffer-file-name () ((:output "/home/user/project/path/to/foo.py"))))
    (dolist (struct '((("--last-failed" "--baba test_foo")
		       "/foo/bin/pytest --color=yes --last-failed --cov-report= test_foo")
		      (("--cov-report=term" "--bubu test_foo")
		       "/foo/bin/pytest --color=yes --cov-report=term test_foo")))
      (let ((arglist (pop struct))
	    (expected (pop struct)))
	(mocker-let
	    ((compile (command) ((:input `(,expected) :output 'success :occur 1))))
	  (test-cockpit-test-function arglist))))))

(ert-deftest test-python-build-ext-switch-default-command ()
  (setq test-cockpit--project-engines nil)
  (mocker-let
      ((projectile-project-type () ((:output 'python-pip :occur 1)))
       (projectile-project-root (&optional _dir) ((:input-matcher (lambda (_) t) :output "foo-project")))
       (test-cockpit--python--pytest-binary-path () ((:output "/foo/bin/pytest")))
       (buffer-file-name () ((:output "/home/user/project/tests/path/to/test_foo.py")))
       (compile (command) ((:input '("python setup.py build_ext --inplace && /foo/bin/pytest --color=yes --last-failed --cov-report=")
				   :output 'success :occur 1))))
    (test-cockpit-test-project '("--last-failed" "build_ext"))))

(ert-deftest test-python-build-ext-switch-changed-command ()
  (setq test-cockpit--project-engines nil)
  (let ((test-cockpit-python-build-ext-command "foo build-ext command"))
    (mocker-let
       ((projectile-project-type () ((:output 'python-pip :occur 1)))
	(projectile-project-root (&optional _dir) ((:input-matcher (lambda (_) t) :output "foo-project")))
	(test-cockpit--python--pytest-binary-path () ((:output "/foo/bin/pytest" :occur 1)))
	(buffer-file-name () ((:output "/home/user/project/tests/path/to/test_foo.py")))
	(compile (command) ((:input '("foo build-ext command && /foo/bin/pytest --color=yes --last-failed --cov-report=")
				    :output 'success :occur 1))))
     (test-cockpit-test-project '("--last-failed" "build_ext")))))

(ert-deftest test-insert-no-coverage-to-switches ()
  (dolist (struct '((("--last-failed") ("--last-failed" "--cov-report="))
		    (("--last-failed" "--cov-report=term") ("--last-failed" "--cov-report=term"))))
    (let ((arglist (pop struct))
	  (expected (pop struct)))
      (should (equal (test-cockpit--python--insert-no-coverage-to-switches arglist) expected)))))

(ert-deftest test-find-test-method-simple ()
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
	  (should (equal (test-cockpit--python--find-current-test) expected-string)))))))

(ert-deftest test-find-test-method-point-in-def ()
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
      (should (equal (test-cockpit--python--find-current-test) expected-string)))))

(ert-deftest test-find-test-method-point-in-def-numeric-start ()
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
      (should (equal (test-cockpit--python--find-current-test) expected-string)))))


(ert-deftest test-find-test-method-constant-pos ()
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
      (test-cockpit--python--find-current-test)
      (should (eq (point) init-pos)))))


(ert-deftest test-find-test-method-nested ()
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
	  (should (equal (test-cockpit--python--find-current-test) expected-string)))))))


(ert-deftest test-find-test-method-in-test-class ()
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
	  (should (equal (test-cockpit--python--find-current-test) expected-string)))))))


(ert-deftest test-find-test-method-in-test-class-numeric-start ()
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
	  (should (equal (test-cockpit--python--find-current-test) expected-string)))))))


(ert-deftest test-find-test-method-in-no-test-class ()
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
	  (should (equal (test-cockpit--python--find-current-test) expected-string)))))))

(ert-deftest test-find-test-method-in-non-testcase-test-class ()
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
	  (should (equal (test-cockpit--python--find-current-test) expected-string)))))))

(ert-deftest test-find-test-method-test-class-with-spaces ()
  (let ((buffer-contents "
class 	 TestClass:

    def 	 test_in_class(self):
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
	  (should (equal (test-cockpit--python--find-current-test) expected-string)))))))

(ert-deftest test-find-test-method-in-test-class-after-teardown ()
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
	  (should (equal (test-cockpit--python--find-current-test) expected-string)))))))

(ert-deftest test-find-test-method-point-in-async-def ()
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
      (should (equal (test-cockpit--python--find-current-test) expected-string)))))

(ert-deftest test-concat-file-path-test-method ()
  (mocker-let ((buffer-file-name () ((:output "/foo/bar/project/path/to/test_file.py")))
	       (projectile-project-root () ((:output "/foo/bar/project")))
	       (test-cockpit--python--find-current-test () ((:output "test_foo"))))
    (should (equal (test-cockpit--python--test-function-path) "path/to/test_file.py::test_foo"))))

(ert-deftest test-concat-file-path-no-test-method ()
  (mocker-let ((buffer-file-name () ((:output "/foo/bar/project/path/to/test_file.py")))
	       (projectile-project-root () ((:output "/foo/bar/project")))
	       (test-cockpit--python--find-current-test () ((:output nil))))
    (should (equal (test-cockpit--python--test-function-path) "path/to/test_file.py"))))

(ert-deftest test-python-infix ()
  (mocker-let ((projectile-project-type () ((:output 'python-pip))))
    (let ((infix (test-cockpit-infix)))
      (should (equal (aref (aref infix 0) 0) "Switches"))
      (should (equal (car (aref (aref infix 0) 1)) "-k"))
      (should (equal (aref (aref infix 0) 2) '("-l" "only lastly failed tests" "--last-failed")))
      (should (equal (aref (aref infix 0) 3) '("-b" "build extensions before testing" "build_ext")))
      (should (equal (aref (aref infix 1) 0) "Output"))
      (should (equal (aref (aref infix 1) 1) '("-v" "show single tests" "--verbose")))
      (should (equal (aref (aref infix 1) 2) '("-c" "print coverage report" "--cov-report=term")))
      (should (equal (aref (aref infix 1) 3) '("-r" "report output of passed tests" "-rFP")))
      (should (equal (aref (aref infix 1) 4) '("-w" "don't output warnings" "--disable-warnings"))))))
