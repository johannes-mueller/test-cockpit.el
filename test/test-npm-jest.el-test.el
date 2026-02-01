;;; test-npm-jest.el-test.el --- Tests for test-cockpit.el -*- lexical-binding: t; -*-

(require 'mocker)
(require 'test-cockpit-npm-jest)

(require 'cc-mode)
(defvar js-testing-syntax-table
  (let ((table (make-syntax-table)))
    (c-populate-syntax-table table)
    (modify-syntax-entry ?` "\"" table)
    table))

(defmacro with-temp-js-buffer (&rest body)
  (declare (indent 0) (debug t))
  (let ((temp-buffer (make-symbol "temp-buffer")))
    `(let ((,temp-buffer (generate-new-buffer " *temp*")))
       ;; `kill-buffer' can change current-buffer in some odd cases.
                (with-current-buffer ,temp-buffer
             (unwind-protect
                 (with-syntax-table js-testing-syntax-table ,@body)
               (and (buffer-name ,temp-buffer)
                    (kill-buffer ,temp-buffer)))))))


(ert-deftest test-cockpit-npm-jest-type-available ()
  (should (alist-get 'npm test-cockpit--project-types)))


(ert-deftest test-get-npm-test-test-project-command-no-switches ()
  (setq test-cockpit--project-engines nil)
  (mocker-let
   ((projectile-project-type () ((:output 'npm :min-occur 0)))
    (projectile-project-root (&optional dir) ((:input-matcher (lambda (_dir) t) :output "foo-project")))
    (compile (command) ((:input '("npm test -- --color") :output 'success))))
   (test-cockpit-test-project)))

(ert-deftest test-get-npm-test-test-project-command-only-changed-switch ()
  (setq test-cockpit--project-engines nil)
  (mocker-let
   ((projectile-project-type () ((:output 'npm :min-occur 0)))
    (projectile-project-root (&optional dir) ((:input-matcher (lambda (_dir) t) :output "foo-project")))
    (compile (command) ((:input '("npm test -- --color --onlyChanged") :output 'success))))
   (test-cockpit-test-project '("--onlyChanged"))))

(ert-deftest test-get-npm-test-test-project-command-two-switches ()
  (setq test-cockpit--project-engines nil)
  (mocker-let
   ((projectile-project-type () ((:output 'npm :min-occur 0)))
    (projectile-project-root (&optional dir) ((:input-matcher (lambda (_dir) t) :output "foo-project")))
    (compile (command) ((:input '("npm test -- --color --onlyChanged --coverage") :output 'success))))
   (test-cockpit-test-project '("--onlyChanged" "--coverage"))))


(ert-deftest test-get-npm-test-test-module-command-no-switches ()
  (setq test-cockpit--project-engines nil)
  (mocker-let
   ((projectile-project-type () ((:output 'npm :min-occur 0)))
    (projectile-project-root (&optional dir) ((:input-matcher (lambda (_dir) t) :output "foo-project")))
    (buffer-file-name () ((:output "/path/to/file.test.js")))
    (compile (command) ((:input '("npm test -- --color --testPathPattern '/path/to/file\\.test\\.js'") :output 'success))))
   (test-cockpit-test-module)))

(ert-deftest test-get-npm-test-test-module-command-only-failures-switch ()
  (setq test-cockpit--project-engines nil)
  (mocker-let
   ((projectile-project-type () ((:output 'npm :min-occur 0)))
    (projectile-project-root (&optional dir) ((:input-matcher (lambda (_dir) t) :output "foo-project")))
    (buffer-file-name () ((:output "/path/to/otherfile.test.js")))
    (compile (command) ((:input '("npm test -- --color --testPathPattern '/path/to/otherfile\\.test\\.js' --onlyFailures") :output 'success))))
   (test-cockpit-test-module '("--onlyFailures"))))

(ert-deftest test-get-npm-test-test-module-command-two-switches ()
  (setq test-cockpit--project-engines nil)
  (mocker-let
   ((projectile-project-type () ((:output 'npm :min-occur 0)))
    (projectile-project-root (&optional dir) ((:input-matcher (lambda (_dir) t) :output "foo-project")))
    (buffer-file-name () ((:output "/path/to/otherfile.test.js")))
    (compile (command)
             ((:input
               '("npm test -- --color --testPathPattern '/path/to/otherfile\\.test\\.js' --onlyFailures --coverage")
               :output 'success))))
   (test-cockpit-test-module '("--onlyFailures" "--coverage"))))


(ert-deftest test-npm-jest-test-function-command-no-switches ()
  (setq test-cockpit--project-engines nil)
  (mocker-let
   ((projectile-project-type () ((:output 'npm :min-occur 0)))
    (test-cockpit-npm-jest--find-current-test () ((:output "desc it")))
    (buffer-file-name () ((:output "/path/to/file.test.js")))
    (compile (command) ((:input '("npm test -- --color --testPathPattern '/path/to/file\\.test\\.js' --testNamePattern 'desc it'") :output 'success))))
   (test-cockpit-test-function)))

(ert-deftest test-npm-jest-test-function-command-coverage-switch ()
  (setq test-cockpit--project-engines nil)
  (mocker-let
   ((projectile-project-type () ((:output 'npm :min-occur 0)))
    (test-cockpit-npm-jest--find-current-test () ((:output "desc it")))
    (buffer-file-name () ((:output "/path/to/otherfile.test.js")))
    (compile (command) ((:input '("npm test -- --color --testPathPattern '/path/to/otherfile\\.test\\.js' --testNamePattern 'desc it' --coverage") :output 'success))))
   (test-cockpit-test-function '("--coverage"))))

(ert-deftest test-npm-jest-test-function-two-swiches ()
  (setq test-cockpit--project-engines nil)
  (mocker-let
   ((projectile-project-type () ((:output 'npm :min-occur 0)))
    (test-cockpit-npm-jest--find-current-test () ((:output "desc it")))
    (buffer-file-name () ((:output "/path/to/otherfile.test.js")))
    (compile (command)
             ((:input
               '("npm test -- --color --testPathPattern '/path/to/otherfile\\.test\\.js' --testNamePattern 'desc it' --coverage --onlyFailures")
               :output 'success))))
   (test-cockpit-test-function '("--coverage" "--onlyFailures"))))

(ert-deftest test-npm-current-module-string-no-file-buffer-is-nil ()
  (mocker-let ((buffer-file-name () ((:output nil))))
    (let ((engine (make-instance test-cockpit-npm-jest--engine)))
      (should (eq (test-cockpit--engine-current-module-string engine) nil)))))


(ert-deftest test-npm-current-module-string-test-file-buffer-is-filename ()
  (mocker-let ((buffer-file-name () ((:output "/some/path/file.test.js"))))
    (let ((engine (make-instance test-cockpit-npm-jest--engine)))
      (should (equal (test-cockpit--engine-current-module-string engine) "/some/path/file.test.js")))))


(ert-deftest test-npm-current-module-string-no-test-file-buffer-is-nil ()
  (mocker-let ((buffer-file-name () ((:output "/some/path/file.js"))))
    (let ((engine (make-instance test-cockpit-npm-jest--engine)))
      (should (equal (test-cockpit--engine-current-module-string engine) nil)))))


(ert-deftest test-npm-find-current-test-simple-test-single-quote ()
  (let ((buffer-contents "
test('this should work', () => {
    expect(something).toBe(expected);
})
"))
    (with-temp-js-buffer
      (insert buffer-contents)
      (goto-char 50)
      (should (equal (test-cockpit-npm-jest--find-current-test) "this should work")))))


(ert-deftest test-npm-find-current-test-simple-test-no-test ()
  (let ((buffer-contents "

"))
    (with-temp-js-buffer
      (insert buffer-contents)
      (goto-char 1)
      (should (eq (test-cockpit-npm-jest--find-current-test) nil)))))


(ert-deftest test-npm-find-current-test-simple-it-single-quote ()
  (let ((buffer-contents "
it('should work', () => {
    expect(something).toBe(expected);
})
"))
    (with-temp-js-buffer
      (insert buffer-contents)
      (goto-char 50)
      (should (equal (test-cockpit-npm-jest--find-current-test) "should work")))))


(ert-deftest test-npm-find-current-test-simple-it-backtick ()
  (let ((buffer-contents "
it(`should still work`, () => {
    expect(something).toBe(expected);
})
"))
    (with-temp-js-buffer
      (insert buffer-contents)
      (goto-char 50)
      (should (equal (test-cockpit-npm-jest--find-current-test) "should still work")))))


(ert-deftest test-npm-find-current-test-simple-test-double-quote ()
  (let ((buffer-contents "
test(\"should still work\", () => {
    expect(something).toBe(expected);
})
"))
    (with-temp-js-buffer
      (insert buffer-contents)
      (goto-char 50)
      (should (equal (test-cockpit-npm-jest--find-current-test) "should still work")))))


(ert-deftest test-npm-find-current-test-simple-describe-it-single-quote ()
  (let ((buffer-contents "
describe('this thing', () => {
  it(`should work`, () => {
      expect(something).toBe(expected);
  });
});
"))
    (with-temp-js-buffer
      (insert buffer-contents)
      (goto-char 70)
      (should (equal (test-cockpit-npm-jest--find-current-test) "this thing should work")))))


(ert-deftest test-npm-find-current-simple-describe-only-outside-it-backtick ()
  (let ((buffer-contents "
test('not this', () => {
    expect(something).toBe(expected);
});

describe(`that thing`, () => {

  it(`should work`, () => {
      expect(something).toBe(expected);
  });
});
"))
    (with-temp-js-buffer
      (insert buffer-contents)
      (goto-char 96)
      (should (equal (test-cockpit-npm-jest--find-current-test) "that thing")))))


(ert-deftest test-npm-find-current-simple-describe-no-it-double-quote ()
  (let ((buffer-contents "
describe(\"this very thing\", () => {

  it(`should work`, () => {
      expect(something).toBe(expected);
  });
});
"))
    (with-temp-js-buffer
      (insert buffer-contents)
      (goto-char 31)
      (should (equal (test-cockpit-npm-jest--find-current-test) "this very thing")))))


(ert-deftest test-npm-find-current-test-not-change-point ()
  (let ((buffer-contents "
test(\"should still work\", () => {
    expect(something).toBe(expected);
})
"))
    (with-temp-js-buffer
      (insert buffer-contents)
      (goto-char 50)
      (test-cockpit-npm-jest--find-current-test)
      (should (eq (point) 50)))))


(ert-deftest test-npm-find-current-test-simple-modifiers ()
  (let ((buffer-contents "
test.only.concurrent.skip('should still work', () => {
    expect(something).toBe(expected);
})
"))
    (with-temp-js-buffer
      (insert buffer-contents)
      (goto-char 70)
      (should (equal (test-cockpit-npm-jest--find-current-test) "should still work")))))


(ert-deftest test-npm-find-current-desc-simple-modifiers ()
  (let ((buffer-contents "
describe.only.concurrent.skip('this very thing', () => {

  it(`should still work`, () => {
      expect(something).toBe(expected);
  });
});
"))
    (with-temp-js-buffer
     (insert buffer-contents)
     (goto-char 56)
     (should (equal (test-cockpit-npm-jest--find-current-test) "this very thing")))))


(ert-deftest test-npm-find-current-test-simple-test-single-quote-extra-space ()
  (let ((buffer-contents "
test ( 'this should still work', () => {
    expect(something).toBe(expected);
})
"))
    (with-temp-js-buffer
      (insert buffer-contents)
      (goto-char 50)
      (should (equal (test-cockpit-npm-jest--find-current-test) "this should still work")))))

(ert-deftest test-npm-find-current-test-simple-test-single-quote-extra-line-break ()
  (let ((buffer-contents "
test
   (
    'this should still work', () => {
    expect(something).toBe(expected);
})
"))
    (with-temp-js-buffer
      (insert buffer-contents)
      (goto-char 58)
      (should (equal (test-cockpit-npm-jest--find-current-test) "this should still work")))))

(ert-deftest test-npm-find-current-test-simple-array ()
  (let ((buffer-contents "
test.only.each([
    [1, 1, fn('foo')],
    [1, 2, fn('foo')],
    [2, 1, fn('foo')],
])('should $a $b still work', () => {
    expect(something).toBe(expected);
})
"))
    (with-temp-js-buffer
      (insert buffer-contents)
      (goto-char 70)
      (should (equal (test-cockpit-npm-jest--find-current-test) "should $a $b still work")))))


(ert-deftest test-npm-find-current-desc-simple-array ()
  (let ((buffer-contents "
describe.each([
    [1, 1, fn('foo')],
    [1, 2, fn('foo')],
    [2, 1, fn('foo')],
])('this very thing', () => {

  it(`should still work`, () => {
      expect(something).toBe(expected);
  });
});
"))
    (with-temp-js-buffer
      (insert buffer-contents)
      (goto-char 56)
      (should (equal (test-cockpit-npm-jest--find-current-test) "this very thing")))))


(ert-deftest test-npm-find-anglar-case-describe ()
  (let ((buffer-contents "
describe('AppComponent', () => {
  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [
        AppComponent
      ],
    }).compileComponents();
  });

  it('should create the app', () => {
    const fixture = TestBed.createComponent(AppComponent);
    const app = fixture.componentInstance;
    expect(app).toBeTruthy();
  });
})"))
    (with-temp-js-buffer
      (insert buffer-contents)
      (goto-char 190)
      (should (equal (test-cockpit-npm-jest--find-current-test) "AppComponent")))))


(ert-deftest test-npm-find-anglar-case-distracting-test-marker ()
  (let ((buffer-contents "
describe('AppComponent', () => {
  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [
        AppComponent
      ],
    }).compileComponents();
  });

  it('should create the test app', () => {
    const fixture = TestBed.createComponent(AppComponent);
    const app = fixture.componentInstance;
    expect(app).toBeTruthy();
  });
})"))
    (with-temp-js-buffer
      (insert buffer-contents)
      (goto-char 288)
      (should (equal (test-cockpit-npm-jest--find-current-test) "AppComponent should create the test app")))))


(ert-deftest test-npm-empty-describe-block-before-it ()
  (let ((buffer-contents "
describe('outer', () => {
  describe('middle', () => {
  });

  it('foo', async () => {
    expect(2).toBe(2); // HERE
  });
});
"))
    (with-temp-js-buffer
      (insert buffer-contents)
      (goto-char 100)
      (should (equal (test-cockpit-npm-jest--find-current-test) "outer foo")))))

(ert-deftest test-npm-non-empty-describe-block-before-it ()
  (let ((buffer-contents "
describe('outer', () => {
  describe('middle', () => {
    it('bar', async () => {
      expect(2).toBe(2); // HERE
    });
  });

  it('foo', async () => {
    expect(2).toBe(2); // HERE
  });
});
"))
    (with-temp-js-buffer
      (insert buffer-contents)
      (goto-char 150)
      (should (equal (test-cockpit-npm-jest--find-current-test) "outer foo")))))
