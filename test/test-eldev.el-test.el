;;; test-cockpit-eldev.el-test.el --- Tests for test-cockpit.el -*- lexical-binding: t; -*-

(require 'mocker)
(require 'test-cockpit-eldev)

(ert-deftest test-project-eldev-type-available ()
  (should (alist-get 'emacs-eldev test-cockpit--project-types))
  )

(ert-deftest eldev-current-module-string-no-file-buffer-is-nil ()
  (mocker-let ((buffer-file-name () ((:output nil))))
    (let ((engine (make-instance test-cockpit-eldev-engine)))
      (should (eq (test-cockpit--engine-current-module-string engine) nil)))))

(ert-deftest eldev-current-function-string-no-file-buffer-is-nil ()
  (mocker-let ((buffer-file-name () ((:output nil :min-occur 0))))
    (let ((engine (make-instance test-cockpit-eldev-engine)))
      (should (eq (test-cockpit--engine-current-function-string engine) nil)))))

(ert-deftest test-get-eldev-test-project ()
  (setq test-cockpit--project-engines nil)
  (mocker-let
   ((projectile-project-type () ((:output 'emacs-eldev)))
    (projectile-project-root (&optional dir) ((:input-matcher (lambda (_dir) t) :output "foo-project")))
    (buffer-file-name () ((:output "tests/test-foo.el-test.el")))
    (which-function () ((:output "func-to-test"))))
   (mocker-let ((compile (command) ((:input '("eldev test -r concise --omit-backtraces") :output 'success :occur 1))))
     (test-cockpit-test-project))
   (mocker-let ((compile (command) ((:input '("eldev test -r concise --omit-backtraces --stop") :output 'success :occur 1))))
     (test-cockpit-test-project '("--stop")))
   (mocker-let ((compile (command) ((:input '("eldev test -r concise --print-backtraces=0") :output 'success :occur 1))))
     (test-cockpit-test-project '("backtraces")))))

(ert-deftest test-get-eldev-test-module ()
    (setq test-cockpit--project-engines nil)
    (mocker-let
   ((projectile-project-type () ((:output 'emacs-eldev)))
    (projectile-project-root (&optional dir) ((:input-matcher (lambda (_dir) t) :output "/home/user/foo-project/")))
    (buffer-file-name () ((:output "/home/user/foo-project/tests/test-foo.el-test.el")))
    (which-function () ((:output "func-to-test"))))
   (mocker-let ((compile (command) ((:input '("eldev test -r concise --omit-backtraces tests/test-foo.el-test.el") :output 'success :occur 1))))
     (test-cockpit-test-module))
   (mocker-let ((compile (command) ((:input '("eldev test -r concise --omit-backtraces --stop tests/test-foo.el-test.el") :output 'success :occur 1))))
     (test-cockpit-test-module '("--stop")))
   (mocker-let ((compile (command) ((:input '("eldev test -r concise --print-backtraces=0 tests/test-foo.el-test.el") :output 'success :occur 1))))
     (test-cockpit-test-module '("backtraces")))))

(ert-deftest test-get-eldev-test-module-no-el-test-file ()
    (setq test-cockpit--project-engines nil)
    (mocker-let ((projectile-project-type () ((:output 'emacs-eldev)))
                 (projectile-project-root (&optional dir) ((:input-matcher (lambda (_dir) t) :output "foo-project")))
                 (buffer-file-name () ((:output "tests/foo.el"))))
      (should (eq (test-cockpit--current-module-string) nil))))

(ert-deftest test-get-eldev-test-function-no-el-test-file ()
    (setq test-cockpit--project-engines nil)
    (mocker-let ((projectile-project-type () ((:output 'emacs-eldev)))
                 (projectile-project-root (&optional dir) ((:input-matcher (lambda (_dir) t) :output "foo-project")))
                 (buffer-file-name () ((:output "tests/foo.el"))))
      (should (eq (test-cockpit--current-function-string) nil))))


(ert-deftest test-get-eldev-test-function ()
  (setq test-cockpit--project-engines nil)
  (mocker-let
   ((projectile-project-type () ((:output 'emacs-eldev)))
    (projectile-project-root (&optional dir) ((:input-matcher (lambda (_dir) t) :output "foo-project")))
    (buffer-file-name () ((:output "tests/test-foo.el-test.el")))
    (which-function () ((:output "func-to-test"))))
   (mocker-let ((compile (command) ((:input '("eldev test -r concise --omit-backtraces func-to-test") :output 'success :occur 1))))
     (test-cockpit-test-function))
   (mocker-let ((compile (command) ((:input '("eldev test -r concise --omit-backtraces --stop func-to-test") :output 'success :occur 1))))
     (test-cockpit-test-function '("--stop")))
   (mocker-let ((compile (command) ((:input '("eldev test -r concise --print-backtraces=0 func-to-test") :output 'success :occur 1))))
     (test-cockpit-test-function '("backtraces")))))

(ert-deftest test-eldev-infix ()
  (setq test-cockpit--project-engines nil)
  (mocker-let
   ((projectile-project-type () ((:output 'emacs-eldev))))
   (let ((infix (aref (test-cockpit--infix) 0)))
     (should
      (and (equal (aref infix 0) "Eldev specific switches")
           (equal (aref infix 1) '("-b" "print backtraces" "backtraces"))
           (equal (aref infix 2) '("-x" "exit after one error" "--stop")))))))

;;; test-eldev.el-test.el ends here
