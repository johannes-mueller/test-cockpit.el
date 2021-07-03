(require 'mocker)
(require 'test-cockpit-cask)

(ert-deftest test-project-cask-type-available ()
  (should (alist-get 'emacs-cask test-cockpit--project-types))
  )

(ert-deftest test-get-cask-test-project ()
  (setq test-cockpit--project-engines nil)
  (mocker-let
   ((projectile-project-type () ((:output 'emacs-cask)))
    (projectile-project-root (&optional _dir) ((:input-matcher (lambda (_) t) :output "foo-project")))
    (buffer-file-name () ((:output "tests/test-foo.el-test.el")))
    (which-function () ((:output "func-to-test"))))
   (mocker-let ((compile (command) ((:input '("cask exec ert-runner") :output 'success :occur 1))))
     (test-cockpit-test-project))
   (mocker-let ((compile (command) ((:input '("cask install && cask exec ert-runner") :output 'success :occur 1))))
     (test-cockpit-test-project '("install")))))

(ert-deftest test-get-cask-test-module ()
    (setq test-cockpit--project-engines nil)
    (mocker-let
   ((projectile-project-type () ((:output 'emacs-cask)))
    (projectile-project-root (&optional _dir) ((:input-matcher (lambda (_) t) :output "foo-project")))
    (buffer-file-name () ((:output "tests/test-foo.el-test.el")))
    (which-function () ((:output "func-to-test"))))
   (mocker-let ((compile (command) ((:input '("cask exec ert-runner tests/test-foo.el-test.el") :output 'success :occur 1))))
     (test-cockpit-test-module))
   (mocker-let ((compile (command) ((:input '("cask install && cask exec ert-runner tests/test-foo.el-test.el") :output 'success :occur 1))))
     (test-cockpit-test-module '("install")))))

(ert-deftest test-get-cask-test-function ()
  (setq test-cockpit--project-engines nil)
  (mocker-let
   ((projectile-project-type () ((:output 'emacs-cask)))
    (projectile-project-root (&optional _dir) ((:input-matcher (lambda (_) t) :output "foo-project")))
    (buffer-file-name () ((:output "tests/test-foo.el-test.el")))
    (which-function () ((:output "func-to-test"))))
   (mocker-let ((compile (command) ((:input '("cask exec ert-runner -p func-to-test") :output 'success :occur 1))))
     (test-cockpit-test-function))
   (mocker-let ((compile (command) ((:input '("cask install && cask exec ert-runner -p func-to-test") :output 'success :occur 1))))
     (test-cockpit-test-function '("install")))))

(ert-deftest test-cask-infix ()
  (setq test-cockpit--project-engines nil)
  (mocker-let
   ((projectile-project-type () ((:output 'emacs-cask))))
   (let ((infix (test-cockpit-infix)))
     (should
      (and (equal (aref infix 0) "Cask specific switches")
	   (equal (aref infix 1) '("-i" "Run `cask install` before test" "install")))))))

(ert-deftest test-cask-insert-install-command-unset ()
  (should (equal (test-cockpit--cask--insert-install-command "foo" nil) "foo")))

(ert-deftest test-cask-insert-install-command-set ()
  (should (equal (test-cockpit--cask--insert-install-command "foo" "install") "cask install && foo")))

;;; test-cask.el-test.el ends here
