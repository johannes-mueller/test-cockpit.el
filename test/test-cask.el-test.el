(require 'mocker)
(require 'test-cockpit-cask)

(ert-deftest test-project-cask-type-available ()
  (should (alist-get 'emacs-cask test-cockpit-project-types))
  )

(ert-deftest test-get-cask-test-project-command ()
  (mocker-let
   ((projectile-project-type () ((:output 'emacs-cask))))
   (should (equal (test-cockpit-test-project-command nil)
		  "cask exec ert-runner"))
   (should (equal (test-cockpit-test-project-command '("install"))
		  "cask install && cask exec ert-runner"))))

(ert-deftest test-get-cask-test-module-command ()
  (mocker-let
   ((projectile-project-type () ((:output 'emacs-cask)))
    (buffer-file-name () ((:output "tests/test-foo.el-test.el"))))
   (should (equal (test-cockpit-test-module-command nil)
		  "cask exec ert-runner tests/test-foo.el-test.el"))
   (should (equal (test-cockpit-test-module-command '("install"))
		  "cask install && cask exec ert-runner tests/test-foo.el-test.el"))))

(ert-deftest test-get-cask-test-function-command ()
  (mocker-let
   ((projectile-project-type () ((:output 'emacs-cask)))
    (which-function () ((:output "func-to-test"))))
   (should (equal (test-cockpit-test-function-command nil)
		  "cask exec ert-runner -p func-to-test"))
   (should (equal (test-cockpit-test-function-command '("install"))
		  "cask install && cask exec ert-runner -p func-to-test"))))

(ert-deftest test-cask-infix ()
  (let ((infix (test-cockpit-infix)))
    (should (and (equal (aref infix 0) "Cask specific switches")
		 (equal (aref infix 1) '("-i" "Run `cask install` before test" "install"))))))

(ert-deftest test-cask-insert-install-command-unset ()
  (should (equal (test-cockpit--cask--insert-install-command "foo" nil) "foo")))

(ert-deftest test-cask-insert-install-command-set ()
  (should (equal (test-cockpit--cask--insert-install-command "foo" "install") "cask install && foo")))

;;; test-cask.el-test.el ends here
