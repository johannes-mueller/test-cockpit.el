;;; test-cockpit-transient.el --- Transient objects for test-cockpit -*- lexical-binding: t; -*-

;; Author: Johannes Mueller <github@johannes-mueller.org
;; URL: https://github.com/johannes-mueller/test-cockpit.el
;; Version: 0.1.0
;; License: GPLv3
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; Transient objects needed by test-cockpit for the user interface.

;;; Code:

(require 'transient)

(defclass test-cockpit-transient-selection (transient-variable)
  ((scope :initarg :scope))
  "A transient-variable to select from a list of mutually non exclusive items.")

(cl-defmethod transient-init-value ((obj test-cockpit-transient-selection))
  (let ((variable (oref obj variable)))
    (oset obj value (symbol-value variable))))

(cl-defmethod transient-infix-read ((obj test-cockpit-transient-selection))
  (let ((prompt (oref obj prompt))
        (choices (oref obj choices)))
    (completing-read prompt (funcall choices))))

(cl-defmethod transient-infix-set ((obj test-cockpit-transient-selection) item)
  (let* ((variable (oref obj variable))
         (selected (symbol-value variable)))
    (set variable
         (if (member item selected)
             (delete item selected)
           (append selected (list item))))
    (oset obj value (symbol-value variable))))

(cl-defmethod transient-format-value ((obj test-cockpit-transient-selection))
  (let ((enabled-items (oref obj value))
        (choices (oref obj choices)))
    (concat
     (propertize "[" 'face 'transient-inactive-value)
     (mapconcat (lambda (item)
                  (if (member item enabled-items)
                      (propertize item 'face 'transient-value)
                    (propertize item 'face 'transient-inactive-value)))
                (funcall choices)
                (propertize ", " 'face 'transient-inactive-value))
     (propertize "]" 'face 'transient-inactive-value))))


(provide 'test-cockpit-transient)
;;; test-cockpit-transient.el ends here
