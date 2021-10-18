(uiop:define-package #:com.andrewsoutar.just-enough-x11/utils
  (:use #:cl)
  (:export #:make-collector #:collect))
(cl:in-package #:com.andrewsoutar.just-enough-x11/utils)

;;; FIXME I might want to spin some of these utils out into a library,
;;; I end up copy-pasting them into all my projects
(defun make-collector ()
  (let ((ret (cons nil nil)))
    (setf (car ret) ret)
    ret))

(defun collect (collector &rest things)
  (when things
    (setf (car collector) (last (setf (cdar collector) things))))
  (cdr collector))
