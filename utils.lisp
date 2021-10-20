(uiop:define-package #:com.andrewsoutar.just-enough-x11/utils
  (:use #:cl)
  (:export #:ct)
  (:export #:make-collector #:collect))
(cl:in-package #:com.andrewsoutar.just-enough-x11/utils)

(defmacro ct (&body body)
  "Shorthand for (EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE) ...)"
  `(eval-when (:compile-toplevel :load-toplevel :execute) ,@body))

;;; FIXME I might want to spin some of these utils out into a library,
;;; I end up copy-pasting them into all my projects
(defun make-collector (&rest things)
  (let ((ret (cons nil things)))
    (setf (car ret) (last ret))
    ret))

(defun collect-all (collector &rest collectors)
  (dolist (2collector collectors)
    (when (setf (cdar collector) (cdr collector))
      (setf (car collector) (car 2collector))))
  collector)

(defun collect (collector &rest things)
  (cdr (collect-all collector (apply #'make-collector things))))
