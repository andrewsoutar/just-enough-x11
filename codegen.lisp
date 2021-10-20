(uiop:define-package #:com.andrewsoutar.just-enough-x11/codegen
  (:use #:cl #:alexandria #:cffi #:com.andrewsoutar.matcher)
  (:local-nicknames (#:m #:com.andrewsoutar.matcher/matchers))
  (:use #:com.andrewsoutar.just-enough-x11/utils)
  (:use #:com.andrewsoutar.just-enough-x11/xcb)
  (:use #:com.andrewsoutar.just-enough-x11/xcbproto-parser)
  (:import-from #:cxml)
  (:import-from #:cxml-dom)
  (:import-from #:trivial-features)
  (:export #:define-from-xml))
(cl:in-package #:com.andrewsoutar.just-enough-x11/codegen)

(ct
  (defun make-struct-outputter (fields buffer-ptr &key request-hack (offset 0) (alignment (cons 0 1)))
    "Returns values: lambda-list, length-form, declarations, initializers, alignment"
    (let ((lambda-list (make-collector))
          (declarations (make-collector))
          (initializers (make-collector)))
      (when request-hack
        ;; In requests, the first byte is at offset 1
        (incf offset)
        (incf (car alignment)))
      (dolist (field fields
                     (values (collect lambda-list) offset (collect declarations) (collect initializers) alignment))
        (let (width)
          (flet ((make-setter (val)
                   (let* ((worst-case (logior (car alignment) (cdr alignment) width #-32-bit 8 #-64-bit 4))
                          (stride (1+ (logandc1 worst-case (1- worst-case))))
                          (type (find-symbol (format nil "UINT~A" (* 8 stride)) :keyword)))
                     (assert type)
                     (if (= stride width)
                         `(setf (mem-ref ,buffer-ptr ,type ,offset) (ldb (byte ,stride 0) ,val))
                         (let ((val-var (gensym "VAL")))
                           `(let ((,val-var ,val))
                              ,@(loop for i from 0 below width by stride
                                      collect `(setf (mem-ref ,buffer-ptr ,type ,(+ offset i))
                                                     (ldb (byte ,stride ,i) ,val-var)))))))))
            (match-ecase field
              ((:pad bytes &rest)
               (setf width bytes))
              ((:field &key name type &rest)
               (let ((var (make-symbol (substitute #\- #\_ (string-upcase name)))))
                 (collect lambda-list var)
                 (match-ecase (find-type type)
                   (nil (error "~A: undefined type ~A" name type))
                   (:xid (setf width 4)
                         (collect declarations `(type (unsigned-byte 29) ,var))
                         (collect initializers (make-setter var)))
                   (((m:and kind (m:or :unsigned :signed)) bits)
                    (multiple-value-bind (bytes leftover) (floor bits 8)
                      (assert (zerop leftover))
                      (setf width bytes))
                    (collect declarations `(type (,(if (eql kind :signed) 'signed-byte 'unsigned-byte) ,bits) ,var))
                    (collect initializers (make-setter var)))
                   (t (error "Unable to parse type: ~A" type)))))
              (t (error "Unable to parse field: ~A" field))))
          (when width
            (when request-hack
              ;; This field fits in the 1-byte gap in the request
              ;; header. After that, continue at offset 4.
              (assert (= width 1))
              (setf width 3 request-hack nil))
            (incf offset width)
            (incf (car alignment) width)))))))

(ct
  (defparameter *supported-messages*
    '(("xproto" "MapWindow"))))

(defmacro with-pointer-to-bytes ((var length) &body body)
  (cond
    #+sbcl
    ((not (constantp length))
     ;; SBCL can't stack-allocate non-constant-size aliens, but it can
     ;; access arrays without copying. I'd rather allocate on the
     ;; lisp-heap than the c-heap because gencgc should be good at
     ;; collecting these short-lived allocations
     (with-gensyms (buffer)
       `(let ((,buffer (make-array (list ,length) :element-type '(unsigned-byte 8))))
          (sb-sys:with-pinned-objects (,buffer)
            (let ((,var (sb-sys:vector-sap ,buffer)))
              ,@body)))))
    (t
     `(with-foreign-object (,var :uint8 ,length)
        ,@body))))

(defmacro define-from-xml (proto-file &body names)
  (let* ((*proto-header* (import-proto proto-file))
         (forms (make-collector)))
    (dolist (name names `(progn ,@(collect forms)))
      (let* ((xmsg-name (delete #\- (string-capitalize name))))
        (unless (some (lambda (supported)
                        (destructuring-bind (header &rest messages) supported
                          (and (equal header (name *proto-header*))
                               (member xmsg-name messages :test #'equal))))
                      *supported-messages*)
          (warn "Message ~A.~A is not known to be supported, proceed at your own risk!"
                (name *proto-header*) xmsg-name))
        (match-ecase (find-message xmsg-name)
          (nil (cerror "Skip" "Couldn't find message ~A" name))
          ((:request &key opcode fields &rest)
           (collect forms
             (let ((connection (copy-symbol 'connection))
                   (length (gensym "LENGTH"))
                   (buffer (gensym "BUFFER")))
               (multiple-value-bind (lambda-list length-form decls initializers)
                   (make-struct-outputter fields buffer :request-hack t :alignment (cons 0 0))
                 `(defun ,name (,connection ,@lambda-list)
                    (declare (type foreign-pointer ,connection)
                             ,@decls)
                    (let ((,length ,length-form))
                      (with-pointer-to-bytes (,buffer ,length)
                        ,@initializers
                        (xcb-send ,connection ,opcode nil ,buffer ,length (null-pointer) 0)))))))))))))
