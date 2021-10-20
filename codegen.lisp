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
  (defun pascal-to-kebab (name)
    "Convert a PascalCaseName to a kebab-case-name"
    (do* ((start 0 end)
          (end #1=(position-if #'upper-case-p name :start (1+ start)) #1#)
          (temp #2=(string-downcase (subseq name start end)) (concatenate 'string temp "-" #2#)))
         ((null end) temp))))

(ct (defun gen-setter (buffer-ptr-var width alignment &key signedp name-hint)
      #.(format nil "Generates code for setting a (potentially ~
      unaligned) WIDTH-byte int in the buffer. Returns values like ~
      MAKE-STRUCT-OUTPUTTER.")
      (do* ((value-var (if name-hint (make-symbol name-hint) (gensym "VALUE")))
            (type `(type (,(if signedp 'signed-byte 'unsigned-byte) ,(* 8 width)) ,value-var))
            (initializers (make-collector))
            (offset 0))
           ((>= offset width) (values value-var type () width initializers alignment))
        (let* ((worst-case (logior (car alignment) (cdr alignment) 8 #-64-bit 4))
               (stride (- (logior (ash -1 (1- (integer-length (- width offset)))) worst-case (- worst-case))))
               (type (find-symbol (format nil "UINT~A" (* 8 stride)) :keyword)))
          (assert type)
          (collect initializers
            `(setf (mem-ref ,buffer-ptr-var ,type)
                   (ldb (byte ,(* stride 8) ,(* 8 #+big-endian (- width offset stride) #+little-endian offset))
                        ,value-var))
            `(incf-pointer ,buffer-ptr-var ,stride))
          (incf offset stride)
          (incf (car alignment) stride)))))

(ct
  (defun make-struct-outputter (fields buffer-ptr-var &key request-hack (alignment (cons 0 1)))
    "Returns values: lambda-list, types, wrapper-forms-collector, length-form, initializers-collector, alignment"
    (let ((lambda-list (make-collector))
          (types (make-collector))
          (wrapper-forms (make-collector))
          (length-forms (make-collector))
          (initializers (make-collector)))
      (when request-hack
        ;; In requests, the first byte is at offset 1
        (collect initializers `(incf-pointer ,buffer-ptr-var 1))
        (collect length-forms 1)
        (incf (car alignment)))
      (dolist (field fields)
        (match-ecase field
          ((:pad bytes &rest)
           (collect initializers `(incf-pointer ,buffer-ptr-var ,bytes))
           (collect length-forms bytes)
           (incf (car alignment) bytes))
          ((:field &key name type &rest)
           (let ((name-hint (substitute #\- #\_ (string-upcase name)))
                 override-type)
             (multiple-value-bind (inner-ll inner-types inner-wrapper-forms inner-length-form
                                   inner-initializers inner-alignment)
                 (match-ecase (find-type type)
                   (:xid (setf override-type '(unsigned-byte 29))
                         (gen-setter buffer-ptr-var 4 alignment :name-hint name-hint))
                   (((m:and kind (m:or :unsigned :signed)) bits)
                    (multiple-value-bind (bytes leftover) (floor bits 8)
                      (assert (zerop leftover))
                      (gen-setter buffer-ptr-var bytes alignment :signedp (eql kind :signed) :name-hint name-hint)))
                   (type-form (error "Unable to parse type \"~A\" as ~A" type type-form)))
               (match-ecase inner-ll
                 (())
                 ((m:and var (m:type symbol))
                  (collect lambda-list var)
                  (collect types (if override-type `(type ,override-type ,var) inner-types)))
                 ((&rest inner-ll)
                  (let ((var (make-symbol name-hint)))
                    (collect lambda-list var)
                    (collect types `(type list ,var))
                    (collect wrapper-forms
                      `(destructuring-bind (,@inner-ll) ,var
                         (declare ,@inner-types))))))
               (collect-all wrapper-forms inner-wrapper-forms)
               (collect length-forms inner-length-form)
               (collect-all initializers inner-initializers)
               (setf alignment inner-alignment)))))
        (when request-hack
          ;; This field fits in the 1-byte gap in the request
          ;; header. After that, continue at offset 4.
          (assert (equal (apply #'+ (collect length-forms)) 2))
          (collect length-forms 2)
          (collect initializers `(incf-pointer ,buffer-ptr-var 2))
          (incf (car alignment) 2)
          (setf request-hack nil)))

      (values (collect lambda-list) (collect types) wrapper-forms
              `(+ ,@(collect length-forms)) initializers alignment))))

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
                   (buffer (gensym "BUFFER"))
                   (buffer-fill (gensym "BUFFER-FILL")))
               (multiple-value-bind (lambda-list types wrapper-forms length-form initializers)
                   (make-struct-outputter fields buffer-fill :request-hack t :alignment (cons 0 0))
                 `(defun ,name (,connection ,@lambda-list)
                    (declare (type foreign-pointer ,connection) ,@types)
                    (nest ,@(collect wrapper-forms)
                      (let ((,length ,length-form))
                        (with-pointer-to-bytes (,buffer ,length)
                          (let ((,buffer-fill ,buffer))
                            ,@(collect initializers)
                            (assert (pointer-eq (inc-pointer ,buffer ,length) ,buffer-fill)))
                          (xcb-send ,connection ,opcode nil ,buffer ,length (null-pointer) 0))))))))))))))
