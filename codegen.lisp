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

;;; Alignments are cons cells: the car is the offset, the cdr is the
;;; multiple. An alignment of (a . n) represents any integer of the
;;; form (a + k*n) for all k. In particular, (a . 0) represents an
;;; alignment of exactly a; (x . 1) represents unknown alignment.
(ct (defun alignment-union (&rest alignments)
      "An alignment which can represent any of the ALIGNMENTS"
      ;; We choose our multiple such that it divides each of the
      ;; multiples in ALIGNMENTS, and so that all the offsets of the
      ;; ALIGNMENTs are pairwise equivalent modulo the multiple. After
      ;; that, which ALIGNMENT we choose to provide the eventual
      ;; offset is arbitrary.
      (assert (not (endp alignments)))
      (cons (car (first alignments))
            (multiple-value-call #'gcd
              (values-list (mapcar #'cdr alignments))
              (values-list (mapcar (lambda (a) (- (cdr a) (car (first alignments)))) (rest alignments)))))))

(ct (defvar *temporaries*))
(ct (defvar *temporary-decls*))
(ct (defvar *alignment*))

(ct (defclass serializer-info ()
      ((parameters :reader parameters :initarg :parameters :initform (make-collector))
       (parameter-decls :accessor parameter-decls :initarg :parameter-decls :initform (make-collector))
       (initializers :reader initializers :initform (make-collector))
       (lengths :reader lengths :initarg :lengths :initform (make-collector))
       (serializers :reader serializers :initform (make-collector)))))

(ct (defun gen-setter (buffer-ptr-var width &key signedp name-hint)
      #.(format nil "Generates code for setting a (potentially ~
      unaligned) WIDTH-byte int in the buffer. Returns a SERIALIZER-INFO.")
      (with (info (make-instance 'serializer-info))
        (let* ((param (gensym (or name-hint "PARAM")))
               (temp (gensym (format nil "~A-TEMP" param)))
               (type `(,(if signedp 'signed-byte 'unsigned-byte) ,(* 8 width))))
          (collect *temporaries* `(,temp 0))
          (collect *temporary-decls* `(type ,type ,temp))
          (collect (parameters info) param)
          (collect (parameter-decls info) `(type ,type ,param))
          (collect (initializers info) `(setq ,temp ,param))
          (collect (lengths info) width)
          (do ((offset 0))
              ((>= offset width))
            (let* ((worst-case (logior (car *alignment*) (cdr *alignment*) 8 #-64-bit 4))
                   (stride (- (logior (ash -1 (1- (integer-length (- width offset)))) worst-case (- worst-case))))
                   (type (find-symbol (format nil "UINT~A" (* 8 stride)) :keyword)))
              (assert type)
              (collect (serializers info)
                `(setf (mem-ref ,buffer-ptr-var ,type)
                       (ldb (byte ,(* stride 8) ,(* 8 #+big-endian (- width offset stride) #+little-endian offset))
                            ,temp))
                `(incf-pointer ,buffer-ptr-var ,stride))
              (incf offset stride)
              (incf (car *alignment*) stride)))))))

(ct (defun make-type-serializer (type buffer-ptr-var &key name-hint)
      #.(format nil "~
Returns a SERIALIZER-INFO which is able to serialize TYPE into the buffer
pointed to by the pointer in BUFFER-PTR-VAR.")
      (flet ((copy-from-parent-ify (inner-info &optional override-type)
               (match-let* ((info (make-instance 'serializer-info))
                            (var (gensym (or name-hint "VALUE")))
                            ((inner-var) (collect (parameters inner-info)))
                            ((('type inner-type (m:eql inner-var))) (collect (parameter-decls inner-info))))
                 (collect (parameters info) var)
                 (collect (parameter-decls info)
                   `(type (or (eql :copy-from-parent) ,(or override-type inner-type)) ,var))
                 (collect (initializers info)
                   `(let ((,inner-var (if (eql ,var :copy-from-parent) 0 ,var)))
                      (declare ,@(collect (parameter-decls inner-info)))
                      ,@(collect (initializers inner-info))))
                 (collect-all (lengths info) (lengths inner-info))
                 (collect-all (serializers info) (serializers inner-info))
                 info)))
        (match-ecase (find-type type)
          (:xid (copy-from-parent-ify (gen-setter buffer-ptr-var 4 :name-hint name-hint) '(unsigned-byte 29)))
          (((m:and kind (m:or :unsigned :signed)) bits)
           (multiple-value-bind (bytes leftover) (floor bits 8)
             (assert (zerop leftover))
             (copy-from-parent-ify
              (gen-setter buffer-ptr-var bytes :signedp (eql kind :signed) :name-hint name-hint))))))))

(ct
  (defun make-struct-outputter (fields buffer-ptr-var &key request-hack)
    "Returns a SERIALIZER-INFO"
    (let ((info (make-instance 'serializer-info))
          (value-list-fields ()))
      ;; value_mask + value_list at the end gets converted to keyword arguments
      (block parse-value-list
        (match-case (last fields 2)
          (((:field &key ((:name "value_mask")) ((:type "CARD32")) ((:enum (mask :mask))) &rest)
            (:switch-struct &key ((:name "value_list")) ((:expr (:field-ref "value_mask"))) cases &rest))
           (let ((mask-alist (find-enum mask)))
             (setf value-list-fields
                   (mapcar (lambda (case)
                             (or (match-case case
                                   ((:bitcase &key fields ((:matches (match-num)))
                                              ((:enum ((m:equal mask) :mask))) &rest)
                                    (when-let ((keyword-name (car (rassoc match-num mask-alist))))
                                      (list (intern (nstring-upcase (pascal-to-kebab keyword-name)) :keyword)
                                            match-num fields))))
                                 (return-from parse-value-list)))
                           cases)
                   fields (butlast fields 2))))))

      (when request-hack
        ;; In requests, the first byte is at offset 1
        (collect (serializers info) `(incf-pointer ,buffer-ptr-var 1))
        (collect (lengths info) 1)
        (incf (car *alignment*)))

      (dolist (field fields)
        (match-ecase field
          ((:pad bytes &rest)
           (collect (serializers info) `(incf-pointer ,buffer-ptr-var ,bytes))
           (collect (lengths info) bytes)
           (incf (car *alignment*) bytes))
          ((:field &key name type ((:enum (enum maskp))) &rest)
           (let* ((inner-info (make-type-serializer type buffer-ptr-var))
                  (var (make-symbol (substitute #\- #\_ (string-upcase name)))))
             (when-let ((parameters (collect (parameters inner-info))))
               (collect (parameters info) var)
               (match-ecase parameters
                 ((lone-var)
                  (multiple-value-bind (param-type value)
                      (match-let* (((('type (m:and type ('or ('eql :copy-from-parent) raw-type)) (m:eql lone-var)))
                                    (collect (parameter-decls inner-info)))
                                   (enum-alist
                                    (when enum
                                      (mapcar (lambda (entry)
                                                (cons (intern (string-upcase (pascal-to-kebab (car entry))) :keyword)
                                                      (cdr entry)))
                                              (find-enum enum)))))
                        (flet ((parse-one-enum-value (var)
                                 (assert enum)
                                 `(etypecase ,var
                                    (integer ,var)
                                    ,@(mapcar (lambda (entry) `((eql ,(car entry)) ,(cdr entry))) enum-alist))))
                          (cond (maskp
                                 (assert (eql (car raw-type) 'unsigned-byte))
                                 (values 'list `(reduce #'logior ,var :key (lambda (x) ,(parse-one-enum-value 'x)))))
                                (enum
                                 (assert (member (car raw-type) '(unsigned-byte signed-byte)))
                                 (values `(or ,raw-type (member ,@(mapcar #'car enum-alist)))
                                         (parse-one-enum-value var)))
                                (t (values type var)))))
                    (collect (parameter-decls info) `(type ,param-type ,var))
                    (collect (initializers info)
                      `(let ((,lone-var ,value))
                         (declare ,@(collect (parameter-decls inner-info)))
                         ,@(collect (initializers inner-info))))))
                 ((&rest lambda-list)
                  (assert (null enum))
                  (collect (parameter-decls info) `(type list ,var))
                  (collect (initializers info)
                    `(destructuring-bind ,lambda-list ,var
                       (declare ,@(collect (parameter-decls inner-info)))
                       ,@(collect (initializers inner-info))))))
               (collect-all (lengths info) (lengths inner-info))
               (collect-all (serializers info) (serializers inner-info))))))
        (when request-hack
          ;; This field fits in the 1-byte gap in the request
          ;; header. After that, continue at offset 4.
          (assert (equal (apply #'+ (collect (lengths info))) 2))
          (collect (lengths info) 2)
          (collect (serializers info) `(incf-pointer ,buffer-ptr-var 2))
          (incf (car *alignment*) 2)
          (setf request-hack nil)))

      (when value-list-fields
        (collect (parameters info) '&key)
        (match-let* ((value-mask-ptr-var (gensym "VALUE-MASK-PTR"))
                     (switch-serializers (make-collector))
                     (mask-info (gen-setter value-mask-ptr-var 4 :name-hint "VALUE-MASK"))
                     ((value-mask-var) (collect (parameters mask-info))))
          (assert (every #'constantp (collect (lengths mask-info))))
          (collect-all (lengths info) (lengths mask-info))
          (dolist (switch-field value-list-fields)
            (destructuring-bind (keyword mask-num fields) switch-field
              (let* ((old-alignment *alignment*)
                     (inner-info (make-struct-outputter fields buffer-ptr-var))
                     (present-p-var (gensym (format nil "~A-PRESENT-P" keyword)))
                     (present-p-temp (gensym (format nil "~A-TEMP" present-p-var))))
                (match-ecase (collect (parameters inner-info))
                  ((lone-var)
                   (collect (parameters info) `((,keyword ,lone-var) nil ,present-p-var))
                   (apply #'collect (parameter-decls info)
                          (mapcar (lambda (inner-type)
                                    (match-ecase inner-type
                                      (('type type var) `(type (or null ,type) ,var))))
                                  (collect (parameter-decls inner-info))))
                   (collect (initializers info) `(when ,present-p-var ,@(collect (initializers inner-info))))))
                (collect *temporaries* present-p-temp)
                (collect *temporary-decls* `(type boolean ,present-p-temp))
                (collect (parameter-decls info) `(type boolean ,present-p-var))
                (collect (initializers info) `(setq ,present-p-temp ,present-p-var))
                (collect (lengths info) `(if ,present-p-var (+ ,@(collect (lengths inner-info))) 0))
                (collect switch-serializers `(when ,present-p-var
                                               (setf ,value-mask-var (logior ,value-mask-var ,mask-num))
                                               ,@(collect (serializers inner-info))))
                (setf *alignment* (alignment-union old-alignment *alignment*)))))
          (collect (serializers info)
            `(let ((,value-mask-var 0)
                   (,value-mask-ptr-var ,buffer-ptr-var))
               (declare ,@(collect (parameter-decls mask-info)))
               (incf-pointer ,buffer-ptr-var 4)
               ,@(collect switch-serializers)
               ,@(collect (initializers mask-info))
               ,@(collect (serializers mask-info))))))

      info)))

(ct
  (defparameter *supported-messages*
    '(("xproto" "CreateWindow" "MapWindow"))))

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
               (let* ((*temporaries* (make-collector))
                      (*temporary-decls* (make-collector))
                      (*alignment* (cons 0 0))
                      (info (make-struct-outputter fields buffer-fill :request-hack t)))
                 `(defun ,name (,connection ,@(collect (parameters info)))
                    (declare (type foreign-pointer ,connection) ,@(collect (parameter-decls info)))
                    (let ,(collect *temporaries*)
                      (declare ,@(collect *temporary-decls*))
                      ,@(collect (initializers info))
                      (let ((,length (+ ,@(collect (lengths info)))))
                        (with-pointer-to-bytes (,buffer ,length)
                          (let ((,buffer-fill ,buffer))
                            ,@(collect (serializers info))
                            (assert (pointer-eq (inc-pointer ,buffer ,length) ,buffer-fill)))
                          (xcb-send ,connection ,opcode nil ,buffer ,length (null-pointer) 0))))))))))))))
