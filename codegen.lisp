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

(ct (defvar *alignment*))

(ct (defclass serializer-info ()
      ((parameters :reader parameters :initarg :parameters :initform (make-collector))
       (parameter-decls :accessor parameter-decls :initarg :parameter-decls :initform (make-collector))
       (wrapper-forms :reader wrapper-forms :initform (make-collector))
       (lengths :reader lengths :initarg :lengths :initform (make-collector))
       (serializers :reader serializers :initform (make-collector)))))

(ct (defun gen-setter (buffer-ptr-var width &key signedp name-hint)
      #.(format nil "Generates code for setting a (potentially ~
      unaligned) WIDTH-byte int in the buffer. Returns a SERIALIZER-INFO.")
      (do* ((value-var (if name-hint (make-symbol name-hint) (gensym "VALUE")))
            (type-decl `(type (,(if signedp 'signed-byte 'unsigned-byte) ,(* 8 width)) ,value-var))
            (info (make-instance 'serializer-info :parameters (make-collector value-var)
                                                  :parameter-decls (make-collector type-decl)
                                                  :lengths (make-collector width)))
            (offset 0))
           ((>= offset width) info)
        (let* ((worst-case (logior (car *alignment*) (cdr *alignment*) 8 #-64-bit 4))
               (stride (- (logior (ash -1 (1- (integer-length (- width offset)))) worst-case (- worst-case))))
               (type (find-symbol (format nil "UINT~A" (* 8 stride)) :keyword)))
          (assert type)
          (collect (serializers info)
            `(setf (mem-ref ,buffer-ptr-var ,type)
                   (ldb (byte ,(* stride 8) ,(* 8 #+big-endian (- width offset stride) #+little-endian offset))
                        ,value-var))
            `(incf-pointer ,buffer-ptr-var ,stride))
          (incf offset stride)
          (incf (car *alignment*) stride)))))

(ct (defun make-type-serializer (type buffer-ptr-var &key name-hint)
      #.(format nil "~
Returns a SERIALIZER-INFO which is able to serialize TYPE into the buffer
pointed to by the pointer in BUFFER-PTR-VAR.")
      (match-ecase (find-type type)
        (:xid (with (info (gen-setter buffer-ptr-var 4 :name-hint name-hint))
                (match-let* (((var) (collect (parameters info))))
                  (setf (parameter-decls info) (make-collector `(type (unsigned-byte 29) ,var))))))
        (((m:and kind (m:or :unsigned :signed)) bits)
         (multiple-value-bind (bytes leftover) (floor bits 8)
           (assert (zerop leftover))
           (gen-setter buffer-ptr-var bytes :signedp (eql kind :signed) :name-hint name-hint))))))

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
          ((:field &key name type &rest)
           (let ((name-hint (substitute #\- #\_ (string-upcase name))))
             (let ((inner-info (make-type-serializer type buffer-ptr-var :name-hint name-hint)))
               (match-ecase (collect (parameters inner-info))
                 (())
                 ((var)
                  (collect (parameters info) var))
                 ((&rest inner-ll)
                  (let ((var (make-symbol name-hint)))
                    (collect (parameters info) var)
                    (collect (parameter-decls info) `(type list ,var))
                    (collect (wrapper-forms info)
                      `(destructuring-bind (,@inner-ll) ,var
                         (declare ,@(collect (parameter-decls inner-info))))))))
               (collect-all (parameter-decls info) (parameter-decls inner-info))
               (collect-all (wrapper-forms info) (wrapper-forms inner-info))
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
        (let ((value-mask-ptr-var (gensym "VALUE-MASK-PTR"))
              (switch-serializers (make-collector)))
          (multiple-value-bind (value-mask-var mask-type-decl mask-serializers)
              (let ((inner-info (gen-setter value-mask-ptr-var 4 :name-hint "VALUE-MASK")))
                (match-let* (((var) (collect (parameters inner-info))))
                  (assert (endp (collect (wrapper-forms inner-info))))
                  (collect-all (lengths info) (lengths inner-info))
                  (values var (collect (parameter-decls inner-info)) (serializers inner-info))))
            (dolist (switch-field value-list-fields)
              (destructuring-bind (keyword mask-num fields) switch-field
                (let ((old-alignment *alignment*))
                  (let ((inner-info (make-struct-outputter fields buffer-ptr-var)))
                    (assert (endp (collect (wrapper-forms inner-info))))
                    (let ((present-p-var (gensym (format nil "~A-PRESENT-P" keyword))))
                      (match-ecase (collect (parameters inner-info))
                        (())
                        ((lone-var)
                         (collect (parameters info) `((,keyword ,lone-var) nil ,present-p-var))
                         (apply #'collect (parameter-decls info)
                                (mapcar (lambda (inner-type)
                                          (match-ecase inner-type
                                            (('type type var) `(type (or null ,type) ,var))))
                                        (collect (parameter-decls inner-info))))))
                      (collect (lengths info) `(if ,present-p-var (+ ,@(collect (lengths inner-info))) 0))
                      (collect switch-serializers `(when ,present-p-var
                                                      (setf ,value-mask-var (logior ,value-mask-var ,mask-num))
                                                      ,@(collect (serializers inner-info))))
                      (setf *alignment* (alignment-union old-alignment *alignment*)))))))
            (collect (serializers info)
              `(let ((,value-mask-var 0)
                     (,value-mask-ptr-var ,buffer-ptr-var))
                 (declare ,@mask-type-decl)
                 (incf-pointer ,buffer-ptr-var 4)
                 ,@(collect switch-serializers)
                 ,@(collect mask-serializers))))))

      info)))

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
               (let* ((*alignment* (cons 0 0))
                      (info (make-struct-outputter fields buffer-fill :request-hack t)))
                 `(defun ,name (,connection ,@(collect (parameters info)))
                    (declare (type foreign-pointer ,connection) ,@(collect (parameter-decls info)))
                    (nest ,@(collect (wrapper-forms info))
                      (let ((,length (+ ,@(collect (lengths info)))))
                        (with-pointer-to-bytes (,buffer ,length)
                          (let ((,buffer-fill ,buffer))
                            ,@(collect (serializers info))
                            (assert (pointer-eq (inc-pointer ,buffer ,length) ,buffer-fill)))
                          (xcb-send ,connection ,opcode nil ,buffer ,length (null-pointer) 0))))))))))))))
