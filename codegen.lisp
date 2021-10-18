(uiop:define-package #:com.andrewsoutar.just-enough-x11/codegen
  (:use #:cl #:alexandria #:cffi)
  (:use #:com.andrewsoutar.just-enough-x11/utils)
  (:import-from #:cxml)
  (:import-from #:cxml-dom)
  (:import-from #:trivial-features)
  (:export #:define-from-xml))
(cl:in-package #:com.andrewsoutar.just-enough-x11/codegen)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-foreign-library libxcb (t (:default "libxcb"))))
(use-foreign-library libxcb)

;;; FIXME I have an awful lot of CT stuff, I should probably pull them
;;; out into a separate file
(defmacro ct (&body body)
  "Shorthand for (EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE) ...)"
  `(eval-when (:compile-toplevel :load-toplevel :execute) ,@body))

;;; FIXME this has a lot of similarities to the parser in
;;; cl-wayland-client, maybe want to dedup
(ct
  (defun text-contents (node)
    (when node
      (apply 'concatenate 'string
             (map 'list (lambda (node)
                          (cond ((dom:comment-p node) "")
                                ((dom:text-node-p node) (dom:data node))
                                (t (cerror "Skip" "Non-text found: ~A" node) "")))
                  (dom:child-nodes node))))))

(ct
  (defun child-elems (node)
    (coerce (remove-if-not #'dom:element-p (dom:child-nodes node)) 'list)))

(ct
  (defun get-attribute (element name)
    (when-let ((attr (dom:get-attribute-node element name)))
      (dom:value attr))))

(ct (defvar *xcb-import-cache* (make-hash-table :test 'equal)))
(ct (defparameter *default-proto-file* #p"/usr/share/xcb/xproto.xml"))
(ct (defvar *xcb-header*))
(ct
  (defclass xcb-header ()
    ((name :type string :reader name :initarg :name)
     (c-name :type string :reader c-name :initarg :c-name)
     (extension-name :type (or null string) :reader extension-name :initarg :extension-name)
     (imports :type list :accessor imports :initform ())
     (types :type hash-table :reader types :initform (make-hash-table :test 'equal))
     (enums :type hash-table :reader enums :initform (make-hash-table :test 'equal))
     (messages :type hash-table :reader messages :initform (make-hash-table :test 'equal)))))
(ct
  (defmethod initialize-instance :after ((self xcb-header) &key &allow-other-keys)
    (let ((c-name (c-name self))
          (extension-name (extension-name self)))
      (unless (eql (null c-name) (null extension-name))
        (error "~A has ~1{~A ~A but no ~A~}" self
               (if c-name (list 'c-name c-name 'extension-name) (list 'extension-name extension-name 'c-name)))))))
(ct
  (defun xcb-import (proto-file)
    (setf proto-file (pathname proto-file))
    (multiple-value-bind (header foundp) (gethash proto-file *xcb-import-cache*)
      (when foundp
        (return-from xcb-import header)))
    (let* ((root (dom:document-element (cxml:parse-file proto-file (cxml-dom:make-dom-builder))))
           ;; I know this looks strange but the names in the XML don't line up with our usage
           (header (make-instance 'xcb-header :name (or (get-attribute root "header")
                                                        (error "No header attribute on root"))
                                              :c-name (get-attribute root "extension-name")
                                              :extension-name (get-attribute root "extension-xname"))))
      (assert (equal (dom:tag-name root) "xcb"))
      (setf (gethash proto-file *xcb-import-cache*) header)
      (unless (equal proto-file *default-proto-file*)
        (push (xcb-import *default-proto-file*) (imports header)))
      (dolist (child (child-elems root) header)
        (let* ((tag (dom:tag-name child))
               (name (cond ((member tag '("request" "event" "eventcopy" "error" "errorcopy" "struct"
                                          "union" "eventstruct" "xidtype" "xidunion" "enum") :test #'equal)
                            (get-attribute child "name"))
                           ((equal tag "typedef")
                            (get-attribute child "newname"))
                           ((equal tag "import")
                            (push (xcb-import (make-pathname
                                               :name (string-trim #(#\Space #\Newline #\Tab) (text-contents child))
                                               :type "xml"
                                               :defaults proto-file))
                                  (imports header))
                            (go next)))))
          (assert name)
          (let ((table (cond ((member tag '("request" "event" "eventcopy") :test #'equal) (messages header))
                             ((equal tag "enum") (enums header))
                             (t (types header)))))
            (assert (null (shiftf (gethash name table) child)))))
        next))))

(ct
  (defun make-struct-outputter (elem buffer-ptr &key request-hack ignored-extras (offset 0) (alignment (cons 0 1)))
    "Returns values: lambda-list, length-form, declarations, initializers, alignment"
    (assert (null (imports *xcb-header*))) ; No code to deal with these yet
    (let ((lambda-list (make-collector))
          (declarations (make-collector))
          (initializers (make-collector)))
      (when request-hack
        ;; In requests, the first byte is at offset 1
        (incf offset)
        (incf (car alignment)))
      (dolist (field (child-elems elem)
                     (values (collect lambda-list) offset (collect declarations) (collect initializers) alignment))
        (let ((tag (dom:tag-name field))
              width)
          (labels ((make-setter (val)
                     (let* ((worst-case (logior (car alignment) (cdr alignment) width #-32-bit 8 #-64-bit 4))
                            (stride (1+ (logandc1 worst-case (1- worst-case))))
                            (type (find-symbol (format nil "UINT~A" (* 8 stride)) :keyword)))
                       (assert type)
                       (if (= stride width)
                           `(setf (mem-ref ,buffer-ptr ,type ,offset) ,val)
                           (let ((val-var (gensym "VAL")))
                             `(let ((,val-var ,val))
                                ,@(loop for i from 0 below width by stride
                                        collect `(setf (mem-ref ,buffer-ptr ,type ,(+ offset i))
                                                       (ldb (byte ,stride ,i) ,val-var)))))))))
            (cond ((equal tag "pad")
                   (setf width (parse-integer (get-attribute field "bytes"))))
                  ((equal tag "field")
                   ;; FIXME I really should have a get-attribute-or-lose function for stuff like this
                   (let* ((name (or (get-attribute field "name") (error "field ~A has no name" field)))
                          (var (make-symbol (substitute #\- #\_ (string-upcase name))))
                          (type (or (get-attribute field "type") (error "field ~A has no type" field)))
                          (type-elem (or (gethash type (types *xcb-header*))
                                         (error "~A: undefined type ~A" field type)))
                          (type-elem-tag (dom:tag-name type-elem)))
                     (collect lambda-list var)
                     (cond ((equal type-elem-tag "xidtype")
                            (setf width 4)
                            (collect declarations `(type (unsigned-byte 29) ,var))
                            (collect initializers (make-setter var)))
                           (t (error "Type ~A not handled" type-elem)))))
                  ((member tag ignored-extras :test #'equal))
                  (t (error "Field ~A not handled" field))))
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
  (let* ((*xcb-header* (xcb-import (merge-pathnames proto-file *default-proto-file*)))
         (forms (make-collector)))
    (dolist (name names `(progn ,@(collect forms)))
      (let* ((xmsg-name (delete #\- (string-capitalize name)))
             (message-elem (gethash xmsg-name (messages *xcb-header*))))
        (unless (some (lambda (sup)
                        (and (equal (car sup) (name *xcb-header*))
                             (member xmsg-name (cdr sup) :test #'equal)))
                      *supported-messages*)
          (warn "Message ~A.~A is not known to be supported, proceed at your own risk!"
                (name *xcb-header*) xmsg-name))
        (cond ((null message-elem) (cerror "Skip" "Couldn't find message ~A" name))
              ((equal (dom:tag-name message-elem) "request")
               (assert (null (c-name *xcb-header*)))
               (collect forms
                 (with-gensyms (length buffer)
                   (let ((connection (copy-symbol 'connection))
                         (opcode (parse-integer
                                  (or (get-attribute message-elem "opcode") (error "~A: no opcode" message-elem)))))
                     (multiple-value-bind (lambda-list length-form decls initializers)
                         (make-struct-outputter message-elem buffer :request-hack t
                                                                    :ignored-extras '("doc") :alignment (cons 0 0))
                       `(defun ,name (,connection ,@lambda-list)
                          (declare (type foreign-pointer ,connection)
                                   ,@decls)
                          (let ((,length ,length-form))
                            (with-pointer-to-bytes (,buffer ,length)
                              ,@initializers
                              (xcb-send ,connection ,opcode nil ,buffer ,length (null-pointer) 0)))))))))
              (t (error "Don't know how to compile ~A yet" message-elem)))))))

;;; FIXME only checked this on Linux
(defcstruct iovec
  (base :pointer)
  (len :size))

(defcstruct xcb-protocol-request
  (count :size)
  (ext :pointer)
  (opcode :uint8)
  (isvoid :uint8))

(defcfun (xcb-send-request-with-fds64 :library libxcb) :uint64
  (connection :pointer)
  (flags :int)
  (vector (:pointer (:struct iovec)))
  (req (:pointer (:struct xcb-protocol-request)))
  (n-fds :uint)
  (fds (:pointer :int)))

(defun xcb-send (connection opcode has-reply buffer buffer-len fds fds-len &optional extension)
  (declare (type foreign-pointer connection)
           (type (unsigned-byte 8) opcode)
           (type foreign-pointer buffer)
           (type fixnum buffer-len)
           (type (or null foreign-pointer) fds)
           (type fixnum fds-len)
           (type (or null foreign-pointer) extension))
  ;; There's an undocumented requirement that vector[-1] and
  ;; vector[-2] are allocated; we're putting everything in one array,
  ;; so allocate three entries
  (with-foreign-objects ((iovec '(:struct iovec) 3)
                         (req '(:struct xcb-protocol-request)))
    (setf (mem-aref iovec '(:struct iovec) 2) (list 'base buffer 'len buffer-len))
    (setf (mem-ref req '(:struct xcb-protocol-request))
          (list 'count 1 'ext (or extension (null-pointer)) 'opcode opcode 'isvoid (if has-reply 0 1)))
    (xcb-send-request-with-fds64 connection 0 (mem-aptr iovec '(:struct iovec) 2) req
                                 fds-len (or fds (null-pointer)))))
