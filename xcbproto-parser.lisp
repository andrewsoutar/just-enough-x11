(uiop:define-package #:com.andrewsoutar.just-enough-x11/xcbproto-parser
  (:documentation #.(format nil "Support for parsing xcb-proto XML ~
  files into more easily digestible lispy structures that are easier ~
  to use for codegen."))
  (:use #:cl #:alexandria)
  (:shadow #:parse-error)
  (:import-from #:cxml)
  (:import-from #:cxml-dom)
  (:export #:*default-proto-file* #:import-proto)
  (:export #:proto-header #:name #:c-name #:extension-name)
  (:export #:*proto-header* #:find-message #:find-type))
(cl:in-package #:com.andrewsoutar.just-enough-x11/xcbproto-parser)

;;; Helpers

;;; FIXME: All this is pretty identical to stuff in cl-wayland-client,
;;; maybe think about sharing some of it
(defun trim-whitespace (string)
  (string-trim #(#\Newline #\Tab #\Space) string))

(defun text-contents (node)
  #.(format nil "Get the text contents of a string-node NODE, or ~
  NIL if NODE is null")
  (when node
    (apply 'concatenate 'string
           (map 'list (lambda (node)
                        (cond ((dom:comment-p node) "")
                              ((dom:text-node-p node) (dom:data node))
                              (t (cerror "Skip" "Non-text found: ~A" node) "")))
                (dom:child-nodes node)))))

(defun child-elems (node)
  "Gets a list of all the children of NODE which are ELEMENTs; ignores all others"
  (coerce (remove-if-not #'dom:element-p (dom:child-nodes node)) 'list))

(defun get-attribute (element name)
  "Gets the value of the attribute NAME of ELEMENT, or NIL if it's not present"
  (when-let ((attr (dom:get-attribute-node element name)))
    (dom:value attr)))

(define-condition parse-error (error) ())
(define-condition unknown-tag-error (parse-error)
  ((element :reader element :initarg :element))
  (:report (lambda (err stream)
             (format stream "Unknown element ~A" (element err)))))
(define-condition missing-attr-error (parse-error)
  ((element :reader element :initarg :element)
   (attribute :reader attribute :initarg :attribute))
  (:report (lambda (err stream)
             (format stream "Element ~A missing required attribute \"~A\""
                     (element err) (attribute err)))))

(defun get-attribute-or-lose (element name)
  "Gets the value of the attribute NAME of ELEMENT, or loses"
  (or (get-attribute element name)
      (error 'missing-attr-error :element element :attribute name)))


;;; Parser data structures
(defclass proto-header ()
  ((name :type string :reader name :initarg :name :documentation "Header name")
   (c-name :type string :reader c-name :initarg :c-name :documentation "C symbol prefix")
   (extension-name :type (or null string) :reader extension-name :initarg :extension-name
                   :documentation "X11 extension name")
   (imports :type list :accessor imports :initform ())
   (types :type hash-table :reader types :initform (make-hash-table :test 'equal))
   (enums :type hash-table :reader enums :initform (make-hash-table :test 'equal))
   (messages :type hash-table :reader messages :initform (make-hash-table :test 'equal)))
  (:documentation #.(format nil "Parser-internal data structure ~
  representing one header. This structure contains raw XML elements, ~
  so isn't suitable to be read directly by parser end-users.")))

(defmethod initialize-instance :after ((self proto-header) &key &allow-other-keys)
  (let ((c-name (c-name self))
        (extension-name (extension-name self)))
    (unless (eql (null c-name) (null extension-name))
      (cerror "Keep going anyway" "~A has ~1{~A ~A but no ~A~}" self
              (if c-name
                  (list 'c-name c-name 'extension-name)
                  (list 'extension-name extension-name 'c-name))))))


(defvar *proto-header* nil
  #.(format nil "The PROTO-HEADER that's currently being processed. ~
  Used by many functions in this package, which may need to access ~
  header-specific context, eg. types."))

(defun parse-structlike (elem &optional extra-handler)
  "Parse a structure-like element into a list of its fields."
  (mapcan (lambda (child)
            (let ((tag (dom:tag-name child)))
              (cond ((string= tag "pad")
                     (list `(:pad ,(parse-integer (get-attribute-or-lose child "bytes")))))
                    ((string= tag "field")
                     (list `(:field :name ,(get-attribute-or-lose child "name")
                                    :type ,(get-attribute-or-lose child "type"))))
                    ((and extra-handler (funcall extra-handler child))
                     ())
                    (t (error "Unknown structure field: ~A" child)))))
          (child-elems elem)))

(defun find-message (message-name)
  (when-let ((elem (gethash message-name (messages *proto-header*))))
    (let ((tag (dom:tag-name elem)))
      (cond ((equal tag "request")
             `(:request
               :opcode ,(parse-integer (get-attribute-or-lose elem "opcode"))
               :fields ,(parse-structlike elem (lambda (child)
                                                 (let ((tag (dom:tag-name child)))
                                                   (cond ((equal tag "doc")
                                                          ;; FIXME
                                                          t)))))))
            ((member tag '("event" "eventcopy") :test #'equal)
             (error "Don't know how to parse ~A yet" elem))
            (t (error 'unknown-tag-error :element elem))))))

(defun find-type (type-name)
  ;; These are the ones hard-coded in xcbproto:xcbgen/xtypes.py
  (when-let ((prim-type (assoc type-name
                               '#.`(,@(mapcan (lambda (x) `((,(format nil "CARD~A" x) . (:unsigned ,x))
                                                            (,(format nil "INT~A" x) . (:signed ,x))))
                                              '(8 16 32))
                                    ("BYTE" . (:unsigned 8))
                                    ,@(mapcar (lambda (x) `(,x ,(intern (string-upcase x) :keyword)))
                                              '("BOOL" "char" "float" "double" "void")))
                               :test #'equal)))
    (return-from find-type (cdr prim-type)))
  (destructuring-bind (*proto-header* elem &aux (tag (dom:tag-name elem)))
      (or (some (lambda (header)
                  (multiple-value-bind (elem foundp) (gethash type-name (types header))
                    (when foundp (list header elem))))
                (list* *proto-header* (imports *proto-header*)))
          (error "No type named \"~A\"" type-name))
    (cond ((equal tag "xidtype")
           :xid)
          ((equal tag "typedef")
           (find-type (get-attribute-or-lose elem "oldname")))
          (t (error 'unknown-tag-error :element elem)))))


(defvar *import-cache* (make-hash-table :test 'equal)
  #.(format nil "Cache: file pathname => proto-header object. Useful ~
  for saving work and handling dependency loops. If things go ~
  sideways it's safe to reset this."))

(defvar *default-proto-file* #p"/usr/share/xcb/xproto.xml"
  #.(format nil "The default file, which is included in all protos ~
  by default. This is typically located in the same directory as the ~
  other protocol XML files, so it's also useful for finding other ~
  protocol files."))

(defun import-proto (proto-file)
  #.(format nil "Parse the xcb-proto file PROTO-FILE into a ~
  PROTO-HEADER. Searches for PROTO-FILE as given, and also ~
  MERGE-PATHNAME'd with *DEFAULT-PROTO-FILE*.")
  (let* ((dom
           (with-open-stream (stream (or (open proto-file :direction :input
                                                          :if-does-not-exist nil
                                                          :element-type '(unsigned-byte 8))
                                         (open (merge-pathnames proto-file *default-proto-file*)
                                               :direction :input
                                               :element-type '(unsigned-byte 8))))
             (multiple-value-bind (header foundp) (gethash (truename stream) *import-cache*)
               (when foundp
                 (return-from import-proto header)))
             (cxml:parse-stream stream (cxml-dom:make-dom-builder))))
         (root (dom:document-element dom))

         ;; I know this looks strange but the names in the XML don't line up with our usage
         (header (make-instance 'xcb-header :name (get-attribute-or-lose root "header")
                                            :c-name (get-attribute root "extension-name")
                                            :extension-name (get-attribute root "extension-xname"))))

    (assert (equal (dom:tag-name root) "xcb"))
    (setf (gethash proto-file *import-cache*) header)
    (let ((default-header (import-proto *default-proto-file*)))
      (unless (eql default-header header)
        (push default-header (imports header))))
    (dolist (child (child-elems root) header)
      (let* ((tag (dom:tag-name child))
             (name (cond ((member tag '("request" "event" "eventcopy" "error" "errorcopy" "struct"
                                        "union" "eventstruct" "xidtype" "xidunion" "enum") :test #'equal)
                          (get-attribute-or-lose child "name"))
                         ((equal tag "typedef")
                          (get-attribute-or-lose child "newname"))
                         ((equal tag "import")
                          (let ((import-name (trim-whitespace (text-contents child)))
                                (*default-pathname-defaults* proto-file))
                            (push (import-proto (make-pathname :name import-name :type "xml")) (imports header)))
                          (go next)))))
        (let ((table (cond ((member tag '("request" "event" "eventcopy") :test #'equal) (messages header))
                           ((equal tag "enum") (enums header))
                           (t (types header)))))
          (assert (null (shiftf (gethash name table) child)))))
      next)))
