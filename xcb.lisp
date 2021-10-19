(uiop:define-package #:com.andrewsoutar.just-enough-x11/xcb
  (:documentation #.(format nil "Runtime support for performing X11 ~
  calls through XCB; codegen will generate calls to functions in ~
  here, and some functions (eg. xcb_connect) will also be exported ~
  to end-users."))
  (:use #:cl #:cffi)
  (:export #:xcb-send))
(cl:in-package #:com.andrewsoutar.just-enough-x11/xcb)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-foreign-library libxcb (t (:default "libxcb"))))
(use-foreign-library libxcb)

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
