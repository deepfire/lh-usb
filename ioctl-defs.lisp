;; SBCL bindings for Linux usbdevfs
;; Copyright 2008 Alastair Bridgewater
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;;
;;; ioctl-defs.lisp
;;;
;;; IOCTL constant and structure definitions for Linux usbdevfs.
;;;

(cl:in-package :lh-usb-ioctls)

;;; Linux IOCTL definition helpers

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Damned IOCTL junk is defined in unsigned, but implemented in signed. :-/
  (defun u32->s32 (foo)
    (if (logbitp 31 foo)
	(logior #x-80000000 foo)
	foo)))

(defmacro encode-ioctl (mode type index &optional (struct nil struct-p))
  (when (and (eq mode :io) struct-p)
    (error "Attempting to encode :IO IOCTL with a STRUCT"))
  (when (and (not (eq mode :io)) (not struct-p))
    (error "Attempting to encode ~S IOCTL without a STRUCT" mode))

  (let* ((mode-bits (ecase mode
		      (:iowr 3)
		      (:ior 2)
		      (:iow 1)
		      (:io 0)))
	 (type-bits (char-code type))
	 (length-bits `(foreign-type-size ',struct))
	 (non-length-bits (dpb mode-bits (byte 2 30)
			       (dpb type-bits (byte 8 8)
				    index))))
    `(u32->s32 ,(if struct-p
		    `(dpb ,length-bits (byte 14 16)
			  ,non-length-bits)
		    non-length-bits))))

(defmacro define-ioctl (name mode type index &optional (struct nil struct-p))
  `(defconstant ,name (encode-ioctl ,mode ,type ,index ,@(when struct-p (list struct)))))


;;; USB IOCTL structures

(defcstruct usbdevfs-ctrltransfer
  (request-type :uint8)
  (request :uint8)
  (value :uint16)
  (index :uint16)
  (length :uint16)
  (timeout :uint32) ;; in milliseconds (frames?)
  (data :pointer))

(defcstruct usbdevfs-bulktransfer
  (endpoint :uint)
  (length :uint)
  (timeout :uint) ;; in milliseconds (frames?)
  (data :pointer))

(defcstruct usbdevfs-setinterface
  (interface :uint)
  (altsetting :uint))

(defcstruct usbdevfs-disconnectsignal
  (signr :uint)
  (context :pointer))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +usbdevfs-maxdrivername+ 255))

(defcstruct usbdevfs-getdriver
  (interface :uint)
  (driver :char :count #.(1+ +usbdevfs-maxdrivername+)))

(defcstruct usbdevfs-connectinfo
  (devnum :uint)
  (slow :uchar))

;; flags?
(defconstant +usbdevfs-urb-short-not-ok+ 1)
(defconstant +usbdevfs-urb-iso-asap+ 2)

(defconstant +usbdevfs-urb-type-iso+ 0)
(defconstant +usbdevfs-urb-type-interrupt+ 1)
(defconstant +usbdevfs-urb-type-control+ 2)
(defconstant +usbdevfs-urb-type-bulk+ 3)

(defcstruct usbdevfs-iso-packet-desc
  (length :uint)
  (actual-length :uint)
  (status :uint))

(defcstruct usbdevfs-urb
  (type :uchar)
  (endpoint :uchar)
  (status :int)
  (flags :uint)
  (buffer :pointer)
  (buffer-length :int)
  (actual-length :int)
  (start-frame :int)
  (number-of-packets :int)
  (error-count :int)
  (signr :uint)
  (usercontext :pointer)
  (iso-frame-desc usbdevfs-iso-packet-desc :count 0))

(defcstruct usbdevfs-ioctl
  (ifno :int)
  (ioctl-code :int)
  (data :pointer))

(defcstruct usbdevfs-hub-portinfo
  (nports :char)
  (port :char :count 127))


;;; USB IOCTLs

;; We're a userspace program, not a kernel, we don't need the 32-bit
;; specific ioctls, they're for compatability within the kernel
;; itself.
(define-ioctl USBDEVFS_CONTROL           :iowr #\U  0 usbdevfs-ctrltransfer)
(define-ioctl USBDEVFS_BULK              :iowr #\U  2 usbdevfs-bulktransfer)
(define-ioctl USBDEVFS_RESETEP           :ior  #\U  3 :uint)
(define-ioctl USBDEVFS_SETINTERFACE      :ior  #\U  4 usbdevfs-setinterface)
(define-ioctl USBDEVFS_SETCONFIGURATION  :ior  #\U  5 :uint)
(define-ioctl USBDEVFS_GETDRIVER         :iow  #\U  8 usbdevfs-getdriver)
(define-ioctl USBDEVFS_SUBMITURB         :ior  #\U 10 usbdevfs-urb)
;;(define-ioctl USBDEVFS_SUBMITURB32       :ior  #\U 10 usbdevfs-urb32)
(define-ioctl USBDEVFS_DISCARDURB        :io   #\U 11)
(define-ioctl USBDEVFS_REAPURB           :iow  #\U 12 :pointer)
;;(define-ioctl USBDEVFS_REAPURB32         :iow  #\U 12 :uint32)
(define-ioctl USBDEVFS_REAPURBNDELAY     :iow  #\U 13 :pointer)
;;(define-ioctl USBDEVFS_REAPURBNDELAY32   :iow  #\U 13 :uint32)
(define-ioctl USBDEVFS_DISCSIGNAL        :ior  #\U 14 usbdevfs-disconnectsignal)
(define-ioctl USBDEVFS_CLAIMINTERFACE    :ior  #\U 15 :uint)
(define-ioctl USBDEVFS_RELEASEINTERFACE  :ior  #\U 16 :uint)
(define-ioctl USBDEVFS_CONNECTINFO       :iow  #\U 17 usbdevfs-connectinfo)
(define-ioctl USBDEVFS_IOCTL             :iowr #\U 18 usbdevfs-ioctl)
;;(define-ioctl USBDEVFS_IOCTL32           :iowr #\U 18 usbdevfs-ioctl32)
(define-ioctl USBDEVFS_HUB_PORTINFO      :ior  #\U 19 usbdevfs-hub-portinfo)
(define-ioctl USBDEVFS_RESET             :io   #\U 20)
(define-ioctl USBDEVFS_CLEAR_HALT        :ior  #\U 21 :uint)
(define-ioctl USBDEVFS_DISCONNECT        :io   #\U 22)
(define-ioctl USBDEVFS_CONNECT           :io   #\U 23)


;;; Extra syscall return values

(defconstant ENODATA 61)
(defconstant ETIMEDOUT 110)


;;; Thin portability aide.

#+(and (not sbcl) unix)
(defcfun ioctl :int
  (fd :uint)
  (ioctl-nr :int)
  (param :pointer))

(defun stream-ioctl (stream ioctl param)
  "Invoke a UNIX IOCTL with PARAM on the fd associated with STREAM.
PARAM must be a CFFI pointer."
  #-(or sbcl ecl ccl)
  (error "Not implemented: ~S." 'stream-ioctl)
  #+sbcl
  (sb-unix:unix-ioctl (sb-sys:fd-stream-fd stream) ioctl param)
  #+ecl
  (ioctl (si:file-stream-fd stream) ioctl param)
  #+ccl
  (ioctl (ccl::ioblock-device (ccl::stream-ioblock stream t)) ioctl param))

(defun decode-errno (errno)
  "Produce a string explanation of ERRNO."
  #-sbcl (declare (ignore errno))
  #+sbcl (sb-int:strerror errno))

;;; EOF
