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
;;; wrappers.lisp
;;;
;;; Wrappers for usbdevfs IOCTLs.
;;;

(cl:in-package :lh-usb)


;;; Error conditions

(define-condition usb-error (error)
  ((device :initarg :device :reader usb-error-device)
   (operation :initarg :operation :reader usb-error-operation)))

(define-condition usb-ioctl-error (usb-error)
  ;; FIXME: It'd be nice if there were an obvious way to go from errno
  ;; to the name of the error (not the user-readable string, but a
  ;; constant as would be used in source code).
  ((device :reader usb-ioctl-error-device)
   (operation :reader usb-ioctl-error-operation)
   (errno :initarg :errno :reader usb-ioctl-error-errno))
  (:report (lambda (condition stream)
	     (let ((operation (usb-ioctl-error-operation condition))
		   (errno (usb-ioctl-error-errno condition)))
	       (format stream "~S failed, errno ~S (~S)"
		       operation errno (sb-int:strerror errno))))))

(define-condition usb-timeout-error (usb-ioctl-error)
  ()
  (:report (lambda (condition stream)
	     (format stream "~S timed out"
		     (usb-error-operation condition)))))


;;; USB wrappers

(defun usb-disconnect (dev interface)
  "Persuade the kernel driver to relinquish its claim upon a device."
  (with-alien ((ctrl usbdevfs-ioctl))
    (setf (slot ctrl 'ioctl-code) USBDEVFS_DISCONNECT)
    (setf (slot ctrl 'ifno) interface)
    (setf (slot ctrl 'data) nil)
    (multiple-value-bind (successp error)
	(sb-unix:unix-ioctl (sb-sys:fd-stream-fd dev)
			    USBDEVFS_IOCTL
			    (alien-sap (addr ctrl)))
      (unless successp
	(error 'usb-ioctl-error :device dev
	       :operation 'usbdevfs_disconnnect :errno error)))))

(defun usb-connect (dev interface)
  "Request the kernel driver to reassert its claim upon a device."
  (with-alien ((ctrl usbdevfs-ioctl))
    (setf (slot ctrl 'ioctl-code) USBDEVFS_CONNECT)
    (setf (slot ctrl 'ifno) interface)
    (setf (slot ctrl 'data) nil)
    (multiple-value-bind (successp error)
	(sb-unix:unix-ioctl (sb-sys:fd-stream-fd dev)
			    USBDEVFS_IOCTL
			    (alien-sap (addr ctrl)))
      (unless successp
	(error 'usb-ioctl-error :device dev
	       :operation 'usbdevfs_connect :errno error)))))

(defun usb-claim-interface (dev interface)
  "Prevent other drivers from using an interface."
  (with-alien ((iface unsigned-long))
    (setf iface interface)
    (multiple-value-bind (successp error)
	(sb-unix:unix-ioctl (sb-sys:fd-stream-fd dev)
			    USBDEVFS_CLAIMINTERFACE
			    (alien-sap (addr iface)))
      (unless successp
	(error 'usb-ioctl-error :device dev
	       :operation 'usbdevfs_claiminterface :errno error)))))

(defun usb-release-interface (dev interface)
  "Allow other drivers to use an interface."
  (with-alien ((iface unsigned-long))
    (setf iface interface)
    (multiple-value-bind (successp error)
	(sb-unix:unix-ioctl (sb-sys:fd-stream-fd dev)
			    USBDEVFS_RELEASEINTERFACE
			    (alien-sap (addr iface)))
      (unless successp
	(error 'usb-ioctl-error :device dev
	       :operation 'usbdevfs_releaseinterface :errno error)))))

(defun usb-get-driver (dev interface)
  "Find out which driver has claimed an interface."
  (with-alien ((driver usbdevfs-getdriver))
    (setf (slot driver 'interface) interface)
    (multiple-value-bind (successp error)
	(sb-unix:unix-ioctl (sb-sys:fd-stream-fd dev)
			    USBDEVFS_GETDRIVER
			    (alien-sap (addr driver)))
      (unless successp
	(if (= error ENODATA)
	    (return-from usb-get-driver nil)
	    (error 'usb-ioctl-error :device dev
		   :operation 'usbdevfs_getdriver :errno error))))
    (cast (slot driver 'driver) c-string)))

(defun usb-reset (dev)
  "Persuade the kernel driver to perform a USB device reset."
  (multiple-value-bind (successp error)
      (sb-unix:unix-ioctl (sb-sys:fd-stream-fd dev)
                          USBDEVFS_RESET
                          0)
    (unless successp
      (error 'usb-ioctl-error :device dev
             :operation 'usbdevfs_reset :errno error))))

(defun usb-control (dev request-type request value index data)
  "Synchronous USB control transfer."
  (with-alien ((ctrl usbdevfs-ctrltransfer))
    (setf (slot ctrl 'request-type) request-type
	  (slot ctrl 'request) request
	  (slot ctrl 'value) value
	  (slot ctrl 'index) index
	  (slot ctrl 'length) (length data)
	  (slot ctrl 'timeout) 50
	  (slot ctrl 'data) (sb-sys:vector-sap (sb-kernel:%array-data-vector data)))
    (multiple-value-bind (successp error)
	(sb-unix:unix-ioctl (sb-sys:fd-stream-fd dev)
			    USBDEVFS_CONTROL
			    (alien-sap (addr ctrl)))
      (unless successp
	(if (= error ETIMEDOUT)
	    (error 'usb-timeout-error :device dev
		   :operation 'usbdevfs_control :errno error)
	    (error 'usb-ioctl-error :device dev
		   :operation 'usbdevfs_control :errno error))))
    (setf (fill-pointer data) (slot ctrl 'length))))

(defun usb-bulk (dev endpoint data &optional (offset 0))
  "Synchronous USB bulk transfer."
  (with-alien ((bulk usbdevfs-bulktransfer))
    (setf (slot bulk 'endpoint) endpoint
	  (slot bulk 'length) (- (length data) offset)
	  (slot bulk 'timeout) 50
	  (slot bulk 'data) (sb-sys:sap+ (sb-sys:vector-sap (sb-kernel:%array-data-vector data)) offset))
    (multiple-value-bind (successp error)
	(sb-unix:unix-ioctl (sb-sys:fd-stream-fd dev)
			    USBDEVFS_BULK
			    (alien-sap (addr bulk)))
      (unless successp
	(if (= error ETIMEDOUT)
	    (error 'usb-timeout-error :device dev
		   :operation 'usbdevfs_bulk :errno error)
	    (error 'usb-ioctl-error :device dev
		   :operation 'usbdevfs_bulk :errno error))))
    (setf (fill-pointer data) (+ (slot bulk 'length) offset))))

(defun usb-clear-halt (dev endpoint)
  "Clear the halt feature of an USB interface endpoint."
  (with-alien ((endpoint-arg unsigned-int))
    (setf endpoint-arg endpoint)
    (multiple-value-bind (successp error)
	(sb-unix:unix-ioctl (sb-sys:fd-stream-fd dev)
			    USBDEVFS_CLEAR_HALT
			    (alien-sap (addr endpoint-arg)))
      (unless successp
        (error 'usb-ioctl-error :device dev
               :operation 'usbdevfs_clear_halt :errno error)))))

#|

For those reading this who want sample code for performing an
interrupt (or possibly isochronous) transfer, the following is
something I used a while back, but never bothered figuring out how to
provide a high-level interface for.

(with-alien ((urb usbdevfs-urb)
		     (buf (array (unsigned 8) 8))
		     (ptr (* usbdevfs-urb)))
	  (setf (slot urb 'type) +usbdevfs-urb-type-interrupt+)
	  (setf (slot urb 'endpoint) #x81)
	  (setf (slot urb 'flags) 0)
	  (setf (slot urb 'buffer) (addr buf))
	  (setf (slot urb 'buffer-length) 8)
	  (setf (slot urb 'signr) #xffffffff)
	  (setf (slot urb 'actual-length) 0)
	  (setf (slot urb 'number-of-packets) 0)
	  (setf (slot urb 'usercontext) nil)

	  (setf (slot urb 'error-count) 0)
	  (setf (slot urb 'start-frame) 0)
	  (setf (slot urb 'status) 0)
	  
	  (multiple-value-bind (successp error)
	      (sb-unix:unix-ioctl (sb-sys:fd-stream-fd cl-user::*dev*)
				  USBDEVFS_SUBMITURB
				  (alien-sap (addr urb)))
	    (unless successp
	      (error "USBDEVFS_SUBMITURB failed: ~S" (sb-int:strerror error))))
	  
	  (setf ptr nil)
	  (multiple-value-bind (successp error)
	      (sb-unix:unix-ioctl (sb-sys:fd-stream-fd cl-user::*dev*)
				  USBDEVFS_REAPURB
				  (alien-sap (addr ptr)))
	    (unless successp
	      (error "USBDEVFS_REAPURB failed: ~S" (sb-int:strerror error))))
	  
	  (inspect (list urb buf ptr)))
|#

;;; EOF
