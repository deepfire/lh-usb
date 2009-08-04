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
		       operation errno (decode-errno errno))))))

(define-condition usb-timeout-error (usb-ioctl-error)
  ()
  (:report (lambda (condition stream)
	     (format stream "~S timed out"
		     (usb-error-operation condition)))))


;;; USB wrappers

(defun usb-disconnect (dev interface)
  "Persuade the kernel driver to relinquish its claim upon a device."
  (with-foreign-object (ctrl 'usbdevfs-ioctl)
    (setf (foreign-slot-value ctrl 'usbdevfs-ioctl 'ioctl-code) USBDEVFS_DISCONNECT
          (foreign-slot-value ctrl 'usbdevfs-ioctl 'ifno) interface
          (foreign-slot-value ctrl 'usbdevfs-ioctl 'data) (null-pointer))
    (multiple-value-bind (successp error) (stream-ioctl dev USBDEVFS_IOCTL ctrl)
      (unless successp
	(error 'usb-ioctl-error :device dev
	       :operation 'usbdevfs_disconnnect :errno error)))))

(defun usb-connect (dev interface)
  "Request the kernel driver to reassert its claim upon a device."
  (with-foreign-object (ctrl 'usbdevfs-ioctl)
    (setf (foreign-slot-value ctrl 'usbdevfs-ioctl 'ioctl-code) USBDEVFS_CONNECT
          (foreign-slot-value ctrl 'usbdevfs-ioctl 'ifno) interface
          (foreign-slot-value ctrl 'usbdevfs-ioctl 'data) (null-pointer))
    (multiple-value-bind (successp error) (stream-ioctl dev USBDEVFS_IOCTL ctrl)
      (unless successp
	(error 'usb-ioctl-error :device dev
	       :operation 'usbdevfs_connect :errno error)))))

(defun usb-claim-interface (dev interface)
  "Prevent other drivers from using an interface."
  (with-foreign-object (iface :ulong)
    (setf (mem-ref iface :ulong) interface)
    (multiple-value-bind (successp error) (stream-ioctl dev USBDEVFS_CLAIMINTERFACE iface)
      (unless successp
        (error 'usb-ioctl-error :device dev
               :operation 'usbdevfs_claiminterface :errno error)))))

(defun usb-release-interface (dev interface)
  "Allow other drivers to use an interface."
  (with-foreign-object (iface :ulong)
    (setf (mem-ref iface :ulong) interface)
    (multiple-value-bind (successp error) (stream-ioctl dev USBDEVFS_RELEASEINTERFACE iface)
      (unless successp
	(error 'usb-ioctl-error :device dev
	       :operation 'usbdevfs_releaseinterface :errno error)))))

(defun usb-get-driver (dev interface)
  "Find out which driver has claimed an interface."
  (with-foreign-object (driver 'usbdevfs-getdriver)
    (setf (foreign-slot-value driver 'usbdevfs-getdriver 'interface) interface)
    (multiple-value-bind (successp error) (stream-ioctl dev USBDEVFS_GETDRIVER driver)
      (unless successp
	(if (= error ENODATA)
	    (return-from usb-get-driver nil)
	    (error 'usb-ioctl-error :device dev
		   :operation 'usbdevfs_getdriver :errno error))))
    (foreign-string-to-lisp (foreign-slot-value driver 'usbdevfs-getdriver 'driver))))

(defun usb-reset (dev)
  "Persuade the kernel driver to perform a USB device reset."
  (multiple-value-bind (successp error) (stream-ioctl dev USBDEVFS_RESET 0)
    (unless successp
      (error 'usb-ioctl-error :device dev
             :operation 'usbdevfs_reset :errno error))))

(defun usb-control (dev request-type request value index iovec)
  "Synchronous USB control transfer."
  (with-foreign-object (ctrl 'usbdevfs-ctrltransfer)
    (with-pointer-to-vector-data (data iovec)
      (setf (foreign-slot-value ctrl 'usbdevfs-ctrltransfer 'request-type) request-type
            (foreign-slot-value ctrl 'usbdevfs-ctrltransfer 'request) request
            (foreign-slot-value ctrl 'usbdevfs-ctrltransfer 'value) value
            (foreign-slot-value ctrl 'usbdevfs-ctrltransfer 'index) index
            (foreign-slot-value ctrl 'usbdevfs-ctrltransfer 'length) (length iovec)
            (foreign-slot-value ctrl 'usbdevfs-ctrltransfer 'timeout) 50
            (foreign-slot-value ctrl 'usbdevfs-ctrltransfer 'data) data)
      (multiple-value-bind (successp error) (stream-ioctl dev USBDEVFS_CONTROL ctrl)
        (unless successp
          (if (= error ETIMEDOUT)
              (error 'usb-timeout-error :device dev
                     :operation 'usbdevfs_control :errno error)
              (error 'usb-ioctl-error :device dev
                     :operation 'usbdevfs_control :errno error))))
      (foreign-slot-value ctrl 'usbdevfs-ctrltransfer 'length))))

(defun usb-bulk (dev endpoint iovec &optional (offset 0) (iolen (- (length iovec) offset)))
  "Synchronous USB bulk transfer."
  (with-foreign-object (bulk 'usbdevfs-bulktransfer)
    (with-pointer-to-vector-data (data-ptr iovec)
      (setf (foreign-slot-value bulk 'usbdevfs-bulktransfer 'endpoint) endpoint
            (foreign-slot-value bulk 'usbdevfs-bulktransfer 'length) iolen
            (foreign-slot-value bulk 'usbdevfs-bulktransfer 'timeout) 50
            (foreign-slot-value bulk 'usbdevfs-bulktransfer 'data) (inc-pointer data-ptr offset))
      (multiple-value-bind (successp error) (stream-ioctl dev USBDEVFS_BULK bulk)
        (unless successp
          (if (= error ETIMEDOUT)
              (error 'usb-timeout-error :device dev
                     :operation 'usbdevfs_bulk :errno error)
              (error 'usb-ioctl-error :device dev
                     :operation 'usbdevfs_bulk :errno error))))
      (foreign-slot-value bulk 'usbdevfs-bulktransfer 'length))))

(defun usb-clear-halt (dev endpoint)
  "Clear the halt feature of an USB interface endpoint."
  (with-foreign-object (endpoint-arg :uint)
    (setf (mem-ref endpoint-arg :uint) endpoint)
    (multiple-value-bind (successp error) (stream-ioctl dev USBDEVFS_CLEAR_HALT endpoint-arg)
      (unless successp
        (error 'usb-ioctl-error :device dev
               :operation 'usbdevfs_clear_halt :errno error)))))

#|

For those reading this who want sample code for performing an
interrupt (or possibly isochronous) transfer, the following is
something I used a while back, but never bothered figuring out how to
provide a high-level interface for.

(with-foreign-objects ((urb usbdevfs-urb)
                       (buf :uchar 8)
                       (ptr (:pointer usbdevfs-urb)))
  (setf (foreign-slot-value urb 'type) +usbdevfs-urb-type-interrupt+)
  (setf (foreign-slot-value urb 'endpoint) #x81)
  (setf (foreign-slot-value urb 'flags) 0)
  (setf (foreign-slot-value urb 'buffer) buf)
  (setf (foreign-slot-value urb 'buffer-length) 8)
  (setf (foreign-slot-value urb 'signr) #xffffffff)
  (setf (foreign-slot-value urb 'actual-length) 0)
  (setf (foreign-slot-value urb 'number-of-packets) 0)
  (setf (foreign-slot-value urb 'usercontext) nil)

  (setf (foreign-slot-value urb 'error-count) 0)
  (setf (foreign-slot-value urb 'start-frame) 0)
  (setf (foreign-slot-value urb 'status) 0)
	  
  (multiple-value-bind (successp error)
      (sb-unix:unix-ioctl (sb-sys:fd-stream-fd cl-user::*dev*)
                          USBDEVFS_SUBMITURB
                          urb)
    (unless successp
      (error "USBDEVFS_SUBMITURB failed: ~S" (sb-int:strerror error))))
	  
  (setf ptr nil)
  (multiple-value-bind (successp error)
      (sb-unix:unix-ioctl (sb-sys:fd-stream-fd cl-user::*dev*)
                          USBDEVFS_REAPURB
                          ptr)
    (unless successp
      (error "USBDEVFS_REAPURB failed: ~S" (sb-int:strerror error))))
	  
  (inspect (list urb buf ptr)))
|#

;;; EOF
