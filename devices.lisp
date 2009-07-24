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
;;; devices.lisp
;;;
;;; Functions for working with device files.
;;;

(cl:in-package :lh-usb)

(defstruct usb-device 
  (bus-nr nil :type (unsigned-byte 16))
  (device-nr nil :type (unsigned-byte 16))
  (vendor-id nil :type (unsigned-byte 16))
  (product-id nil :type (unsigned-byte 16)))

(defun sysfs-query-usb-devices ()
  "Returns the list of bus/device id pairs, using the sysfs interface."
  ;; First find out real devices with device and vendor IDs, then work
  ;; down from that.
  (let ((actual-devices (directory #p"/sys/bus/usb/devices/*/idProduct")))
    (flet ((read-hex-token (directory name)
             (with-open-file (f (make-pathname :directory directory :name name))
               (let ((*read-base* #x10)
                     (*read-eval* nil))
                 (read f)))))
      (mapcar (lambda (dir)
                (make-usb-device :bus-nr (read-hex-token dir "busnum")
                                 :device-nr (read-hex-token dir "devnum")
                                 :vendor-id (read-hex-token dir "idVendor")
                                 :product-id (read-hex-token dir "idProduct")))
              (mapcar #'pathname-directory actual-devices)))))


(defun open-usb-device (bus device)
  (let ((device-file (format nil "/dev/bus/usb/~3,'0D/~3,'0D" bus device)))
    (open device-file
	  :direction :io
	  :if-exists :overwrite
	  :element-type '(unsigned-byte 8))))

(defun find-usb-devices-by-id (vendor prodid)
  "Returns a list of (bus device) lists for each USB device attached
to the system with a given VENDOR and PRODID (both (unsigned-byte 16)
values)."
  (loop :for dev :in (sysfs-query-usb-devices)
     :when (and (= vendor (usb-device-vendor-id dev))
                (= prodid (usb-device-product-id dev)))
     :collect (list (usb-device-bus-nr dev) (usb-device-device-nr dev))))

;;; EOF
