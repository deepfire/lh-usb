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

(defun file-abuse-complaint (file)
  "Return contents of select(2)-abuse-prone files as a string."
  #+sbcl
  (with-open-file (devlist file)
    ;; This "file" violates the usual contract of select(2). An
    ;; input-ready condition on the file indicates that a device has
    ;; been connected or disconnected, not that there happens to be
    ;; more data to read. No select(2) means no serve-event. No
    ;; serve-event means that we can't use the standard Lisp file I/O,
    ;; and have to do our own buffering... And, while we're at it, we
    ;; won't bother doing our own error handling.
    (let* ((buffers
     (loop for buf = (make-array #x200
     :element-type '(unsigned-byte 8)
     :fill-pointer t)
    for (len error) = (multiple-value-list
         (sb-unix:unix-read
          (sb-sys:fd-stream-fd devlist)
          (sb-sys:vector-sap
           (sb-kernel:%array-data-vector buf))
          #x200))
    until (or (null len) (zerop len))
    do (setf (fill-pointer buf) len)
    collect buf))
    (combined-buffer (apply #'concatenate
       '(vector (unsigned-byte 8)) buffers))
    (file-contents (sb-ext:octets-to-string combined-buffer)))
      file-contents))
  #-sbcl
  (let ((vector (make-array (file-length file) :element-type 'base-char)))
    (read-sequence vector file)
    vector))

(defun sysfs-query-usb-devices ()
  "Returns the list of bus/device id pairs, using the sysfs interface."
  ;; First find out real devices with device and vendor IDs, then work
  ;; down from that.
  (let ((actual-devices (mapcan (lambda (x)
                                  (directory (make-pathname :name "idProduct" :defaults x)))
                                (mapcan #'directory (directory #P"/sys/bus/usb/devices/*"
                                                               #+ccl :follow-links #+ccl nil)))))
    (labels ((deduce-device-bus-number-on-old-kernels (directory)
               "This, in particular, pertains to 2.6.18, as used on CentOS 5.4"
               (flet ((dirname-to-busnum (name)
                        (let* ((dash-pos (position #\- name))
                               (colon-pos (when dash-pos (position #\: name :start (1+ dash-pos))))
                               (dot-pos (when colon-pos (position #\. name :start (1+ colon-pos)))))
                          (when (and dot-pos
                                     (parse-integer name :junk-allowed t :start (1+ dash-pos) :end colon-pos)
                                     (parse-integer name :junk-allowed t :start (1+ colon-pos) :end dot-pos)
                                     (parse-integer name :junk-allowed t :start (1+ dot-pos)))
                            (parse-integer name :junk-allowed t :end dash-pos)))))
                 (dolist (dir (directory (make-pathname :directory `(,@directory :wild))))
                   (let ((number (dirname-to-busnum (car (last (pathname-directory dir))))))
                     (when number
                       (return number))))))
             (token-path (directory name)
               (make-pathname :directory directory :name name))
             (read-token (directory name base)
               (parse-integer (file-abuse-complaint (token-path directory name)) :radix base)))
      (mapcar (lambda (dir)
                (make-usb-device :bus-nr (if (open (token-path dir "busnum") :direction :probe :if-does-not-exist nil)
                                             (read-token dir "busnum" 10)
                                             (or (deduce-device-bus-number-on-old-kernels dir)
                                                 (error "Kernel is so old, its sysfs doesn't tell us enough about USB device bus numbers.")))
                                 :device-nr (read-token dir "devnum" 10)
                                 :vendor-id (read-token dir "idVendor" #x10)
                                 :product-id (read-token dir "idProduct" #x10)))
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
