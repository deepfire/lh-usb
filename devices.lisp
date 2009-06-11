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


(defun read-usb-devices ()
  "Returns the contents of /proc/bus/usb/devices as a string."
  (with-open-file (devlist "/proc/bus/usb/devices")
    ;; This "file" violates the usual contract of select(2).  An
    ;; input-ready condition on the file indicates that a device has
    ;; been connected or disconnected, not that there happens to be
    ;; more data to read.  No select(2) means no serve-event.  No
    ;; serve-event means that we can't use the standard Lisp file I/O,
    ;; and have to do our own buffering...  And, while we're at it, we
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
    
      file-contents)))

(defun open-usb-device (bus device)
  (let ((device-file (format nil "/proc/bus/usb/~3,'0D/~3,'0D" bus device)))
    (open device-file
	  :direction :io
	  :if-exists :overwrite
	  :element-type '(unsigned-byte 8))))

(defun find-usb-devices-by-id (vendor prodid)
  "Returns a list of (bus device) lists for each USB device attached
to the system with a given VENDOR and PRODID (both (unsigned-byte 16)
values)."
  (with-input-from-string (device-data (read-usb-devices))
    (loop
       with devices
       with bus
       with dev
       for line = (read-line device-data nil nil)
       while line
       when (not (zerop (length line)))
       do (case (aref line 0)
	    (#\T
	     (let ((bus-position (+ (search "Bus=" line) 4))
		   (dev-position (+ (search "Dev#=" line) 5)))
	       (setf bus (parse-integer line :start bus-position
					:end (+ bus-position 2))
		     dev (parse-integer line :start dev-position
					:end (+ dev-position 3)))))
	    (#\P
	     (let* ((vendor-position (+ (search "Vendor=" line) 7))
		    (prodid-position (+ (search "ProdID=" line) 7))
		    (dev-vendor (parse-integer line :radix 16
					       :start vendor-position
					       :end (+ vendor-position 4)))
		    (dev-prodid (parse-integer line :radix 16
					       :start prodid-position
					       :end (+ prodid-position 4))))
	       (when (and (= dev-vendor vendor)
			  (= dev-prodid prodid))
		 (push (list bus dev) devices)))))
       finally (return devices))))

;;; EOF
