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
;;; package.lisp
;;;
;;; Package definitions for lh-usb.
;;;

(cl:defpackage :lh-usb-ioctls
  (:use :common-lisp :sb-alien)
  (:export

   ;; Structure names.
   "USBDEVFS-CTRLTRANSFER"
   "USBDEVFS-BULKTRANSFER"
   "USBDEVFS-SETINTERFACE"
   "USBDEVFS-DISCONNECTSIGNAL"
   "USBDEVFS-GETDRIVER"
   "USBDEVFS-CONNECTINFO"
   "USBDEVFS-ISO-PACKET-DESC"
   "USBDEVFS-URB"
   "USBDEVFS-IOCTL"
   "USBDEVFS-HUB-PORTINFO"

   ;; Structure field names.
   "REQUEST-TYPE"
   "REQUEST"
   "VALUE"
   "INDEX"
   "LENGTH"
   "TIMEOUT"
   "DATA"
   "ENDPOINT"
   "INTERFACE"
   "ALTSETTING"
   "SIGNR"
   "CONTEXT"
   "DRIVER"
   "DEVNUM"
   "SLOW"
   "ACTUAL-LENGTH"
   "STATUS"
   "FLAGS"
   "BUFFER"
   "BUFFER-LENGTH"
   "START-FRAME"
   "NUMBER-OF-PACKETS"
   "ERROR-COUNT"
   "USERCONTEXT"
   "ISO-FRAME-DESC"
   "IFNO"
   "IOCTL-CODE"
   "NPORTS"
   "PORT"

   ;; Supporting constants.
   "+USBDEVFS-MAXDRIVERNAME+"
   "+USBDEVFS-URB-SHORT-NOT-OK+"
   "+USBDEVFS-URB-ISO-ASAP+"
   "+USBDEVFS-URB-TYPE-ISO+"
   "+USBDEVFS-URB-TYPE-INTERRUPT+"
   "+USBDEVFS-URB-TYPE-CONTROL+"
   "+USBDEVFS-URB-TYPE-BULK+"
   "ETIMEDOUT"
   "ENODATA"

   ;; IOCTLs.
   "USBDEVFS_CONTROL"
   "USBDEVFS_BULK"
   "USBDEVFS_RESETEP"
   "USBDEVFS_SETINTERFACE"
   "USBDEVFS_SETCONFIGURATION"
   "USBDEVFS_GETDRIVER"
   "USBDEVFS_SUBMITURB"
   "USBDEVFS_DISCARDURB"
   "USBDEVFS_REAPURB"
   "USBDEVFS_REAPURBNDELAY"
   "USBDEVFS_DISCSIGNAL"
   "USBDEVFS_CLAIMINTERFACE"
   "USBDEVFS_RELEASEINTERFACE"
   "USBDEVFS_CONNECTINFO"
   "USBDEVFS_IOCTL"
   "USBDEVFS_HUB_PORTINFO"
   "USBDEVFS_RESET"
   "USBDEVFS_CLEAR_HALT"
   "USBDEVFS_DISCONNECT"
   "USBDEVFS_CONNECT"))

(cl:defpackage :lh-usb
  (:use :common-lisp :sb-alien :lh-usb-ioctls)
  (:export

   ;; Status / Management wrappers.
   "USB-DISCONNECT"
   "USB-CONNECT"
   "USB-CLAIM-INTERFACE"
   "USB-RELEASE-INTERFACE"
   "USB-GET-DRIVER"
   "USB-RESET"

   ;; Synchronous transfer wrappers.
   "USB-CONTROL"
   "USB-BULK"
   "USB-CLEAR-HALT"

   ;; Exceptional situations.
   "USB-ERROR"
   "USB-ERROR-DEVICE"
   "USB-ERROR-OPERATION"
   "USB-IOCTL-ERROR"
   "USB-IOCTL-ERROR-DEVICE"
   "USB-IOCTL-ERROR-OPERATION"
   "USB-IOCTL-ERROR-ERRNO"
   "USB-TIMEOUT-ERROR"

   ;; Finding devices.
   "SYSFS-QUERY-USB-DEVICES"
   "OPEN-USB-DEVICE"
   "FIND-USB-DEVICES-BY-ID"))

;;; EOF
