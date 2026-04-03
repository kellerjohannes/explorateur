(in-package :explorateur)

(defparameter *socket* nil)

(defun open-udp-connection (host port)
  (setf *socket* (usocket:socket-connect host port :protocol :datagram)))

(defun close-udp-connection ()
  (usocket:socket-close *socket*))

(defun send-udp-packet (group-id sequence values)
  (check-type group-id (unsigned-byte 8))
  (check-type sequence (unsigned-byte 8))
  (let ((packet (make-array (+ 2 (* 2 (length values))) :element-type '(unsigned-byte 8))))
    (setf (aref packet 0) group-id)
    (setf (aref packet 1) sequence)

    (loop for v in values
          for i from 0 do
          (setf (aref packet (+ 2 (* i 2))) (ldb (byte 8 8) v))
          (setf (aref packet (+ 3 (* i 2))) (ldb (byte 8 0) v)))

    (usocket:socket-send *socket* packet (length packet))))



(defun send-udp-message (module valve pressure)
  (check-type module (unsigned-byte 8))
  (check-type valve (unsigned-byte 8))
  (check-type pressure (unsigned-byte 16))
  (let ((packet (make-array 4 :element-type '(unsigned-byte 8))))
    (setf (aref packet 0) module)
    (setf (aref packet 1) valve)
    (setf (aref packet 2) (ldb (byte 8 8) pressure))
    (setf (aref packet 3) (ldb (byte 8 0) pressure))
    (usocket:socket-send *socket* packet 4)))



;; (open-udp-connection "127.0.0.1" 8888)
;; (send-udp-message 128 128 65535)
;; (/ (/ (* 4 8 1000 10000) 1000) 1000)
