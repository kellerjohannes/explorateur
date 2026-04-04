(in-package :explorateur)

(defparameter *midi-connections* (make-hash-table))

(defclass midi-connection ()
  ((direction :initform :input :initarg :directon :accessor direction)
   (port-name :initform "default MIDI in" :initarg :port-name :accessor port-name)
   (midi-stream :initform nil :accessor midi-stream)
   (callbacks :initform nil :accessor callbacks)))

(defmethod connect ((connection midi-connection))
  (setf (midi-stream connection)
        (jackmidi:open :direction (direction connection) :port-name (port-name connection))))

(defmethod send ((connection midi-connection))
  (jackmidi:write-short (midi-stream connection) (jackmidi:message status data1 data2) 3))

(defmethod start-responder-loop ((connection midi-connection))
  (incudine:recv-start (midi-stream connection)))

(defmethod stop-responder-loop ((connection midi-connection))
  (incudine:recv-stop (midi-stream connection)))

(defmethod initialize-instance :after ((connection midi-connection)
                                       &rest initargs
                                       &key &allow-other-keys)
  (declare (ignore initargs))
  (setf (callbacks connection) (make-hash-table))
  (when (and (midi-stream connection) (jackmidi:open-p (midi-stream connection)))
    (jackmidi:close (midi-stream connection)))
  (connect connection)
  (incudine:remove-all-responders (midi-stream connection))
  (incudine:make-responder (midi-stream connection)
                           (lambda (status data1 data2)
                             (call-callbacks connection status data1 data2)))
  (start-responder-loop connection))


(defun make-hash-key (status data1)
  (alexandria:make-keyword (string-upcase (format nil "M~a-~a" status data1))))

(defmethod add-callback ((connection midi-connection) status data1 callback-fun)
  (push callback-fun (gethash (make-hash-key status data1) (callbacks connection))))

(defmethod remove-callbacks ((connection midi-connection) status data1)
  (remhash (make-hash-key status data1) (callbacks connection)))

(defmethod call-callbacks ((connection midi-connection) status data1 data2)
  (let ((fun-list (gethash (make-hash-key status data1) (callbacks connection))))
    (when fun-list
      (dolist (callback fun-list)
        (when (functionp callback)
          (funcall callback status data1 data2))))))




(defun register-midi-connection (id port-name direction)
  (setf (gethash id *midi-connections*) (make-instance 'midi-connection
                                                       :direction direction
                                                       :port-name port-name)))

(defun get-midi-connection-instance (connection-id)
  (gethash connection-id *midi-connections*))

(defun register-midi-callback (connection-id status data1 callback-fun)
  (add-callback (get-midi-connection-instance connection-id) status data1 callback-fun))

(defun start-all-responders ()
  (loop for connection being the hash-values of *midi-connections* do
        (start-responder-loop connection)))



;; (register-midi-connection :triple-midi-kbd-a "MIDI kbd A" :input)
;; (register-midi-connection :triple-midi-kbd-b "MIDI kbd B" :input)


;; (register-midi-callback :triple-midi-kbd-b 144 64 (lambda (status data1 data2) (declare (ignore status data1 data2)) (format t "~&Note on 64 received!~%")))

;; (register-midi-callback :triple-midi-kbd-b 144 64 (lambda (status data1 data2) (declare (ignore status data1 data2)) (format t "~&Note on 64 received as well!~%")))

;; (register-midi-callback :triple-midi-kbd-b 144 65 (lambda (status data1 data2) (declare (ignore status data1 data2)) (format t "~&Note on 65 received!~%")))
