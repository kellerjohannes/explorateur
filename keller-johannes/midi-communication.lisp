(in-package :explorateur.midi)

(defparameter *midi-monitor-number-of-lines* 15)
(defparameter *midi-monitor* nil)
(defparameter *midi-monitor-cursor* 0)

(defparameter *midi-monitor-gui-hooks* '())

(defun add-midi-monitor-hook (hook-fun)
  (push hook-fun *midi-monitor-gui-hooks*))

(defmacro inc-midi-monitor-cursor (field)
  `(if (< ,field (1- *midi-monitor-number-of-lines*))
       (incf ,field)
       (setf ,field 0)))

;; TODO check if ever used
(defmacro dec-midi-monitor-cursor (field)
  `(if (<= ,field 0)
       (setf ,field (1- *midi-monitor-number-of-lines*))
       (decf ,field)))


(defun dump-midi-monitor ()
  (let ((result (make-array *midi-monitor-number-of-lines*)))
    (loop for line across *midi-monitor*
          for new-index from 0
          with cursor = *midi-monitor-cursor*
          do
             (setf (aref result new-index) (aref *midi-monitor* cursor))
             (inc-midi-monitor-cursor cursor))
    result))

(defun call-midi-monitor-hooks ()
  (dolist (hook *midi-monitor-gui-hooks*)
    (funcall hook (dump-midi-monitor))))

(defun clear-midi-monitor ()
  (setf *midi-monitor-cursor* 0)
  (setf *midi-monitor* (make-array *midi-monitor-number-of-lines* :initial-element "[empty]"))
  (call-midi-monitor-hooks))

(clear-midi-monitor)

(defun push-midi-monitor-line (time type data1 data2)
  (setf (aref *midi-monitor* *midi-monitor-cursor*)
        (format nil "~a ~a ~a ~a"
                time
                type
                data1
                data2))
  (inc-midi-monitor-cursor *midi-monitor-cursor*)
  (call-midi-monitor-hooks))


;; TODO Might never be used: delete?
(defun max-number-midi-lines ()
  *midi-monitor-number-of-lines*)





(defparameter *midi-connections* (make-hash-table))

(defclass midi-connection ()
  ((direction :initform :input :initarg :directon :accessor direction)
   (port-name :initform "default MIDI in" :initarg :port-name :accessor port-name)
   (midi-stream :initform nil :accessor midi-stream)
   (callbacks :initform nil :accessor callbacks)))

(defmethod connect ((connection midi-connection))
  (setf (midi-stream connection)
        (jackmidi:open :direction (direction connection) :port-name (port-name connection))))

(defmethod send ((connection midi-connection) status data1 data2)
  (jackmidi:write-short (midi-stream connection) (jackmidi:message status data1 data2) 3))

(defmethod start-responder-loop ((connection midi-connection))
  (incudine:recv-start (midi-stream connection)))

(defmethod stop-responder-loop ((connection midi-connection))
  (incudine:recv-stop (midi-stream connection)))

(defun make-hash-key (status data1)
  (alexandria:make-keyword (string-upcase (format nil "M~a-~a" status data1))))

(defmethod call-callbacks ((connection midi-connection) status data1 data2)
  (format t "~&DEBUG: ~a" status)
  (push-midi-monitor-line (incudine:now) status data1 data2)
  (let ((fun-list (gethash (make-hash-key status data1) (callbacks connection))))
    (when fun-list
      (dolist (callback fun-list)
        (when (functionp callback)
          (funcall callback status data1 data2))))))

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

(defmethod add-callback ((connection midi-connection) status data1 callback-fun)
  (push callback-fun (gethash (make-hash-key status data1) (callbacks connection))))

(defmethod remove-callbacks ((connection midi-connection) status data1)
  (remhash (make-hash-key status data1) (callbacks connection)))




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
