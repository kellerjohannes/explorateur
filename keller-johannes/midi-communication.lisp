(in-package :explorateur.midi)

(defparameter *midi-monitor-number-of-lines* 5000)
(defparameter *midi-monitor* nil)
(defparameter *midi-monitor-cursor* 0)

(defparameter *midi-monitor-gui-hooks* '())

(defun add-midi-monitor-hook (hook-fun)
  (push hook-fun *midi-monitor-gui-hooks*))

(defmacro inc-midi-monitor-cursor (field)
  `(if (< ,field (1- *midi-monitor-number-of-lines*))
       (incf ,field)
       (setf ,field 0)))

(defmacro dec-midi-monitor-cursor (field)
  `(if (<= ,field 0)
       (setf ,field (1- *midi-monitor-number-of-lines*))
       (decf ,field)))

(defun dump-midi-monitor (&optional (number-of-lines *midi-monitor-number-of-lines*))
  (let ((result (make-array number-of-lines)))
    (loop for line across *midi-monitor*
          for new-index from 0
          for countdown downfrom number-of-lines
          with cursor = (1- *midi-monitor-cursor*)
          while (> countdown 0)
          do (setf (aref result new-index) (aref *midi-monitor* cursor))
             (dec-midi-monitor-cursor cursor))
    result))

(defun call-midi-monitor-hooks ()
  (dolist (hook *midi-monitor-gui-hooks*)
    (funcall hook)))

(defun clear-midi-monitor ()
  (setf *midi-monitor-cursor* 0)
  (setf *midi-monitor* (make-array *midi-monitor-number-of-lines* :initial-element "[empty]"))
  (call-midi-monitor-hooks))

(clear-midi-monitor)

(defun lookup-status-type (status)
  (cond ((<= #x80 status #x8f) (cons :note-off (- status #x80)))
        ((<= #x90 status #x9f) (cons :note-on (- status #x90)))
        ((<= #xa0 status #xaf) (cons :key-pressure (- status #xa0)))
        ((<= #xb0 status #xbf) (cons :controller (- status #xb0)))
        ((<= #xc0 status #xcf) (cons :program-change (- status #xc0)))
        ((<= #xd0 status #xdf) (cons :pitch-bend (- status #xd0)))
        (t (cons :unidentified status))))


(defun lookup-status-byte (status-keyword channel)
  (case status-keyword
    (:note-off (+ #x80 channel))
    (:note-on (+ #x90 channel))
    (:key-pressure (+ #xa0 channel))
    (:controller (+ #xb0 channel))
    (:program-change (+ #xc0 channel))
    (:pitch-bend (+ #xd0 channel))
    (t
     (format t "~&Warning: MIDI msg status keyword unknown: ~a" status-keyword)
     0)))

(defun push-midi-monitor-line (connection time status data1 data2)
  (setf (aref *midi-monitor* *midi-monitor-cursor*)
        (let ((status-resolved (lookup-status-type status)))
          (format nil "~a: [@~d] ~a (~a/CH~a) | ~a | ~a"
                  (port-name connection)
                  (floor time)
                  status
                  (car status-resolved)
                  (cdr status-resolved)
                  data1
                  data2)))
  (inc-midi-monitor-cursor *midi-monitor-cursor*)
  (call-midi-monitor-hooks))


;; TODO Might never be used: delete?
(defun max-number-midi-lines ()
  *midi-monitor-number-of-lines*)





(defparameter *midi-connections* (make-hash-table))

(defclass midi-connection ()
  ((direction
    :reader direction
    :initform :input
    :initarg :direction
    :documentation
    "MIDI direction, :input or :output are possible.")
   (port-name
    :reader port-name
    :initform "default MIDI in"
    :initarg :port-name
    :documentation
    "Name of the MIDI port, visible in the MIDI handling software, for example qjackctl.")
   (midi-stream
    :initform nil
    :accessor midi-stream
    :documentation
    "Holds the actual stream object, once it's opened.")
   (dispatch-table
    :initform nil
    :accessor dispatch-table
    :documentation
    "Hash table containing callback functions for MIDI message processing."))
  (:documentation
   "Holds all data of a MIDI input or output, managed by Jackmidi.

Can be initialized before or after the realtime thread of Incudine started."))

(defmethod connect ((connection midi-connection))
  "Connect via jackmidi. This can be done before or after the real time thread started."
  (format t "~&Connectin ~a (direction ~a)."
          (port-name connection)
          (direction connection))
  (setf (midi-stream connection)
        (jackmidi:open :direction (direction connection) :port-name (port-name connection))))

(defmethod disconnect ((connection midi-connection))
  "Closes a MIDI stream via jackmidi."
  (jackmidi:close (midi-stream connection)))

(defmethod send ((connection midi-connection) status data1 data2)
  "Generic MIDI send, via jackmidi."
  (jackmidi:write-short (midi-stream connection) (jackmidi:message status data1 data2) 3))

(defmethod start-responder-loop ((connection midi-connection))
  "Start the MIDI listener for all events arriving to this MIDI connection."
  (incudine:recv-start (midi-stream connection)))

(defmethod stop-responder-loop ((connection midi-connection))
  "Stop the MIDI listener for this entire MIDI connection."
  (incudine:recv-stop (midi-stream connection)))

(defun make-hash-key (status data1)
  "These keywords are used for accessing dispatch tables (hash tables)."
  (alexandria:make-keyword (string-upcase (format nil "M~a-~a" status data1))))

(defmethod get-callback-functions ((connection midi-connection) status data1)
  "Return a list of callback functions based on the status and data1 byte of a MIDI message."
  (gethash (make-hash-key status data1) (dispatch-table connection)))

(defmethod call-callbacks ((connection midi-connection) status data1 data2)
  "Call callback functions stored in the dispatch table of the MIDI connection."
  (push-midi-monitor-line connection (inc:get-current-sample) status data1 data2)
  (dolist (callback (get-callback-functions connection status data1))
    (when (functionp callback) (funcall callback status data1 data2))))

(defmethod init-midi-connection ((connection midi-connection))
  "Handle INCUDINE and jackmidi. Can also be used to reset a connection."
  (when (and (midi-stream connection) (jackmidi:open-p (midi-stream connection)))
    (jackmidi:close (midi-stream connection)))
  (connect connection)
  (when (eq (direction connection) :input)
    (incudine:remove-all-responders (midi-stream connection))
    (incudine:make-responder (midi-stream connection)
                             (lambda (status data1 data2)
                               (funcall #'call-callbacks connection status data1 data2)))
    (start-responder-loop connection)))

(defmethod reset-dispatch-table ((connection midi-connection))
  "Deletes the entire dispatch table."
  (setf (dispatch-table connection) (make-hash-table)))

(defmethod initialize-instance :after ((connection midi-connection)
                                       &rest initargs
                                       &key &allow-other-keys)
  (declare (ignore initargs))
  (reset-dispatch-table connection)
  (init-midi-connection connection)
  (format t "~&MIDI connection ~a initialized." (port-name connection)))

(defmethod add-callback ((connection midi-connection) status data1 callback-fun)
  "Register a callback function for a MIDI status and data1 byte as identifiers.

The callback function must accept three arguments: status, data1 and data2."
  (push callback-fun (gethash (make-hash-key status data1) (dispatch-table connection))))

(defmethod remove-callbacks ((connection midi-connection) status data1)
  "Remove all callback functions from an entry in the dispatch table."
  (remhash (make-hash-key status data1) (callbacks connection)))

(defun get-midi-connection-instance (id)
  "Returns an instance of MIDI-CONNECTION stored in the MIDI connection hash table with key ID."
  (gethash id *midi-connections*))



;;; Public interface

(defun disconnect-midi-port (id)
  "Disconnects an open MIDI port referred to by ID."
  (disconnect (get-midi-connection-instance id)))

(defun register-midi-connection (id port-name direction)
  "Public function to add and open a MIDI port.

This function can be called before or after the INCUDINE real time thread has been started.  Ports
that are registered while the real time thread is off will become visible and usable as soon as the
real time thread is activated.  ID can be chosen freely and will be used as hash-key. PORT-NAME is a
string that will be visible in MIDI managing software, for example qjackctl. DIRECTION can be :input
or :output.

If a MIDI connection with this ID is already established, calling this function will close the
existing connection and establish a new one. This function can thus safely be called repeatedly
without creating errors ('port already in use' or similar) and without leading to unintended
redundancies."
  (when (gethash id *midi-connections*) (disconnect-midi-port id))
  (setf (gethash id *midi-connections*) (make-instance 'midi-connection
                                                       :direction direction
                                                       :port-name port-name)))

(defun register-midi-callback (id status data1 callback-fun)
  "Public function to add a callback function to the dispatch table of a specific MIDI port.

Each MIDI port, referred to by ID, has its own dispatch table that stores callback functions. These
functions are called when MIDI data is received. For each combination of STATUS and DATA1, there can
be a list of callback functions that are called when an incoming MIDI message contains this STATUS
and this DATA1.

This function adds one CALLBACK-FUN to the entry in the dispatch table. It must accept three
arguments representing status, data1 and data2 of a MIDI message."
  (add-callback (get-midi-connection-instance id) status data1 callback-fun))

(defun clear-dispatch-table (id)
  "Deletes the entire dispatch table of a MIDI connection with ID."
  (reset-dispatch-table (get-midi-connection-instance id)))

(defun send-msg-raw (connection-id status data1 data2)
  (send (get-midi-connection-instance connection-id) status data1 data2))

(defun send-msg (connection-id status-keyword channel data1 data2)
  (send (get-midi-connection-instance connection-id)
        (lookup-status-byte status-keyword channel) data1 data2))

(defun update-midi-valve (connection-id channel note pressure)
  (send-msg connection-id :key-pressure channel note pressure))

(defparameter *loopingp* t)
(defun stop () (setf *loopingp* nil))
(defun arm () (setf *loopingp* t))

(defun random-pressure-loop (connection-id channel note step-duration-in-samp
                             &optional (time (inc:get-current-sample)))
  (when *loopingp*
    (update-midi-valve connection-id channel note (random 128))
    (incudine:at (+ time step-duration-in-samp)
                 #'random-pressure-loop
                 connection-id
                 channel
                 note
                 step-duration-in-samp
                 (+ time step-duration-in-samp))))

(defun random-pressure-channel (connection-id channel step-duration-in-samp)
  (arm)
  (dotimes (note 128)
    (random-pressure-loop connection-id channel note step-duration-in-samp)))

(defun random-pressure (connection-id step-duration-in-s)
  (arm)
  (dotimes (ch 16)
    (random-pressure-channel connection-id ch (* (inc:get-sample-rate) step-duration-in-s))))


;; brute force
(defun stress-test-loop-1 (valve-id step-duration-in-samp &optional (time (inc:get-current-sample)))
  (when *loopingp*
    (send-osc valve-id (random 2000))
    (incudine:at (+ time step-duration-in-samp)
                 #'stress-test-loop-1
                 valve-id
                 step-duration-in-samp
                 (+ time step-duration-in-samp))))

;; brute force
(defun stress-test-osc-1 (number-of-loops step-duration-in-s)
  (arm)
  (dotimes (l number-of-loops)
    (stress-test-loop-1 l (* step-duration-in-s (inc:get-sample-rate)))))




(defparameter *num-valves* 1000)

(defparameter *valve-pressures* (make-array *num-valves* :element-type 'single-float
                                                         :initial-element 0.0f0))

(defun build-blob-message-template (num-floats)
  "Build a static OSC message with address /valves/pressure and a blob
   argument placeholder. Returns (values octets-vector blob-data-offset
   blob-size)."
  (let* ((address "/valves/pressure")
         (blob-size (* num-floats 4))  ; 1000 * 4 = 4000 bytes
         ;; Calculate padded sizes
         (addr-padded (ceiling (1+ (length address)) 4))  ; includes null
         (addr-bytes (* addr-padded 4))
         (type-tag "b")
         (type-padded (ceiling (1+ (length type-tag)) 4))
         (type-bytes (* type-padded 4))
         ;; Blob: 4 bytes size + blob-size bytes + padding to 4
         (blob-padded (ceiling blob-size 4))
         (blob-total-bytes (* blob-padded 4))
         ;; Total message size
         (total (+ addr-bytes type-bytes 4 blob-total-bytes))
         ;; Create byte vector
         (vec (make-array total :element-type '(unsigned-byte 8)
                                :initial-element 0))
         (offset 0))
    ;; Write address pattern (null-terminated, 4-byte aligned)
    (loop for c across address
          do (setf (aref vec offset) (char-code c))
             (incf offset))
    ;; null terminator + padding already zero-filled
    (setf offset addr-bytes)
    ;; Write type tag string (starts with comma)
    (loop for c across type-tag
          do (setf (aref vec offset) (char-code c))
             (incf offset))
    (setf offset type-bytes)
    ;; Write blob size as int32 big-endian
    (setf (aref vec offset)       (ldb (byte 8 24) blob-size))
    (setf (aref vec (1+ offset))  (ldb (byte 8 16) blob-size))
    (setf (aref vec (+ 2 offset)) (ldb (byte 8 8)  blob-size))
    (setf (aref vec (+ 3 offset)) (ldb (byte 8 0)  blob-size))
    (setf offset (+ offset 4))
    ;; Mark where the blob data starts
    (let ((blob-data-offset offset))
      (values vec blob-data-offset blob-size))))

(multiple-value-bind (tpl-vec tpl-offset tpl-size)
    (build-blob-message-template *num-valves*)
  (defvar *blob-template* tpl-vec)
  (defvar *blob-data-offset* tpl-offset)
  (defvar *blob-data-size* tpl-size))

(defun copy-floats-to-blob (float-array blob-vec start-index)
  "Copy single-floats from FLOAT-ARRAY into BLOB-VEC starting at
   START-INDEX. Encodes each float as 4 bytes (little-endian on x86,
   but OSC requires big-endian — we swap if needed).
   This is non-consing: it writes directly into the pre-allocated
   byte vector."
  (declare (type (simple-array single-float (*)) float-array)
           (type (simple-array (unsigned-byte 8) (*)) blob-vec)
           (type fixnum start-index)
           (optimize (speed 3) (safety 0)))
  (let ((offset start-index)
        (len (length float-array)))
    (declare (type fixnum offset len))
    (dotimes (i len)
      (let ((bits (sb-kernel:single-float-bits (aref float-array i))))
        (declare (type (signed-byte 32) bits))
        ;; Big-endian encoding for OSC network byte order
        (setf (aref blob-vec offset)       (ldb (byte 8 24) bits))
        (setf (aref blob-vec (1+ offset))  (ldb (byte 8 16) bits))
        (setf (aref blob-vec (+ 2 offset)) (ldb (byte 8 8)  bits))
        (setf (aref blob-vec (+ 3 offset)) (ldb (byte 8 0)  bits))
        (incf offset 4)))
    blob-vec))

(defun valve-update-tick (time)
  "Real-time tick: copy valve pressures into OSC blob and send."
  ;; 1. Your wind model updates *valve-pressures* here
  ;; (update-wind-model *valve-pressures*)

  ;; 2. Copy float array into the blob region of the pre-built message
  (copy-floats-to-blob *valve-pressures*
                       *blob-template*
                       *blob-data-offset*)

  ;; 3. Load the complete message bytes into the OSC stream
  (osc:octets-to-buffer *blob-template* *osc-out*)

  ;; 4. Send as a bundle (with time tag) or as a plain message
  (osc:send-bundle *osc-out* time)
  ;; Alternatively: (osc:send *oscout*)  for immediate send without bundle

  ;; Schedule next tick (1 ms = 48 samples at 48 kHz)
  (incudine:aat (+ time (* 0.001 (inc:get-sample-rate))) #'valve-update-tick it))

(incudine.util:rt-eval () (valve-update-tick (incudine:now)))


(incudine:flush-pending)



;;; OSC prototyping
;; TODO Migrate to a separate, dedicated OSC source file and its own package


(defparameter *osc-out* (osc:open :port 5000 :direction :output :protocol :udp))


(defun send-osc (valve-id pressure-value)
  (osc:message *osc-out* (format nil "/exp/v~d/" valve-id) "i" pressure-value))
