(in-package :explorateur)

(defparameter *faderfox-in* nil)
(defparameter *osc-out* nil)

(defparameter *explo-outs* (make-array 6 :initial-element nil))

(defun explo-out (id)
  (aref *explo-outs* id))

(defun init ()
  (incudine:remove-all-responders)
  (when *faderfox-in* (jackmidi:close *faderfox-in*))
  (setf *faderfox-in* (jackmidi:open :port-name "Faderfox in"))
  (when *osc-out* (osc:close *osc-out*))
  (setf *osc-out* (osc:open :direction :output :host "192.168.1.10" :port 5800))
  (dotimes (id 6)
    (when (explo-out id) (jackmidi:close (explo-out id)))
    (setf (aref *explo-outs* id) (jackmidi:open :direction :output
                                                :port-name (format nil "Explo out ~d" id))))
  (incudine:rt-start)
  (incudine:recv-start *faderfox-in*)
  (incudine:make-responder *faderfox-in* #'universal-faderfox-responder))

(defconstant +note-off-offset+ 127)
(defconstant +note-on-offset+ 143)
(defconstant +aftertouch-offset+ 159)


(defparameter *explo-modules* '((1 4 48 91 "Trompette" 5 4)
                                (2 7 48 102 "Traverse" 4 7)
                                (3 11 48 103 "Salicional" 3 11)
                                (4 0 48 103 "Gamba" 3 12)
                                (5 1 36 102 "Bourdun" 0 1)
                                (6 3 48 91 "Flute Orchestre" 1 3)
                                (7 2 92 120 "Flute Harmonique" 1 2)
                                (8 9 59 119 "Tierce" 2 9))
  "Module id, midi channel explorateur, lowest note, highest note, name, midi-output-stream, channel")

(defun get-module-metadata () *explo-modules*)
(defun number-of-modules () (length *explo-modules*))

(defun number-of-pipes ()
  (let ((result 0))
    (dolist (module *explo-modules* result)
      (incf result (- (fourth module) (third module))))))

(defun lowest-note-in-module (module-id)
  (third (find module-id *explo-modules* :key #'first)))

(defun highest-note-in-module (module-id)
  (fourth (find module-id *explo-modules* :key #'first)))

(defun module-name (module-id)
  (fifth (find module-id *explo-modules* :key #'first)))

(defun module-id-to-channel (module-id)
  (second (find module-id *explo-modules* :key #'first)))

(defun get-rank-output (module-id)
  (sixth (find module-id *explo-modules* :key #'first)))

(defun get-rank-channel (module-id)
  (seventh (find module-id *explo-modules* :key #'first)))

(defun send-osc (status d1 d2)
  (osc:message *osc-out* "/ex" "iii" status d1 d2))

(defun send-midi (output-id status d1 d2)
  (jackmidi:write-short (explo-out output-id) (jackmidi:message status d1 d2) 3))

(defun send-data (module-id action &optional (d1 0) (d2 0))
  "ACTION can be :NOTE-ON, :NOTE-OFF, :AFTERTOUCH."
  ;;  (format t "~&OSC out: ~d ~d ~d~%" type d1 d2)
  (case action
    (:all-notes-off (progn ;; TODO OSC equivalent
                      (send-midi (get-rank-output module-id)
                                 120 0 0)
                      (send-midi (get-rank-output module-id)
                                 121 0 0)))
    (:note-on (progn ;; (send-osc (+ (module-id-to-channel module-id) +note-on-offset+) d1 d2)
                     (send-midi (get-rank-output module-id)
                                (+ (get-rank-channel module-id) +note-on-offset+)
                                d1 d2)))
    (:note-off (progn ;; (send-osc (+ (module-id-to-channel module-id) +note-on-offset+) d1 d2)
                      (send-midi (get-rank-output module-id)
                                 (+ (get-rank-channel module-id) +note-on-offset+)
                                 d1 d2)))
    (:aftertouch (progn ;; (send-osc (+ (module-id-to-channel module-id) +aftertouch-offset+) d1 d2)
                   (send-midi (get-rank-output module-id)
                              (+ (get-rank-channel module-id) +aftertouch-offset+)
                              d1 d2)
                   ))))

(defun midi-reset ()
  (dotimes (module (length *explo-modules*))
    (send-data module :all-notes-off)))

(defun note-on (note &optional (module-id 4))
  ;; (send-data module-id :note-on note 64)
  (send-data module-id :aftertouch note 64)
  (pressure module-id note 64))

(defun note-off (note &optional (module-id 5))
  ;; (send-data module-id :note-off note 0)
  (send-data module-id :aftertouch note 0)
  (pressure module-id note 0))

(defun aftertouch (note value &optional (module-id 5))
  ;;(format t "~&Aftertouch module ~d, note ~d, value ~d." module-id note value)
  (send-data module-id :aftertouch note value))

(defun set-aftertouch (note value &optional (module-id 5))
  (send-data module-id :aftertouch note value))

(defun play-note (note duration-in-s &optional (module-id 5))
  (note-on note module-id)
  (incudine:at (+ (incudine:now) (* duration-in-s (incudine:rt-sample-rate)))
               #'note-off note module-id))


(defun cluster (lowest-note highest-note duration-in-s &optional (module-id 5))
  (loop for note from lowest-note to highest-note
        do (play-note note duration-in-s module-id)))

(defparameter *running* t)

(defun stop () (setf *running* nil))

(defun start () (setf *running* t))


(defun stuttercluster (lowest-note highest-note duration-in-s &optional (module-id 5))
  (when *running*
    (cluster lowest-note highest-note (* duration-in-s 0.5) module-id)
    (incudine:at (+ (incudine:now) (* duration-in-s (incudine:rt-sample-rate)))
                 #'stuttercluster lowest-note highest-note duration-in-s module-id)))


(defparameter *faderfox-callbacks* (make-array '(256 128)))

(defun valid-faderfox-index-p (type d1)
  (and (<= 0 type (1+ (array-dimension *faderfox-callbacks* 0)))
       (<= 0 d1 (1+ (array-dimension *faderfox-callbacks* 1)))))

(defun get-faderfox-callback-fun (type d1)
  (when (valid-faderfox-index-p type d1)
    (let ((result (aref *faderfox-callbacks* type d1)))
      (if (and result (functionp result))
          (aref *faderfox-callbacks* type d1)
          (lambda (type d1 d2) (declare (ignore type d1 d2)))))))

(defun set-faderfox-callback-fun (type d1 fun)
  "FUN needs to have 3 parameters, TYPE D1 and D2."
  (when (valid-faderfox-index-p type d1)
    (setf (aref *faderfox-callbacks* type d1) fun)))

(defun universal-faderfox-responder (type d1 d2)
  (format t "~&Faderfox in: ~d ~d ~d~%" type d1 d2)
  (funcall (get-faderfox-callback-fun type d1) type d1 d2))


;;; Setting things up

(init)

(defparameter *stutter-times* (make-array 8 :initial-element 1))

(defun update-stutter-time (module delta)
  (incf (aref *stutter-times* (1- module)) delta))

(defparameter *stutter-durations* (make-array 8 :initial-element 0.5))

(defun update-stutter-duration (module delta)
  (incf (aref *stutter-durations* (1- module)) delta))

(defparameter *stutter-states* (make-array 8 :initial-element nil))

(defun toggle-stutter-state (module)
  (if (aref *stutter-states* (1- module))
      (setf (aref *stutter-states* (1- module)) nil)
      (setf (aref *stutter-states* (1- module)) t)))

(defparameter *stutter-ranges* (make-array '(8 2) :initial-contents '((0 127) (0 127) (0 127)
                                                                      (0 127) (0 127) (0 127)
                                                                      (0 127) (0 127))))
(defun set-stutter-range-low (module val)
  (setf (aref *stutter-ranges* (1- module) 0) val))

(defun set-stutter-range-high (module val)
  (setf (aref *stutter-ranges* (1- module) 1) val))

(defun stutter-loop (module)
  (when *running*
    (when (aref *stutter-states* (1- module))
      (cluster (aref *stutter-ranges* (1- module) 0)
               (aref *stutter-ranges* (1- module) 1)
               (aref *stutter-durations* (1- module))
               module))
    (incudine:at (+ (incudine:now) (* (aref *stutter-times* (1- module)) (incudine:rt-sample-rate)))
                 #'stutter-loop module)))

(defun stutter-start ()
  (dotimes (module 8)
    (stutter-loop (1+ module))))






(progn
  (set-faderfox-callback-fun 149 16 (lambda (ty d1 d2) (declare (ignore ty d1 d2))
                                      (toggle-stutter-state 1)))
  (set-faderfox-callback-fun 149 18 (lambda (ty d1 d2) (declare (ignore ty d1 d2))
                                      (toggle-stutter-state 2)))
  (set-faderfox-callback-fun 149 20 (lambda (ty d1 d2) (declare (ignore ty d1 d2))
                                      (toggle-stutter-state 3)))
  (set-faderfox-callback-fun 149 22 (lambda (ty d1 d2) (declare (ignore ty d1 d2))
                                      (toggle-stutter-state 4)))
  (set-faderfox-callback-fun 149 24 (lambda (ty d1 d2) (declare (ignore ty d1 d2))
                                      (toggle-stutter-state 5)))
  (set-faderfox-callback-fun 149 26 (lambda (ty d1 d2) (declare (ignore ty d1 d2))
                                      (toggle-stutter-state 6)))
  (set-faderfox-callback-fun 149 28 (lambda (ty d1 d2) (declare (ignore ty d1 d2))
                                      (toggle-stutter-state 7)))

  (set-faderfox-callback-fun 181 16 (lambda (ty d1 d2) (declare (ignore ty d1))
                                      (set-stutter-range-low 1 d2)))
  (set-faderfox-callback-fun 181 17 (lambda (ty d1 d2) (declare (ignore ty d1))
                                      (set-stutter-range-high 1 d2)))

  (set-faderfox-callback-fun 181 18 (lambda (ty d1 d2) (declare (ignore ty d1))
                                      (set-stutter-range-low 2 d2)))
  (set-faderfox-callback-fun 181 19 (lambda (ty d1 d2) (declare (ignore ty d1))
                                      (set-stutter-range-high 2 d2)))

  (set-faderfox-callback-fun 181 20 (lambda (ty d1 d2) (declare (ignore ty d1))
                                      (set-stutter-range-low 3 d2)))
  (set-faderfox-callback-fun 181 21 (lambda (ty d1 d2) (declare (ignore ty d1))
                                      (set-stutter-range-high 3 d2)))

  (set-faderfox-callback-fun 181 22 (lambda (ty d1 d2) (declare (ignore ty d1))
                                      (set-stutter-range-low 4 d2)))
  (set-faderfox-callback-fun 181 23 (lambda (ty d1 d2) (declare (ignore ty d1))
                                      (set-stutter-range-high 4 d2)))

  (set-faderfox-callback-fun 181 24 (lambda (ty d1 d2) (declare (ignore ty d1))
                                      (set-stutter-range-low 5 d2)))
  (set-faderfox-callback-fun 181 25 (lambda (ty d1 d2) (declare (ignore ty d1))
                                      (set-stutter-range-high 5 d2)))

  (set-faderfox-callback-fun 181 26 (lambda (ty d1 d2) (declare (ignore ty d1))
                                      (set-stutter-range-low 6 d2)))
  (set-faderfox-callback-fun 181 27 (lambda (ty d1 d2) (declare (ignore ty d1))
                                      (set-stutter-range-high 6 d2)))

  (set-faderfox-callback-fun 181 28 (lambda (ty d1 d2) (declare (ignore ty d1))
                                      (set-stutter-range-low 7 d2)))
  (set-faderfox-callback-fun 181 29 (lambda (ty d1 d2) (declare (ignore ty d1))
                                      (set-stutter-range-high 7 d2)))



  (set-faderfox-callback-fun 149 48 (lambda (ty d1 d2) (declare (ignore ty d1 d2))
                                      (toggle-stutter-state 1)))
  (set-faderfox-callback-fun 149 50 (lambda (ty d1 d2) (declare (ignore ty d1 d2))
                                      (toggle-stutter-state 2)))
  (set-faderfox-callback-fun 149 52 (lambda (ty d1 d2) (declare (ignore ty d1 d2))
                                      (toggle-stutter-state 3)))
  (set-faderfox-callback-fun 149 54 (lambda (ty d1 d2) (declare (ignore ty d1 d2))
                                      (toggle-stutter-state 4)))
  (set-faderfox-callback-fun 149 56 (lambda (ty d1 d2) (declare (ignore ty d1 d2))
                                      (toggle-stutter-state 5)))
  (set-faderfox-callback-fun 149 58 (lambda (ty d1 d2) (declare (ignore ty d1 d2))
                                      (toggle-stutter-state 6)))
  (set-faderfox-callback-fun 149 60 (lambda (ty d1 d2) (declare (ignore ty d1 d2))
                                      (toggle-stutter-state 7)))

  (set-faderfox-callback-fun 181 48 (lambda (ty d1 d2) (declare (ignore ty d1))
                                      (update-stutter-time 1 (* 0.01 (- d2 64)))))
  (set-faderfox-callback-fun 181 49 (lambda (ty d1 d2) (declare (ignore ty d1))
                                      (update-stutter-duration 1 (* 0.01 (- d2 64)))))

  (set-faderfox-callback-fun 181 50 (lambda (ty d1 d2) (declare (ignore ty d1))
                                      (update-stutter-time 2 (* 0.01 (- d2 64)))))
  (set-faderfox-callback-fun 181 51 (lambda (ty d1 d2) (declare (ignore ty d1))
                                      (update-stutter-duration 2 (* 0.01 (- d2 64)))))

  (set-faderfox-callback-fun 181 52 (lambda (ty d1 d2) (declare (ignore ty d1))
                                      (update-stutter-time 3 (* 0.01 (- d2 64)))))
  (set-faderfox-callback-fun 181 53 (lambda (ty d1 d2) (declare (ignore ty d1))
                                      (update-stutter-duration 3 (* 0.01 (- d2 64)))))

  (set-faderfox-callback-fun 181 54 (lambda (ty d1 d2) (declare (ignore ty d1))
                                      (update-stutter-time 4 (* 0.01 (- d2 64)))))
  (set-faderfox-callback-fun 181 55 (lambda (ty d1 d2) (declare (ignore ty d1))
                                      (update-stutter-duration 4 (* 0.01 (- d2 64)))))

  (set-faderfox-callback-fun 181 56 (lambda (ty d1 d2) (declare (ignore ty d1))
                                      (update-stutter-time 5 (* 0.01 (- d2 64)))))
  (set-faderfox-callback-fun 181 57 (lambda (ty d1 d2) (declare (ignore ty d1))
                                      (update-stutter-duration 5 (* 0.01 (- d2 64)))))

  (set-faderfox-callback-fun 181 58 (lambda (ty d1 d2) (declare (ignore ty d1))
                                      (update-stutter-time 6 (* 0.01 (- d2 64)))))
  (set-faderfox-callback-fun 181 59 (lambda (ty d1 d2) (declare (ignore ty d1))
                                      (update-stutter-duration 6 (* 0.01 (- d2 64)))))

  (set-faderfox-callback-fun 181 58 (lambda (ty d1 d2) (declare (ignore ty d1))
                                      (update-stutter-time 7 (* 0.01 (- d2 64)))))
  (set-faderfox-callback-fun 181 59 (lambda (ty d1 d2) (declare (ignore ty d1))
                                      (update-stutter-duration 7 (* 0.01 (- d2 64)))))

  (set-faderfox-callback-fun 181 64 (lambda (ty d1 d2) (declare (ignore ty d1))
                                      (select-root-module d2)))
  (set-faderfox-callback-fun 181 65 (lambda (ty d1 d2) (declare (ignore ty d1))
                                      (select-octave-aftertouch d2)))


  (set-faderfox-callback-fun 181 68 (lambda (ty d1 d2) (declare (ignore ty d1))
                                      (pressure *root-module-aftertouch*
                                                (+ 0 (* 12 *root-octave-aftertouch*))
                                                d2)))
  (set-faderfox-callback-fun 181 69 (lambda (ty d1 d2) (declare (ignore ty d1))
                                      (pressure *root-module-aftertouch*
                                                (+ 1 (* 12 *root-octave-aftertouch*))
                                                d2)))
  (set-faderfox-callback-fun 181 70 (lambda (ty d1 d2) (declare (ignore ty d1))
                                      (pressure *root-module-aftertouch*
                                                (+ 2 (* 12 *root-octave-aftertouch*))
                                                d2)))
  (set-faderfox-callback-fun 181 71 (lambda (ty d1 d2) (declare (ignore ty d1))
                                      (pressure *root-module-aftertouch*
                                                (+ 3 (* 12 *root-octave-aftertouch*))
                                                d2)))
  (set-faderfox-callback-fun 181 72 (lambda (ty d1 d2) (declare (ignore ty d1))
                                      (pressure *root-module-aftertouch*
                                                (+ 4 (* 12 *root-octave-aftertouch*))
                                                d2)))
  (set-faderfox-callback-fun 181 73 (lambda (ty d1 d2) (declare (ignore ty d1))
                                      (pressure *root-module-aftertouch*
                                                (+ 5 (* 12 *root-octave-aftertouch*))
                                                d2)))
  (set-faderfox-callback-fun 181 74 (lambda (ty d1 d2) (declare (ignore ty d1))
                                      (pressure *root-module-aftertouch*
                                                (+ 6 (* 12 *root-octave-aftertouch*))
                                                d2)))
  (set-faderfox-callback-fun 181 75 (lambda (ty d1 d2) (declare (ignore ty d1))
                                      (pressure *root-module-aftertouch*
                                                (+ 7 (* 12 *root-octave-aftertouch*))
                                                d2)))
  (set-faderfox-callback-fun 181 76 (lambda (ty d1 d2) (declare (ignore ty d1))
                                      (pressure *root-module-aftertouch*
                                                (+ 8 (* 12 *root-octave-aftertouch*))
                                                d2)))
  (set-faderfox-callback-fun 181 77 (lambda (ty d1 d2) (declare (ignore ty d1))
                                      (pressure *root-module-aftertouch*
                                                (+ 9 (* 12 *root-octave-aftertouch*))
                                                d2)))
  (set-faderfox-callback-fun 181 78 (lambda (ty d1 d2) (declare (ignore ty d1))
                                      (pressure *root-module-aftertouch*
                                                (+ 10 (* 12 *root-octave-aftertouch*))
                                                d2)))
  (set-faderfox-callback-fun 181 79 (lambda (ty d1 d2) (declare (ignore ty d1))
                                      (pressure *root-module-aftertouch*
                                                (+ 11 (* 12 *root-octave-aftertouch*))
                                                d2)))
  )

(defun dummy ()
  (when *running*
    (format t "~&[override safety]")
    (dotimes (module (length *explo-modules*))
      (aftertouch 0 (random 5) (1+ module)))
    (incudine:at (+ (incudine:now) (* 2 44100)) #'dummy)
    ))

(defparameter *root-octave-aftertouch* 0)

(defun select-octave-aftertouch (oct)
  (when (< -1 oct 7)
    (setf *root-octave-aftertouch* oct)))

(defparameter *root-module-aftertouch* 1)

(defun select-root-module (id)
  (when (< 0 id 8)
    (setf *root-module-aftertouch* id)))


(defparameter *pressure-states* (make-array '(8 128) :initial-element 0))

(defun reset-all-pressures ()
  (dotimes (m 8)
    (dotimes (n 128)
      (setf (aref *pressure-states* m n) 0)
      (set-aftertouch n 0 (1+ m))
      (sleep 0.001))))

(defun update-pressure (module note delta)
  (aftertouch note (incf (aref *pressure-states* (1- module) note) delta) module))

(defun pressure (module note pressure)
  ;; (format t "~&Setting pressure module ~d, note ~d to ~d." module note pressure)
  (aftertouch note (setf (aref *pressure-states* (1- module) note) pressure) module)
  (ui:set-pressure module note pressure))

(defun get-pressure (module note)
  (aref *pressure-states* (1- module) note))

(defun take-pressure-snapshot ()
  (let ((result (make-array '(8 128) :initial-element 0)))
    (dotimes (m 8)
      (dotimes (n 128)
        (setf (aref result m n) (aref *pressure-states* m n))))
    result))

(defun recall-pressure-snapshot (snapshot)
  (dotimes (m 8)
    (dotimes (n 128)
      (pressure (1+ m) n (aref snapshot m n)))))




;;;; Drunken breather


(defun searcher (module note low-end high-end delta duration-in-s)
  (when *running*
    (let ((new-pressure (+ delta (get-pressure module note))))
      (cond ((> new-pressure high-end)
             (pressure module note high-end)
             (searcher module note low-end high-end (- delta) duration-in-s))
            ((< new-pressure low-end)
             (pressure module note low-end)
             (searcher module note low-end high-end (- delta) duration-in-s))
            (t (pressure module note new-pressure)
               (incudine:at (+ (incudine:now) (* duration-in-s (incudine:rt-sample-rate)))
                            #'searcher module note low-end high-end delta duration-in-s))))))

(defun breathing (&optional (module 2) (low 48) (high 58) (max-pressure 5) (time-factor 0.01))
  (loop for note from low to high
        do (progn
             (pressure module note (random max-pressure))
             (searcher module note 0 max-pressure 1 (* time-factor (random 100))))))

;; Emacs stuff:
;; (setf slynk:*use-dedicated-output-stream* nil)

(defun hades-breath ()
  (setf *trace-output* (open "incudine-trace-output.log" :direction :output :if-exists :append))
  (with-open-file (*standard-output* "incudine-log.txt"
                                     :direction :output
                                     :if-exists :append
                                     :if-does-not-exist :create)
    (incudine:at (+ (incudine:now) (* 1 (incudine:rt-sample-rate)))
                 #'breathing 2 48 80 7)
    (incudine:at (+ (incudine:now) (* 2 (incudine:rt-sample-rate)))
                 #'breathing 3 48 70 15)
    (incudine:at (+ (incudine:now) (* 3 (incudine:rt-sample-rate)))
                 #'breathing 5 36 90 8 0.03)
    (incudine:at (+ (incudine:now) (* 4 (incudine:rt-sample-rate)))
                 #'breathing 8 60 95 7 0.05)))

(defun hades-1 ()
  (breathing 2 48 80 7)
  )

(defun hades-2 ()
  (breathing 3 48 70 15 0.02)
  )

(defun hades-3 ()
  (breathing 5 36 90 8 0.03)
  )

(defun hades-4 ()
  (breathing 8 60 95 7 0.05)
  )

(defun killall ()
  (stop)
  (reset-all-pressures)
  (incudine:flush-pending)
  (dotimes (module 8) (setf (aref *stutter-states* module) nil))
  (incudine:at (+ (incudine:now) (incudine:rt-sample-rate)) #'start))



;;;; Clouds of Randomness


(defparameter *droplet-duration* 0.1 "In seconds.")
(defparameter *droplet-max-spacing* 1 "In seconds.")

(defun droplet-loop (module note)
  (when *running*
    (let ((next-time (+ (incudine:now)
                        (random (* *droplet-max-spacing* (incudine:rt-sample-rate))))))
      (incudine:at next-time #'play-note note *droplet-duration* module)
      (incudine:at (+ next-time (* *droplet-duration* (incudine:rt-sample-rate)))
                   #'droplet-loop module note))))

(defun cloud (module)
  (loop for note from (lowest-note-in-module module) to (highest-note-in-module module)
        do (droplet-loop module note)))





;;;; Microtonality

(defparameter *pitch-tables*
  '(("Salicional"
     (60
      (0 nil)
      (10 412)
      (20 415)
      (30 420)
      (40 428)
      (50 435)
      (60 438)
      (70 442)
      (80 448)
      (90 453)
      (100 469)
      (110 475)
      (120 480)
      (127 488))
     (61
      (0 nil)
      (10 450)
      (20 455)
      (30 458)
      (40 461)
      (50 463)
      (60 466)
      (70 470)
      (80 475)
      (90 480)
      (100 485)
      (110 490)
      (120 492)
      (128 498))))
  "Lookup tables.")


(defun write-pitch-tables-to-file (filename)
  (with-open-file (out (merge-pathnames (format nil "pitchtable-data/~a" filename)
                                        (asdf/system:system-source-directory :explorateur))
                       :direction :output
                       :if-does-not-exist :create
                       :if-exists :supersede)
    (write *pitch-tables* :stream out)))


(defun read-pitch-table-from-file (filename)
  (with-open-file (in (merge-pathnames (format nil "pitchtable-data/~a" filename)
                                       (asdf/system:system-source-directory :explorateur))
                      :direction :input)
    (setf *pitch-tables* (read in))))

(defun get-rank-data (rank-name pitch-table)
  (rest (find rank-name pitch-table :key #'first :test #'string-equal)))

(defun (setf get-rank-data) (new-rank-data rank-name pitch-table)
  (setf (rest (find rank-name pitch-table :key #'first :test #'string-equal))
        new-rank-data))

(defun get-pipe-data (pipe-number rank-data)
  (rest (find pipe-number rank-data :key #'first :test #'=)))

(defun (setf get-pipe-data) (new-pipe-data pipe-number rank-data)
  (setf (rest (find pipe-number rank-data :key #'first :test #'=)) new-pipe-data))

(defun get-pipe-data-of-rank (pipe-number rank-name pitch-table)
  (get-pipe-data pipe-number (get-rank-data rank-name pitch-table)))

(defun (setf get-pipe-data-of-rank) (new-pipe-data pipe-number rank-name pitch-table)
  (setf (get-pipe-data pipe-number (get-rank-data rank-name pitch-table)) new-pipe-data))

(defun get-aftertouch-data (aftertouch-value pipe-data)
  (rest (find aftertouch-value pipe-data :key #'first :test #'=)))

(defun (setf get-aftertouch-data) (new-aftertouch-data aftertouch-value pipe-data)
  (setf (rest (find aftertouch-value pipe-data :key #'first :test #'=)) new-aftertouch-data))

(defun get-aftertouch-data-of-pipe-of-rank (aftertouch-value pipe-number rank-name pitch-table)
  (get-aftertouch-data aftertouch-value
                       (get-pipe-data-of-rank pipe-number rank-name pitch-table)))

(defun (setf get-aftertouch-data-of-pipe-of-rank)
    (new-aftertouch-data aftertouch-value pipe-number rank-name pitch-table)
  (setf (get-aftertouch-data aftertouch-value
                             (get-pipe-data-of-rank pipe-number rank-name pitch-table))
        new-aftertouch-data))

(defun get-frequency-of-pipe-of-rank (aftertouch-value pipe-number rank-name pitch-table)
  (second (get-aftertouch-data aftertouch-value
                               (get-pipe-data-of-rank pipe-number rank-name pitch-table))))

(defun (setf get-frequency-of-pipe-of-rank)
    (new-frequency aftertouch-value pipe-number rank-name pitch-table)
  (setf (second (get-aftertouch-data aftertouch-value
                                     (get-pipe-data-of-rank pipe-number rank-name pitch-table)))
        new-frequency))

(defun add-frequency (aftertouch-value pipe-number rank-name pitch-table)
  (let ((existing-frequency (get-frequency-of-pipe-of-rank aftertouch-value
                                                           pipe-number
                                                           rank-name
                                                           pitch-table)))
    (if existing-frequency
        (setf (get-aftertouch-data-of-pipe-of-rank aftertouch-value
                                                   pipe-number
                                                   rank-name
                                                   pitch-table)
              "hi"))))



;;; REDO with structs

(defstruct pitch
  aftertouch
  frequency)

(defstruct pipe
  midi-pitch
  pitch-list)

(defstruct rank
  name-string
  pipe-list)

(defstruct pitch-table
  date-string
  temperature
  description
  rank-list)

(defparameter *pitch-table* (make-pitch-table :rank-list nil
                                              :temperature 21.3
                                              :date-string "2026-03-02"
                                              :description "Dummy data for testing purposes"))

(defun get-rank-by-name (rank-name pitch-table)
  (let ((result (find rank-name
                      (pitch-table-rank-list pitch-table) :key #'rank-name-string
                      :test #'string-equal)))
    (cond (result result)
          (t (push (make-rank :name-string rank-name :pipe-list nil)
                   (pitch-table-rank-list pitch-table))
             (get-rank-by-name rank-name pitch-table)))))

(defun get-pipe (pipe-number rank)
  (let ((result (find pipe-number (rank-pipe-list rank) :key #'pipe-midi-pitch :test #'=)))
    (cond (result result)
          (t (push (make-pipe :midi-pitch pipe-number :pitch-list nil)
                   (rank-pipe-list rank))
             (get-pipe pipe-number rank)))))

(defun get-pipe-in-pitch-table (pipe-number rank-name pitch-table)
  (get-pipe pipe-number (get-rank-by-name rank-name pitch-table)))

(defun get-pitch-data (aftertouch-value pipe &optional new-frequency)
  (let ((result (find aftertouch-value (pipe-pitch-list pipe) :key #'pitch-aftertouch
                                                              :test #'=)))
    (cond ((and result new-frequency) (setf (pitch-frequency result) new-frequency))
          (result result)
          (t (push (make-pitch :aftertouch aftertouch-value :frequency nil)
                   (pipe-pitch-list pipe))
             (get-pitch-data aftertouch-value pipe new-frequency)))))

(defun get-pitch-data-in-pitch-table (aftertouch-value pipe-number rank-name pitch-table
                                      &optional new-frequency)
  (get-pitch-data aftertouch-value
                  (get-pipe-in-pitch-table pipe-number rank-name pitch-table)
                  new-frequency))

(defun get-frequency (aftertouch-value pipe-number rank-name pitch-table)
  (pitch-frequency (get-pitch-data-in-pitch-table aftertouch-value
                                                  pipe-number
                                                  rank-name
                                                  pitch-table)))

(defun set-frequency (frequency aftertouch-value pipe-number rank-name pitch-table)
  (get-pitch-data-in-pitch-table aftertouch-value pipe-number rank-name pitch-table frequency))

(defun write-pitch-tables-to-file (filename pitch-table)
  (with-open-file (out (merge-pathnames (format nil "pitchtable-data/~a" filename)
                                        (asdf/system:system-source-directory :explorateur))
                       :direction :output
                       :if-does-not-exist :create
                       :if-exists :supersede)
    (write pitch-table :stream out)))

(defun read-pitch-table-from-file (filename)
  (with-open-file (in (merge-pathnames (format nil "pitchtable-data/~a" filename)
                                       (asdf/system:system-source-directory :explorateur))
                      :direction :input)
    (read in)))


(defun enter-frequency (pipe-number rank-name pitch-table)
  (format t "~&Entering data for pipe ~d in rank ~a ..." pipe-number rank-name)
  (loop for aftertouch from 0 to 128 by 1 do

    (format t "~&Applying aftertouch value ~d. Enter Frequency [number or NIL for noise]: "
            aftertouch)
    (aftertouch pipe-number aftertouch 1)
    (set-frequency (read) aftertouch pipe-number rank-name pitch-table)))

(defun enter-pipe (rank-name pitch-table &optional pipe-number)
  (unless pipe-number (setf pipe-number (progn (format t "~&First pipe [number]: ") (read))))
  (enter-frequency pipe-number rank-name pitch-table)
  (when (yes-or-no-p "Enter next pipe?")
    (enter-pipe rank-name pitch-table (1+ pipe-number))))

(defun enter-rank (pitch-table)
  (let* ((name (progn (format t "~&Rank name [string]: ") (read))))
    (enter-pipe name pitch-table)
    (when (yes-or-no-p "Enter another rank?")
      (enter-rank pitch-table))))

(defun enter-data ()
  (let* ((description (progn (format t "~&Description of pitch table [string]: ") (read)))
         (date (progn (format t "~&Timestamp [string]: ") (read)))
         (temperature (progn (format t "~&Temperature [number]: ") (read)))
         (current-pitch-table (make-pitch-table :date-string date
                                                :temperature temperature
                                                :description description
                                                :rank-list nil)))
    (enter-rank current-pitch-table)
    current-pitch-table))

(defun measure (filename)
  (write-pitch-tables-to-file filename (enter-data)))




;;;; JI experiment

(defun chord-1 ()
  ;; (aftertouch 60 93 5)
  (aftertouch 60 80 5)
  (aftertouch 64 64 5)
  (aftertouch 67 89 5)
  (aftertouch 70 45 5)
  (aftertouch 48 94 5)
  (aftertouch 74 86 2)
  (aftertouch 76 64 2)
  (aftertouch 78 39 2)
  (aftertouch 79 44 2))



;;;; Recording every pipe in every pressure

(vug:define-vug recorder ()
  (when (and *recording* (zerop *in-time*))
    (setf *in-time* (incudine:now)))
  (when *recording*
    (cond ((< (- (incudine:now) *in-time*) *recording-length*)
           (setf *recording* nil)
           (setf *in-time* 0))
          (t (setf (incudine:buffer-value *buffer*
                                          (- (incudine:now) *in-time*)
                                          (incudine:audio-in)))))))


(defparameter *clip-duration* (* 3 48000))

(defparameter *buffer* (incudine:make-buffer *clip-duration*))

(vug:define-vug simple-record (frame)
  (declare (type fixnum frame))
  (setf (incudine:buffer-value *buffer* frame) (incudine:audio-in 0))
  (incf frame))

;; (vug:dsp! rec () (simple-record (coerce 0 'fixnum)))

(defun trigger-recording (filename)
  (rec :id 1)
  (incudine:at (+ (incudine:now) *clip-duration*) #'incudine:free 1)
  (incudine:at (+ (incudine:now) *clip-duration*) #'incudine:buffer-save *buffer* filename))



(defun sample-module (module-id &optional (pipe (lowest-note-in-module module-id)) (aftertouch 0))
  (when *running*
    (cond ((> pipe (highest-note-in-module module-id))
           (format t "~&DONE"))
          ((> aftertouch 127)
           (aftertouch pipe 0 module-id)
           (sample-module module-id (1+ pipe) 0))
          (t (aftertouch pipe aftertouch module-id)
             (format t "~&Playing module '~a', pipe ~d, aftertouch ~d."
                     (module-name module-id)
                     pipe
                     aftertouch)
             (trigger-recording (format nil "~a-note~d-at~d.wav"
                                        (module-name module-id)
                                        pipe
                                        aftertouch))
             (let ((next-time (+ (incudine:now) (* 1.1 *clip-duration*))))
               (incudine:at next-time #'sample-module module-id pipe (+ aftertouch 8)))))))

;;; Offline analysis

(defun find-peak (data-list)
  (let ((peak 0)
        (peak-freq 0))
    (loop for (mag freq) on data-list by #'cddr
          do (when (> mag peak)
               (setf peak mag)
               (setf peak-freq freq)))
    (values peak-freq peak)))


(defun find-all-peaks (module-id)
  (loop for pipe from (lowest-note-in-module module-id) to (highest-note-in-module module-id)
        collect
        (cons pipe
              (loop for aftertouch from 0 to 127 by 8
                    collect
                    (cons aftertouch
                          (let* ((ana-data (ana:stft (format nil "~a-note~d-at~d.wav"
                                                             (module-name module-id)
                                                             pipe
                                                             aftertouch)
                                                     65536))
                                 (data (progn
                                         (setf (ana:pvbuffer-data-type ana-data) :magnitude-frequency)
                                         (reverse
                                          (rest
                                           (reverse
                                            (loop for i from 0 to 65536
                                                  collect (cffi:mem-aref (ana:pvbuffer-data ana-data 0)
                                                                         :double i))))))))
                            (multiple-value-bind (peak peak-mag)
                                (find-peak data)
                              (format t "~&Pipe ~d, aftertouch ~d: peak frequency ~d, peak magnitude: ~d."
                                      pipe
                                      aftertouch
                                      peak
                                      peak-mag)
                              (if (< peak-mag 100)
                                  0.0
                                  peak))))))))


;; (let* ((bt (drawer:make-backend-tikz :filename "spectrum.tex"))
;;        (data (reverse (rest (reverse (loop for i from 0 to 65536 collect (cffi:mem-aref (ana:pvbuffer-data *ana* 100) :double i))))))
;;        (dots (drawer:gr (do ((i 0 (1+ i))
;;                              (result nil)
;;                              (rdata data))
;;                             ((<= (length rdata) 2) result)
;;                           (push (drawer:ln (drawer:pt (/ (second rdata) 240) (/ (first rdata) 10))
;;                                            (drawer:pt (/ (fourth rdata) 240) (/ (third rdata) 10)))
;;                                 result)
;;                           (setf rdata (rest (rest rdata)))))))
;;   (drawer:draw-with-multiple-backends (list bt) (list dots))
;;   (drawer:compile-tikz bt))


(defun random-static ()
  (loop for module from 1 to (number-of-modules)
        do (loop for pipe from (lowest-note-in-module module)
                   to (highest-note-in-module module)
                 do (pressure module pipe (random 128)))))




(defparameter *pitches* nil ;; (find-all-peaks 5)
  )


(defun write-pitches-to-file (filename)
  (with-open-file (out (merge-pathnames (format nil "pitchtable-data/~a" filename)
                                        (asdf/system:system-source-directory :explorateur))
                       :direction :output
                       :if-does-not-exist :create
                       :if-exists :supersede)
    (write *pitches* :stream out)))


(defun read-pitches-from-file (filename)
  (with-open-file (in (merge-pathnames (format nil "pitchtable-data/~a" filename)
                                       (asdf/system:system-source-directory :explorateur))
                      :direction :input)
    (setf *pitches* (read in))))

(defun find-pressure (target-frequency pipe-data &optional (last-candidate nil))
  (cond ((null pipe-data) nil)
        ((and (consp last-candidate)
              (plusp (cdr last-candidate))
              (< (cdr last-candidate) target-frequency (cdr (first pipe-data)))
              (< (/ (cdr (first pipe-data)) (cdr last-candidate)) 9/8))
         (list last-candidate (first pipe-data)))
        (t (find-pressure target-frequency (rest pipe-data) (first pipe-data)))))

(defun find-pipe (target-frequency module-data &optional (results nil))
  (cond ((null module-data) results)
        (t (let ((pressures (find-pressure target-frequency
                                           (first (rest module-data))
                                           (first (first module-data)))))
             (find-pipe target-frequency
                        (rest module-data)
                        (if pressures
                            (append results (cons (first (first module-data)) pressures))
                            results))))))

(defun interpolate-pressure (p1 p2 frequency)
  (let ((frame-interval (/ (cdr p2) (cdr p1)))
        (target-interval (/ frequency (cdr p1)))
        (pressure-delta (- (car p2) (car p1))))
    (+ (car p1) (* pressure-delta (/ (log target-interval) (log frame-interval))))))

(defparameter *old-pipe* 0)

(defun play-frequency (frequency)
  (let ((pipe-data (find-pipe frequency *pitches*)))
    (if pipe-data
        (progn
          (unless (= (first pipe-data) *old-pipe*)
            (pressure 5 *old-pipe* 0))
          (pressure 5
                    (first pipe-data)
                    (floor (interpolate-pressure (second pipe-data) (third pipe-data) frequency)))
          (setf *old-pipe* (first pipe-data)))
        (format t "~&Frequency ~d is not available in the Explorateur." frequency))))

(defparameter *root-frequency* 800)

(defun chord (root-ratio &rest interval-list)
  (tacet)
  (dolist (interval interval-list)
    (play-frequency (* *root-frequency* root-ratio interval))))

(defun tacet ()
  (loop for note from (lowest-note-in-module 5) to (highest-note-in-module 5)
        do (pressure 5 note 0)))
;; (chord 1/1 3/2 2/1 5/2 7/2 4/1)



(defun gliss (freq)
  (when *running*
    (cond ((> freq 3000) nil)
          (t (play-frequency freq)
             (incudine:at (+ (incudine:now) (* 0.3 4410)) #'gliss (* freq (/ 121 120.0)))))))
