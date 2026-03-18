(in-package :explorateur)

(defparameter *default-number-of-valves-per-module* 1024)

(defclass module ()
  ((name :initform "" :initarg :name :accessor name)
   (spatial-id :initform 0 :initarg :spatial-id :accessor spatial-id
             :documentation "An ID (integer) representing the localization of the module in space. This can or should be changed whenever the physical arrangement of the modules is changed, therefore it's not a stable ID.")))

(defparameter *modules-table* (make-hash-table))

(defun add-module (id name spatial-id)
  (setf (gethash id *modules-table*) (make-instance 'module :name name :spatial-id spatial-id)))




(defclass rank ()
  ((name :initform "" :initarg :name :accessor name)
   (description :initform "" :initarg :description :accessor description
                :documentation "Description of the rank, provenience of the pipes, stylistic context.")))

(defparameter *ranks* (make-hash-table))

(defun add-rank (id name description)
  (setf (gethash id *ranks*) (make-instance 'rank
                                            :name name
                                            :description description)))




(defclass valve ()
  ((pressure :initform 0 :accessor pressure
             :documentation "Current pressure the valve is supposed to provide. 1 for standard pressure, 2 for 200% pressure, 0 for no pressure.")
   (module :initform nil :initarg :module :accessor module
           :documentation "Hash key for the module the valve is located in.")
   (id-in-module :initform nil :initarg :id-in-module :accessor id-in-module
                 :documentation "Number of the valve within the hosting module.")
   (mounted-pipe :initform nil :initarg :mounted-pipe :accessor mounted-pipe
                 :documentation "Hash key of the pipe that is mounted on this valve.")
   (midi-out-stream :initform nil :initarg :midi-out-stream :accessor midi-out-stream
                    :documentation "ID (integer) of the MIDI out stream (MIDI hub).")
   (midi-channel :initform nil :initarg :midi-channel :accessor midi-channel
                 :documentation "MIDI channel when using standard MIDI cables and MIDI hubs.")
   (midi-pitch :initform nil :initarg :midi-pitch :accessor midi-pitch
               :documentation "MIDI pitch when using standard MIDI cabels and MIDI hubs.")
   (o-midi-channel :initform nil :initarg :o-midi-channel :accessor o-midi-channel
                   :documentation "MIDI channel when using optical MIDI.")
   (o-midi-pitch :initform nil :initarg :o-midi-pitch :accessor o-midi-pitch
                 :documentation "MIDI pitch when using optical MIDI.")
   (osc-path :initform nil :initarg :osc-path :accessor osc-path
             :documentation "OSC path to this specific valve.")))


(defmethod update-pressure ((valve valve) pressure)
  (setf (pressure valve) pressure)
  ;; send out midi
  ;; send out OSC
  )



(defparameter *valves* (make-hash-table))

(defun add-valve (id module id-in-module pipe-id midi-device-id midi-ch midi-pitch
                  omidi-ch omidi-pitch osc)
  (setf (gethash id *valves*) (make-instance 'valve
                                             :module module
                                             :id-in-module id-in-module
                                             :mounted-pipe pipe-id
                                             :midi-device midi-device-id
                                             :midi-channel midi-ch
                                             :midi-pitch midi-pitch
                                             :o-midi-channel omidi-ch
                                             :o-midi-pitch omidi-pitch
                                             :osc-path osc)))





(defclass pipe ()
  ((rank-id :initform nil :initarg :rank-id :accessor rank-id
            :documentation "The ID (integer) of the rank the pipe is part of.")
   (index-in-rank :initform nil :initarg :index-in-rank :accessor index-in-rank
                  :documentation "1-based index (integer) of the pipe within the enumerated pipes of the entire rank.")
   (classification :initform "" :initarg :classification :accessor classification
                   :documentation "String describing the type of pipe, for example 'principale di legno'.")
   (total-length :initform 0 :initarg :total-length :accessor total-length
                 :documentation "Total length of the pipe in millimeters.")
   (note-name :initform "" :initarg :note-name :accessor note-name
              :documentation "Note name (string).")
   (octave :initform nil :initarg :octave :accessor octave
           :documentation "Octave indicator (integer), 0 for lowest Cello-octave.")
   (theoretical-frequency :initform nil :initarg :theoretical-frequency
                          :accessor theoretical-frequency
                          :documentation "Frequency of the pipe based on the model of the intended tuning system.")
   (detuning :initform 1/1 :initarg :detuning :accessor detuning
             :documentation "Interval ratio describing how much the actual frequency of the pipe differs from its theoretical frequency.")))


(defparameter *pipes* (make-hash-table))

(defun add-pipe (id rank-id index-in-rank classification total-length note-name octave freq detune)
  (setf (gethash id *pipes*) (make-instance 'pipe
                                            :rank-id rank-id
                                            :index-in-rank index-in-rank
                                            :classification classification
                                            :total-length total-length
                                            :note-name note-name
                                            :octave octave
                                            :theoretical-frequency freq
                                            :detuning detune)))


(defun setup-explorateur ()
  (add-module 0 "Bourdun" 4)
  (add-module 1 "Flutes" 5)
  (add-module 2 "Strings" 3)
  (add-module 3 "Aliquotes" 6)
  (add-module 4 "Woodwinds" 2)
  (add-module 5 "Brass" 1)

  (add-rank 0 "Bourdun" "Wooden, stopped 8'.")
  (add-rank 1 "Principal" "Not available yet.")
  (add-rank 2 "Flute Orchestrale" "")
  (add-rank 3 "Flute Harmonique" "")
  (add-rank 4 "Salicional" "")
  (add-rank 5 "Gamba" "")
  (add-rank 6 "Tierce" "")
  (add-rank 7 "Traverse" "")
  (add-rank 8 "Trompette" "")


  (add-pipe 0 0 1 "Bourdun" (/ 1200 (expt 2 0/12)) "C" 0 (/ 440 (expt 2 33/12)) 1/1)
  (add-pipe 1 0 2 "Bourdun" (/ 1200 (expt 2 1/12)) "C♯" 0 (/ 440 (expt 2 32/12)) 1/1)
  (add-pipe 2 0 3 "Bourdun" (/ 1200 (expt 2 2/12)) "D" 0 (/ 440 (expt 2 31/12)) 1/1)
  (add-pipe 3 0 4 "Bourdun" (/ 1200 (expt 2 3/12)) "E♭" 0 (/ 440 (expt 2 30/12)) 1/1)
  (add-pipe 4 0 5 "Bourdun" (/ 1200 (expt 2 4/12)) "E" 0 (/ 440 (expt 2 29/12)) 1/1)


  (add-valve 0 0 1 0 2 6 36 1 36 "/exp/bour/0/")
  (add-valve 1 0 2 1 2 6 37 1 37 "/exp/bour/1/")


  )
