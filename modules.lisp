(in-package :explorateur)

(defparameter *default-number-of-valves-per-module* 1024)

(defclass module ()
  ((name :initform "" :initarg :name :accessor name)
   (order-id :initform 0 :initarg :order-id :accessor order-id)))


(defclass valve ()
  ((pressure :initform 0 :accessor pressure)
   (module :initform nil :initarg :module :accessor module)
   (id-in-module :initform nil :initarg :id-in-module :accessor id-in-module)
   (note-name :initform "" :initarg :note-name :accessor note-name)
   (octave :initform nil :initarg :octave :accessor octave)
   (midi-out-stream :initform nil :initarg :midi-out-stream :accessor midi-out-stream)
   (midi-channel :initform nil :initarg :midi-channel :accessor midi-channel)
   (midi-pitch :initform nil :initarg :midi-pitch :accessor midi-pitch)
   (osc-path :initform nil :initarg :osc-path :accessor osc-path)))


(defmethod update-pressure ((valve valve) pressure)
  (setf (pressure valve) pressure)
  ;; send out midi
  ;; send out OSC
  )

(defparameter *bourdun* (make-instance 'module :name "Bourdun" :order-id 5))

(defparameter *valves* (make-hash-table))

(defun add-valve (id module id-in-module note-name octave)
  (setf (gethash id *valves*) (make-instance 'valve
                                             :module module
                                             :id-in-module id-in-module
                                             :note-name note-name
                                             :octave octave)))


(defun setup-explorateur ()
  (add-valve 0 *bourdun* 36 "C" 0)
  (add-valve 1 *bourdun* 37 "C♯" 0)
  (add-valve 2 *bourdun* 38 "D" 0)
  (add-valve 3 *bourdun* 39 "E♭" 0)
  (add-valve 4 *bourdun* 40 "E" 0)
  (add-valve 5 *bourdun* 41 "F" 0)
  (add-valve 6 *bourdun* 42 "F♯" 0)
  (add-valve 7 *bourdun* 43 "G" 0)
  (add-valve 8 *bourdun* 44 "G♯" 0)
  (add-valve 9 *bourdun* 45 "A" 0)
  (add-valve 10 *bourdun* 46 "B♭" 0)
  (add-valve 11 *bourdun* 47 "B♮" 0)

  (add-valve 12 *bourdun* 48 "C" 1)
  (add-valve 13 *bourdun* 49 "C♯" 1)
  (add-valve 14 *bourdun* 50 "D" 1)
  (add-valve 15 *bourdun* 51 "E♭" 1)
  (add-valve 16 *bourdun* 52 "E" 1)
  (add-valve 17 *bourdun* 53 "F" 1)
  (add-valve 18 *bourdun* 54 "F♯" 1)
  (add-valve 19 *bourdun* 55 "G" 1)
  (add-valve 20 *bourdun* 56 "G♯" 1)
  (add-valve 21 *bourdun* 57 "A" 1)
  (add-valve 22 *bourdun* 58 "B♭" 1)
  (add-valve 23 *bourdun* 59 "B♮" 1)
  )
