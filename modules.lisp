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
   (midi-pitch :initform nil :initarg :midi-pitch :accessor midi-pitch)))

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
  (add-valve 3 *bourdun* 39 "E♭" 0))
