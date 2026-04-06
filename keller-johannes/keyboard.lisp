(in-package :explorateur.keyboard)

(defconstant +max-number-of-octaves+ 8)
(defconstant +number-of-whites-per-octave+ 7)
(defconstant +max-number-of-ordini+ 16)

(defclass key ()
  ((state :initform :off :initarg :state :accessor state)
   (gui-callbacks :initform nil :accessor gui-callbacks)
   (process-callbacks :initform nil :accessor process-callbacks)))

(defmethod update-state ((key key) state)
  (setf (state key) state)
  (dolist (callback (process-callbacks key))
    (funcall callback state))
  (dolist (callback (gui-callbacks key))
    (funcall callback state)))

(defmethod add-gui-callback ((key key) fun)
  (push fun (gui-callbacks key)))

(defmethod add-process-callback ((key key) fun)
  (push fun (process-callbacks key)))




(defclass keyboard ()
  ((keys :initform nil :accessor keys)
   (name :initform "" :initarg :name :accessor name)
   (description :initform "" :initarg :description :accessor description)
   (layout :initform nil :initarg :layout :accessor layout)))

(defmethod initialize-instance :after ((kbd keyboard) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (setf (keys kbd)
        (make-array (list (* +max-number-of-octaves+ +number-of-whites-per-octave+ 2)
                          +max-number-of-ordini+)
                    :initial-element nil)))

(defmethod set-key-state ((kbd keyboard) axis ordine state)
  ;; TODO implement range checking
  (if (aref (keys kbd) axis ordine)
      (update-state (aref (keys kbd) axis ordine) state)
      (setf (aref (keys kbd) axis ordine)
            (make-instance 'key :state state))))


(defmethod get-name ((kbd keyboard)) (name kbd))
(defmethod get-description ((kbd keyboard)) (description kbd))
