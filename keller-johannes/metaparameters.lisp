(in-package :explorateur.metaparameters)

(defparameter *metaparameters* (make-hash-table))

(defparameter *parameter-add-gui-hooks* nil)

(defun add-global-gui-hook (fun)
  (push fun *parameter-add-gui-hooks*))

(defun number-of-metaparameters ()
  (length *metaparameters*))

(defun metaparameter-table ()
  *metaparameters*)

(defclass metaparameter ()
  ((content :initform nil :initarg :content :accessor content)
   (range-minimum :initform nil :initarg :range-minimum :accessor range-minimum)
   (range-maximum :initform nil :initarg :range-maximum :accessor range-maximum)
   (gui-hooks :initform nil :accessor gui-hooks)
   (processing-hooks :initform nil :accessor processing-hooks)))

(defun register-metaparameter (key content &key range-max range-min)
  (setf (gethash key *metaparameters*)
        (make-instance 'metaparameter
                       :content content
                       :range-maximum range-max
                       :range-minimum range-min))
  (dolist (hook *parameter-add-gui-hooks*)
    (funcall hook key))
  content)

(defun add-gui-hook (key fun)
  (let ((instance (gethash key *metaparameters*)))
    (if (and instance (functionp fun))
        (push fun (gui-hooks (gethash key *metaparameters*)))
        (format t "~&Error: no metaparameter with key ~a found." key))))

(defun get-range-string (key)
  (let ((instance (gethash key *metaparameters*)))
    (if instance
        (cond ((and (range-minimum instance) (range-maximum instance))
               (format nil "~a-~a" (range-minimum instance) (range-maximum instance)))
              ((range-minimum instance)
               (format nil "~a-∞" (range-minimum instance)))
              ((range-maximum instance)
               (format nil "∞-~a" (range-maximum instance)))
              (t nil))
        nil)))

(defun defmp (key content &optional (minimum nil) (maximum nil))
  (register-metaparameter key content :range-min minimum :range-max maximum))

(defun getmp (key)
  (let ((parameter-instance (gethash key *metaparameters*)))
    (if parameter-instance
        (content parameter-instance)
        (progn
          (format t "~&No metaparameter found with key ~a." key)
          nil))))

(defun setmp (key new-content)
  (let ((old-parameter-instance (gethash key *metaparameters*)))
    (if old-parameter-instance
        (let ((minimum (range-minimum old-parameter-instance))
              (maximum (range-maximum old-parameter-instance)))
          (if (or (and (numberp new-content)
                       (or (not (numberp minimum))
                           (and (numberp minimum) (>= new-content minimum)))
                       (or (not (numberp maximum))
                           (and (numberp maximum) (<= new-content maximum))))
                  (not (numberp new-content)))
              (progn
                (dolist (hook (processing-hooks old-parameter-instance))
                  (funcall hook old-parameter-instance new-content))
                (setf (content (gethash key *metaparameters*)) new-content)
                (dolist (hook (gui-hooks old-parameter-instance))
                  (funcall hook new-content)))
              (format t "~&Error: Value ~a of Metaparameter with key ~a is out of range ~a-~a."
                      new-content key minimum maximum)))
        (register-metaparameter key new-content))))

(defun incmp (key &optional (delta 1))
  (let ((old-content (getmp key)))
    (when old-content
      (if (numberp old-content)
        (setmp key (+ old-content delta))
        (format t "~&Error: INCMP can't be applied to the non-numeric value of metaparameter ~a with content ~a."
                key old-content)))
    (getmp key)))
