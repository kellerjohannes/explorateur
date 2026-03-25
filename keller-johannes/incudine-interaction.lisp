(in-package :explorateur.incudine)

(defun start-real-time ()
  (rt-start))

(defun stop-real-time ()
  (rt-stop))

(defun real-time-status ()
  (rt-status))

(defun get-current-sample ()
  (now))

(defun get-sample-rate ()
  (rt-sample-rate))
