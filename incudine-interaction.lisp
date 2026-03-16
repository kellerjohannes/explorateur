(in-package :explorateur.inc)

(defun start-real-time ()
  (rt-start)
  (ui:update-real-time-status (rt-status)))

(defun stop-real-time ()
  (rt-stop)
  (ui:update-real-time-status (rt-status)))
