(in-package :explorateur)

(defun rt ()
  (start-incudine-real-time-thread))

(defun rt-stop ()
  (stop-incudine-real-time-thread))
