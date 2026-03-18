(in-package :explorateur)

(defun start-incudine-real-time-thread ()
  (explorateur.inc:start-real-time)
  (explorateur.ui:update-real-time-status))

(defun stop-incudine-real-time-thread ()
  (explorateur.inc:stop-real-time)
  (explorateur.ui:update-real-time-status))
