(in-package :explorateur)

(defun start-incudine-real-time-thread ()
  (inc:start-real-time)
  (ui:update-real-time-status))

(defun stop-incudine-real-time-thread ()
  (inc:stop-real-time)
  (ui:update-real-time-status))
