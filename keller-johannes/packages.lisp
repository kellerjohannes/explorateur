(defpackage :explorateur
  (:use :cl)
  (:nicknames :explo)
  (:export
   :start-incudine-real-time-thread
   :stop-incudine-real-time-thread
   ))


(defpackage :explorateur.ui
  (:use :cl :clog :clog-gui)
  (:nicknames :ui)
  (:export
   :start
   :update-real-time-status))


(defpackage :explorateur.inc
  (:use :cl :incudine)
  (:nicknames :inc)
  (:export
   :start-real-time
   :stop-real-time
   :real-time-status
   :get-current-sample
   :get-sample-rate))
