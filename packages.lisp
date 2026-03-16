(defpackage :explorateur
  (:use :cl)
  (:nicknames :explo)
  (:export
   ))


(defpackage :explorateur.ui
  (:use :cl :clog :clog-gui)
  (:nicknames :ui)
  (:export
   :start
   :update-real-time-status))


(defpackage :explorateur.inc
  (:use :cl :incudine)
  (:nicknames :inc))
