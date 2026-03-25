(defpackage :explorateur
  (:use :cl)
  (:nicknames :explo)
  (:export
   :start-incudine-real-time-thread
   :stop-incudine-real-time-thread
   :init
   ))


(defpackage :explorateur.ui
  (:use :cl :clog :clog-gui)
  (:nicknames :ui)
  (:export
   :start
   :init
   :update-real-time-status))


(defpackage :explorateur.incudine
  (:use :cl :incudine)
  (:nicknames :inc)
  (:export
   :start-real-time
   :stop-real-time
   :real-time-status
   :get-current-sample
   :get-sample-rate))

(defpackage :explorateur.metaparameters
  (:use :cl)
  (:nicknames :mp)
  (:export
   :add-global-gui-hook
   :content
   :get-tag-list
   :get-all-defined-tags
   :number-of-metaparameters
   :metaparameter-table
   :add-gui-hook
   :get-range-string
   :defmp
   :getmp
   :setmp
   :incmp
   ))
