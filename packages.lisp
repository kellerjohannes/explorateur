(defpackage :explorateur
  (:use :cl)
  (:nicknames :explo)
  (:export
   :get-module-metadata
   :number-of-modules
   :lowest-note-in-module
   :highest-note-in-module
   :module-name))


(defpackage :explorateur.ui
  (:use :cl :clog :clog-gui)
  (:nicknames :ui)
  (:export
   :start-ui
   :set-pressure))
