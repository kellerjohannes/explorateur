(asdf:defsystem "explorateur"
  :depends-on (:uiop :alexandria :incudine :drawer :clog :cl-svg :usocket)
  :serial t
  :components (
               (:file "clog-svg")
               (:file "packages")
               (:file "midi-communication")
               (:file "udp-communication")
               (:file "metaparameters")
               ;;(:file "scratch")
               ;;(:file "ui")
               (:file "incudine-interaction")
               (:file "modules")
               (:file "gui")
               (:file "keyboard")
               (:file "gui-keyboards")
               (:file "user-interaction-layer")
               (:file "configuration")
               ))
