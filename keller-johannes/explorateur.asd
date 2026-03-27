(asdf:defsystem "explorateur"
  :depends-on (:uiop :alexandria :incudine :drawer :clog :cl-svg)
  :serial t
  :components (
               (:file "clog-svg")
               (:file "packages")
               (:file "metaparameters")
               ;;(:file "scratch")
               ;;(:file "ui")
               (:file "incudine-interaction")
               (:file "modules")
               (:file "gui")
               (:file "gui-keyboards")
               (:file "user-interaction-layer")
               ))
