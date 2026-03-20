(asdf:defsystem "explorateur"
  :depends-on (:uiop :alexandria :incudine :drawer :clog :cl-svg)
  :serial t
  :components ((:file "packages")
               ;;(:file "scratch")
               ;;(:file "ui")
               (:file "incudine-interaction")
               (:file "modules")
               (:file "gui")
               (:file "user-interaction-layer")
               ))
