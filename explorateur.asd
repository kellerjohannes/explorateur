(asdf:defsystem "explorateur"
  :depends-on (:uiop :alexandria :incudine :drawer :clog)
  :serial t
  :components ((:file "packages")
               (:file "scratch")
               (:file "ui")))
