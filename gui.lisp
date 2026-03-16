(in-package :explorateur.ui)

(defparameter *incudine-real-time-button* nil)

(defun update-real-time-status (status)
  (when *initializedp*
    (case status
      (:started (setf (style *incudine-real-time-button* "background-color") "green"))
      (:stopped (setf (style *incudine-real-time-button* "background-color") "red")))))

(defun on-incudine-monitor (obj)
  (let* ((window (create-gui-window obj
                                    :title "Incudine System Status"
                                    :width 300
                                    :height 120))
         (rt-div (create-div window :style "margin:30px 5px 5px 5px"))
         (rt-button (create-button rt-div :content "activate real time processing")))
    (setf *incudine-real-time-button* rt-button)))

(defun on-new-browser (body)
  (setf (title (html-document body)) "Explorateur Control Center")
  (clog-gui-initialize body)
  (enable-clog-popup)
  (add-class body "w3-cyan")
  (let* ((menu-bar (create-gui-menu-bar body))
         (system-menu (create-gui-menu-drop-down menu-bar :content "System"))
         (tmp (create-gui-menu-item system-menu
                                    :content "Incudine Monitor"
                                    :on-click 'on-incudine-monitor)))
    (declare (ignore tmp)))
  )


(defparameter *initializedp* nil)

(defun start ()
  (initialize #'on-new-browser)
  (setf *initializedp* t)
  (open-browser))
