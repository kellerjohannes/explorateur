(in-package :explorateur.ui)


(defun on-help-about (obj)
  (let* ((about (create-gui-window obj
                                   :title   "About"
                                   :content "<div><p><center>Explorateur Command Center</center>
                                             <center>Studio31/Johannes Keller, March 2026</center></p></div>"
                                   :hidden  t
                                   :width   320
                                   :height  110)))
    (window-center about)
    (setf (visiblep about) t)
    (set-on-window-can-size about (lambda (obj)
                                    (declare (ignore obj))()))))


(defstruct valve
  pressure
  callback-list)

(defparameter *valves* (make-array (list (explo:number-of-modules) 128)))

(defun init-valves ()
  (dotimes (module (explo:number-of-modules))
    (dotimes (pipe 127)
      (setf (aref *valves* module pipe) (make-valve :pressure 0 :callback-list nil)))))

(init-valves)

(defun get-pressure (module-id pipe)
  (valve-pressure (aref *valves* (1- module-id) pipe)))

(defun call-callbacks (module-id pipe)
  ;; (format t "~&Calling callbacks for module ~d, pipe ~d." module-id pipe)
  (dolist (callback (valve-callback-list (aref *valves* (1- module-id) pipe)))
    (funcall callback (get-pressure module-id pipe))))

(defun add-valve-callback (module-id pipe fun)
  ;; (format t "~&Adding callback for module ~d, pipe ~d." module-id pipe)
  (push fun (valve-callback-list (aref *valves* (1- module-id) pipe)))
  (call-callbacks module-id pipe))

(defun set-pressure (module-id pipe value)
  ;; (format t "~&Setting pressure for module ~d, pipe ~d to ~d." module-id pipe value)
  (setf (valve-pressure (aref *valves* (1- module-id) pipe)) value)
  (call-callbacks module-id pipe))

(defun on-valve-click (valve-obj)
  (format t "~&Valve clicked")
  (setf (text valve-obj) "clicked"))

(defun register-valve (obj module-id pipe)
  (let* ((valve (create-div obj :content (format nil "~d:~d" pipe 0) :style "font-size:small;height:80px;border:solid black 2px;border-radius:5px;background-color:white;background-image:linear-gradient(to top, brown 0%, transparent 0% 100%);"))
         ;; (dummy (create-div obj :content "hi"))
         )
    ;; (set-on-click dummy 'on-valve-click)
    (add-valve-callback module-id
                        pipe
                        (lambda (pressure)
                          (let ((percentage (* 100.0 (/ pressure 128.0))))
                            (setf (text valve) (format nil "~d ~d" pipe pressure))
                            (setf (style valve "background-image")
                                  (format nil "linear-gradient(to top, blue ~d%, transparent ~d% 100%)"
                                          percentage
                                          percentage)))))))

(defun generic-monitor-onclick (obj module-id)
  (let* ((window (create-gui-window obj
                                    :title (format nil "Monitor for Module ~d: ~a"
                                                   module-id (explo:module-name module-id))
                                    :width 700
                                    :height (+ (* (1+ (floor (/ (- (explo:highest-note-in-module module-id)
                                                                   (explo:lowest-note-in-module module-id))
                                                                12)))
                                                  90)
                                               60)))
         (valve-container (create-div window :style "padding:30px 10px 10px 10px;display:grid;gap:4px;grid-template-columns:repeat(12,1fr);text-align:center;")))
    (setf (visiblep window) t)
    (loop for note-name in (list "C" "C♯" "D" "E♭" "E" "F" "F♯" "G" "G♯" "A" "B♭" "B♮")
          do (create-div valve-container :content note-name))
    (loop for pipe from (explo:lowest-note-in-module module-id)
            to (explo:highest-note-in-module module-id)
          do (register-valve valve-container module-id pipe))))


(defun fill-monitor-onclicks ()
  (let ((result (make-array (explo:number-of-modules))))
    (dotimes (i (explo:number-of-modules) result)
      (let ((index (1+ i)))
        (setf (aref result i) (lambda (obj) (funcall #'generic-monitor-onclick obj index)))))))

(defun timecode-loop (timecode-div)
  (let* ((total-seconds (/ (incudine:now) (incudine:rt-sample-rate)))
         (total-minutes (floor (/ total-seconds 60.0)))
         (rest-seconds (floor (mod total-seconds 60.0)))
         (total-hours (floor (/ total-minutes 60.0)))
         (rest-minutes (mod total-minutes 60)))
    (setf (text timecode-div) (format nil "NOW ~d samples (~d:~d:~d)"
                                      (floor (incudine:now))
                                      total-hours
                                      rest-minutes
                                      rest-seconds)))
  (sleep 0.1)
  (timecode-loop timecode-div))

(defun on-output-monitor (obj)
  (let* ((window (create-gui-window obj :title "Output Monitor"
                                   :width 600
                                   :height 120
                                   ))
         (timecode (create-div window
                               :style "padding:25px;"
                               :content (format nil "NOW ~d" (incudine:now))))
         (slider (create-meter window :value 30)))
    (timecode-loop timecode)))

(defun on-new-window (body)
  (setf (title (html-document body)) "Explorateur")
  (clog-gui-initialize body)
  (enable-clog-popup)
  (add-class body "w3-cyan")
  (let* ((monitor-onclicks (fill-monitor-onclicks))
         (menu-bar (create-gui-menu-bar body))
         (tmp (create-gui-menu-icon menu-bar :on-click 'on-help-about))
         (monitor-menu (create-gui-menu-drop-down menu-bar :content "Module Monitors"))
         (global-monitors (create-gui-menu-drop-down menu-bar :content "Global Monitors"))
         (tmp (create-gui-menu-item global-monitors :content "Output Monitor"
                                                    :on-click 'on-output-monitor))
         (tmp (create-gui-menu-item global-monitors :content "Input Monitor"))
         (global-menu (create-gui-menu-drop-down menu-bar :content "Global Controls"))
         (tmp (create-gui-menu-item global-menu :content "Resets"))
         (tmp (create-gui-menu-item global-menu :content "Master Wind"))
         )
    (declare (ignore tmp))
    (loop for module-id from 1 to (explo:number-of-modules)
          do (create-gui-menu-item monitor-menu
                                   :content (format nil "~d: ~a" module-id
                                                    (explo:module-name module-id))
                                   :on-click (aref monitor-onclicks (1- module-id))))
    ;; (let* ((win (create-gui-window body))
    ;;        (hi (create-div win :content "hi" :style "padding:35px;background-color:green")))
    ;;   (setf (text hi) "there")
    ;;   (set-on-mouse-down hi (lambda (obj)
    ;;                      (declare (ignore obj))
    ;;                      (format t "~&hi clicked")
    ;;                      (setf (text hi) "clicked"))))
    ))


(defun start-ui ()
  (initialize #'on-new-window)
  (open-browser))
