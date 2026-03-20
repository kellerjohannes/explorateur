(in-package :explorateur.ui)


(defun set-all-styles (list-of-instances style-string value-string)
  (dolist (instance list-of-instances)
    (setf (style instance style-string) value-string)))

(defun set-all-texts (list-of-instances text-string)
  (dolist (instance list-of-instances)
    (setf (text instance) text-string)))

(defparameter *incudine-real-time-indicator* nil)

(defun update-real-time-status ()
  (when (and *initializedp* *incudine-real-time-indicator*)
    (case (inc:real-time-status)
      (:started (set-all-styles *incudine-real-time-indicator* "background-color" "green") )
      (:stopped (set-all-styles *incudine-real-time-indicator* "background-color" "red")))))

(defparameter *incudine-clock-sample-display* nil)
(defparameter *incudine-clock-display* nil)

(defparameter *time-interval-incudine-clock* 0.5 "Time between clock updated in seconds.")




(defun incudine-clock-loop ()
  (when (and *initializedp* *incudine-clock-sample-display*)
    (set-all-texts *incudine-clock-sample-display*
                   (format nil "Audio sample: ~d" (floor (inc:get-current-sample))))
    (let* ((total-seconds (/ (inc:get-current-sample) (inc:get-sample-rate)))
           (total-minutes (floor (/ total-seconds 60.0)))
           (rest-seconds (floor (mod total-seconds 60.0)))
           (total-hours (floor (/ total-minutes 60.0)))
           (rest-minutes (mod total-minutes 60)))
      (set-all-texts *incudine-clock-display* (format nil "Uptime: ~d:~2,'0d:~2,'0d"
                                                      total-hours
                                                      rest-minutes
                                                      rest-seconds))))
  (sleep *time-interval-incudine-clock*)
  (incudine-clock-loop))

(defun on-incudine-monitor (obj)
  (let* ((window (create-gui-window obj
                                    :title "Incudine System Status"
                                    :width 300
                                    :height 125))
         (rt-div (create-div (content window) :style "margin:5px;display:flex;flex-direction:row;justify-content:flex-start;align-items:center;gap:3px;"))
         (rt-indicator (create-div rt-div :style "width:15px;height:15px;border:solid black 1px;margin:2px;"))
         (rt-button (create-button rt-div :content "toggle real time processing"))
         (sample-div (create-div (content window)))
         (samples (create-div sample-div :style "width:290px;margin:5px;"))
         (clock-div (create-div (content window)))
         (clock (create-div clock-div :style "width: 290px;margin:5px;")))
    (set-on-click rt-button (lambda (obj)
                              (declare (ignore obj))
                              (case (inc:real-time-status)
                                (:started (explo:stop-incudine-real-time-thread))
                                (:stopped (explo:start-incudine-real-time-thread)))))
    (push rt-indicator *incudine-real-time-indicator*)
    (update-real-time-status)
    (push samples *incudine-clock-sample-display*)
    (push clock *incudine-clock-display*)
    (incudine-clock-loop)
    ))

(defun on-repl (obj)
  (declare (ignore obj))
  ;; TODO Implement a custom REPL
  )


(defun on-svg-test (obj)
  (let ((window (create-gui-window obj)))
    ;; (let* ((svg-data (svg:make-svg-toplevel 'svg:svg-1.1-toplevel :width 120 :height 60))
    ;;       (key (svg:make-svg-symbol svg-data (:id :generate)
    ;;                (svg:draw* (:rect :x 0 :y 0 :width 12 :height 23)))))
    ;;   (svg:draw svg-data (:rect :x 10 :y 10 :width 20 :height 20))
    ;;   (svg:draw svg-data (:circle :cx 60 :cy 30 :r 17 :id "test-circle") :fill "black")
    ;;   (svg:draw svg-data (:use :xlink-href (svg:xlink-href key))
    ;;             :x 80 :y 40 :fill "red")
    ;;   (let ((svg-container (create-div (content window))))
    ;;     (format t "~&SVG DATA: ~a" (svg::svg->string svg-data))
    ;;     (create-child svg-container (svg::svg->string svg-data))))
    (let* ((svg (clog::create-svg-toplevel (content window)))
           (dot (clog::create-svg-circle svg :cx 50 :cy 50 :r 5))
           )
      ;(set-on-click dot (lambda (obj) (declare (ignore obj)) (setf (clog::cx dot) 60)))
      )
    )
  ;;(js-execute obj "document.getElementById('test-circle').setAttribute('fill', 'green');")
  )

;(set-on-click)

;; (let ((s (make-string-output-stream)))
;;   (let ((scene (svg:make-svg-toplevel 'svg:svg-1.2-toplevel :width 120 :height 100)))
;;     (svg:draw scene (:rect :x 10 :y 10 :width 30 :height 30))
;;     (svg:stream-out s scene)
;;     (format t "~a" (get-output-stream-string s))))


(defun create-menu (body)
  (let* ((menu-bar (create-gui-menu-bar body))
         (system-menu (create-gui-menu-drop-down menu-bar :content "System"))
         (tmp (create-gui-menu-item system-menu
                                    :content "Incudine Monitor"
                                    :on-click 'on-incudine-monitor))
         (tmp (create-gui-menu-item system-menu
                                    :content "REPL"
                                    :on-click 'on-repl))
         (tmp (create-gui-menu-item system-menu
                                    :content "SVG test"
                                    :on-click 'on-svg-test)))
    (declare (ignore tmp))))

(defun on-new-browser (body)
  (setf (title (html-document body)) "Explorateur Control Center")
  (clog-gui-initialize body)
  (enable-clog-popup)
  (add-class body "w3-cyan")
  (create-menu body))


(defparameter *initializedp* nil)

(defun start ()
  (initialize #'on-new-browser)
  (setf *initializedp* t)
  (open-browser))
