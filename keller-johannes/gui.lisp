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


(defun create-arciorgano-keyboard (obj)
  (let* ((svg (create-svg-toplevel obj :width 1200 :height 500))
         (scale 2)
         (white-width (* 12 scale))
         (white-front-length (* 16 scale))
         (black-width (* 6 scale))
         (black-front-length (* 9 scale))
         (black-back-length (* 9 scale))
         (tastino-width (* 5 scale))
         (tastino-length (* 6 scale)))
    (flet ((white-right-of-tastino (x y)
             (create-svg-ortho-shape svg
                                     :x-origin x :y-origin y
                                     :fill "transparent" :stroke "black"
                                     :deltas (list white-width
                                                   (- white-front-length)
                                                   (- (* 1/2 black-width))
                                                   (- (+ black-front-length black-back-length))
                                                   (- (- white-width
                                                         (* 1/2 black-width)
                                                         (* 1/2 tastino-width)))
                                                   tastino-length
                                                   (- (* 1/2 tastino-width)))))
           (white-left-of-tastino (x y)
             (create-svg-ortho-shape svg
                                     :x-origin x :y-origin y
                                     :fill "transparent" :stroke "black"
                                     :deltas (list white-width
                                                   (- (- (+ white-front-length
                                                            black-front-length
                                                            black-back-length)
                                                         tastino-length))
                                                   (- (* 1/2 tastino-width))
                                                   (- tastino-length)
                                                   (- (- white-width
                                                         (* 1/2 black-width)
                                                         (* 1/2 tastino-width)))
                                                   (+ black-back-length black-front-length)
                                                   (- (* 1/2 black-width)))))
           (white-between-blacks (x y)
             (create-svg-ortho-shape svg
                                     :x-origin x :y-origin y
                                     :fill "transparent" :stroke "black"
                                     :deltas (list white-width
                                                   (- white-front-length)
                                                   (- (* 1/2 black-width))
                                                   (- (+ black-front-length black-back-length))
                                                   (- (- white-width black-width))
                                                   (+ black-front-length black-back-length)
                                                   (- (* 1/2 black-width)))))
           (black-front (x y)
             (create-svg-ortho-shape svg
                                     :x-origin x :y-origin y
                                     :fill "transparent" :stroke "black"
                                     :deltas (list black-width
                                                   (- black-front-length)
                                                   (- black-width))))
           (black-back (x y)
             (create-svg-ortho-shape svg
                                     :x-origin x :y-origin y
                                     :fill "transparent" :stroke "black"
                                     :deltas (list black-width
                                                   (- black-back-length)
                                                   (- black-width))))
           (tastino (x y)
             (create-svg-ortho-shape svg
                                     :x-origin x :y-origin y
                                     :fill "transparent" :stroke "black"
                                     :deltas (list tastino-width
                                                   (- tastino-length)
                                                   (- tastino-width))))
           )
      (let ((keyboard (make-array 17 :initial-element nil)))
        (setf (aref keyboard 0) (white-right-of-tastino (+ 5 (* 0 white-width)) 150))
        (setf (aref keyboard 1) (white-between-blacks (+ 5 (* 1 white-width)) 150))
        (setf (aref keyboard 2) (white-left-of-tastino (+ 5 (* 2 white-width)) 150))
        (setf (aref keyboard 3) (white-right-of-tastino (+ 5 (* 3 white-width)) 150))
        (setf (aref keyboard 4) (white-between-blacks (+ 5 (* 4 white-width)) 150))
        (setf (aref keyboard 5) (white-between-blacks (+ 5 (* 5 white-width)) 150))
        (setf (aref keyboard 6) (white-left-of-tastino (+ 5 (* 6 white-width)) 150))
        (setf (aref keyboard 7) (black-front (+ 5 (* 3/4 white-width) (* 0 white-width))
                                             (- 150 white-front-length)))
        (setf (aref keyboard 8) (black-front (+ 5 (* 3/4 white-width) (* 1 white-width))
                                             (- 150 white-front-length)))
        (setf (aref keyboard 9) (black-front (+ 5 (* 3/4 white-width) (* 3 white-width))
                                             (- 150 white-front-length)))
        (setf (aref keyboard 10) (black-front (+ 5 (* 3/4 white-width) (* 4 white-width))
                                             (- 150 white-front-length)))
        (setf (aref keyboard 11) (black-front (+ 5 (* 3/4 white-width) (* 5 white-width))
                                             (- 150 white-front-length)))
        (setf (aref keyboard 12) (black-front (+ 5 (* 3/4 white-width) (* 0 white-width))
                                              (- 150 (+ black-front-length white-front-length))))
        (setf (aref keyboard 13) (black-front (+ 5 (* 3/4 white-width) (* 1 white-width))
                                              (- 150 (+ black-front-length white-front-length))))
        (setf (aref keyboard 14) (black-front (+ 5 (* 3/4 white-width) (* 3 white-width))
                                              (- 150 (+ black-front-length white-front-length))))
        (setf (aref keyboard 15) (black-front (+ 5 (* 3/4 white-width) (* 4 white-width))
                                              (- 150 (+ black-front-length white-front-length))))
        (setf (aref keyboard 16) (black-front (+ 5 (* 3/4 white-width) (* 5 white-width))
                                              (- 150 (+ black-front-length white-front-length))))

        (loop for key across keyboard do
          (when key
            (let ((ckey key))
              (set-on-mouse-over ckey
                                 (lambda (obj)
                                   (declare (ignore obj))
                                   (setf (svg-attribute ckey "fill") "blue")))
              (set-on-mouse-out ckey
                                (lambda (obj)
                                  (declare (ignore obj))
                                  (setf (svg-attribute ckey "fill") "transparent"))))))))))

(defun on-svg-test (obj)
  (let ((window (create-gui-window obj)))
    (let* ((svg (create-svg-toplevel (content window)))
           (dot (create-svg-circle svg :cx 50 :cy 50 :r 5 :fill "black"))
           (square (create-svg-rect svg :x 150 :y 120 :width 25 :height 25 :fill "green"))
           (key-shape (create-svg-ortho-shape svg :x-origin 60
                                                  :y-origin 60
                                                  :deltas (list 20 -30 -5 -20 -15)
                                                  :fill "transparent"
                                                  :stroke "black")))
      (set-on-mouse-over key-shape (lambda (obj)
                                     (setf (svg-attribute key-shape "fill") "yellow")))
      (set-on-mouse-out key-shape (lambda (obj)
                                    (setf (svg-attribute key-shape "fill") "white")))
      (set-on-click square (lambda (obj)
                             (declare (ignore obj))
                             (setf (svg-attribute square "width") 45)))
      (set-on-mouse-over dot (lambda (obj)
                               (declare (ignore obj))
                               (setf (svg-attribute dot "fill") "yellow")))
      (set-on-mouse-out dot (lambda (obj)
                              (declare (ignore obj))
                              (setf (svg-attribute dot "fill") "black")))
      (set-on-click dot (lambda (obj)
                          (declare (ignore obj))
                          (setf (svg-attribute dot "fill") "red"))))))


(defun on-arciorgano-keyboard (obj)
  (let ((window (create-gui-window obj :width 1500 :height 800 :title "Arciorgano")))
    (create-arciorgano-keyboard (content window))))

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
                                    :on-click 'on-svg-test))
         (keyboard-menu (create-gui-menu-drop-down menu-bar :content "Keyboards"))
         (tmp (create-gui-menu-item keyboard-menu
                                    :content "Arciorgano"
                                    :on-click 'on-arciorgano-keyboard)))
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
