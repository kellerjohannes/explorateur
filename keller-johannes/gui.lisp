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
    (let* ((total-seconds (/ (inc:get-current-sample) (if (zerop (inc:get-sample-rate))
                                                          1.0
                                                          (inc:get-sample-rate))))
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



(defun on-arciorgano-keyboard (obj)
  (let ((window (create-gui-window obj :width 1500 :height 800 :title "Arciorgano")))
    (create-arciorgano-keyboard (content window))))





(defstruct tag-filter
  white-list
  black-list)

(defun tag-active-p (tag selection)
  (member tag (tag-filter-white-list selection)))

(defun parameter-active-p (key selection)
  (intersection (mp:get-tag-list key) (tag-filter-white-list selection)))

(defun add-tag-to-white-list (tag selection)
  (unless (tag-active-p tag selection)
    (push tag (tag-filter-white-list selection))))

(defun remove-tag-from-white-list (tag selection)
  (setf (tag-filter-white-list selection) (remove tag (tag-filter-white-list selection))))

;; TODO Implement black list for tags


(defun make-value-field (mp-key)
  (format nil "~a~@[ [~a]~]"
          (mp:getmp mp-key)
          (mp:get-range-string mp-key)))

(defun make-tags-field (key)
  (format nil "~{:~a~^, ~}" (mp:get-tag-list key)))


(defun make-parameter-table-line (obj key selection)
  (format t "~&parameter active: ~a" (parameter-active-p key selection))
  (when (or (null (tag-filter-white-list selection))
            (parameter-active-p key selection))
    (let* ((line (create-div obj :style (format nil "width:100%;padding:2px;display:flex;border-bottom:solid black 1px;")))
           (key-field (create-span line
                                   :content (string-downcase (format nil "~a" key))
                                   :style "width:150px;padding:2px"))
           (value-field (create-span line
                                     :content (make-value-field key)
                                     :style "width:100px;padding:2px"))
           (tags-field (create-span line
                                    :content (make-tags-field key)
                                    :style "width:250px;padding:2px;font-size:small;font-family:monospace;")))
      (declare (ignore key-field tags-field))
      (mp:add-gui-hook key (lambda (new-content)
                             (declare (ignore new-content))
                             (setf (text value-field) (make-value-field key)))))))

(defun create-tag-control-bar (container selection table-container)
  (dolist (tag (mp:get-all-defined-tags))
    (let ((tag-control (create-div container
                                   :content (format nil ":~a" tag)
                                   :style "font-family:monospace;font-size:smaller;border:solid black 1px;margin:2px;padding:2px;cursor:pointer;"
                                   )))
      (setf (background-color tag-control) (if (tag-active-p tag selection) "green" "transparent"))
      (set-on-click tag-control
                    (lambda (obj)
                      (declare (ignore obj))
                      (cond ((tag-active-p tag selection)
                             (remove-tag-from-white-list tag selection)
                             (setf (background-color tag-control) "transparent"))
                            (t (add-tag-to-white-list tag selection)
                               (setf (background-color tag-control) "green")))
                      (create-metaparameter-table table-container selection))))))

(defun create-metaparameter-table (container selection)
  (clog::destroy-children container)
  (maphash (lambda (key parameter)
             (declare (ignore parameter))
             (make-parameter-table-line container key selection))
           (mp:metaparameter-table)))

(defun on-metaparameters-list (obj)
  (let* ((selection (make-tag-filter :white-list nil :black-list nil))
         (window (create-gui-window obj :title "Metaparameters"))
         (tag-control-container (create-div (content window) :style "display:flex;flex-direction:row;gap:3px;width:100%;height:25px;"))
         (table (create-div (content window) :style "display:flex;flex-direction:column;")))
    (create-tag-control-bar tag-control-container selection table)
    (create-metaparameter-table table selection)
    (mp:add-global-gui-hook (lambda (key) (make-parameter-table-line table key selection)))))

(defun create-menu (body)
  (let* ((menu-bar (create-gui-menu-bar body))
         (system-menu (create-gui-menu-drop-down menu-bar :content "System"))
         (tmp (create-gui-menu-item system-menu
                                    :content "Incudine Monitor"
                                    :on-click 'on-incudine-monitor))
         (tmp (create-gui-menu-item system-menu
                                    :content "REPL"
                                    :on-click 'on-repl))
         (keyboard-menu (create-gui-menu-drop-down menu-bar :content "Keyboards"))
         (tmp (create-gui-menu-item keyboard-menu
                                    :content "Arciorgano"
                                    :on-click 'on-arciorgano-keyboard))
         (parameters-menu (create-gui-menu-drop-down menu-bar :content "Parameters"))
         (tmp (create-gui-menu-item parameters-menu
                                    :content "Snapshot management"
                                    :on-click 'on-snapshot-management))
         (tmp (create-gui-menu-item parameters-menu
                                    :content "Metaparameter list"
                                    :on-click 'on-metaparameters-list))
         (tmp (create-gui-menu-item parameters-menu
                                    :content "Pipe list"
                                    :on-click 'on-pipe-list))
         (tmp (create-gui-menu-item parameters-menu
                                    :content "Valve list"
                                    :on-click 'on-valve-list)))
    (declare (ignore tmp))))

(defun on-new-browser (body)
  (setf (title (html-document body)) "Explorateur Control Center")
  (clog-gui-initialize body)
  (enable-clog-popup)
  (add-class body "w3-cyan")
  (create-menu body))


(defparameter *initializedp* nil)


(defun init (&key (host "0.0.0.0") (port *clog-port*) (start-browser t) clogframe)
  (initialize 'on-new-browser :host host :port port)
  (setf *initializedp* t)
  (when clogframe
    (uiop:run-program (list "./clogframe"
                            "Explorateur Standalone"
                            (format nil "~A" *clog-port*)
                            "1280" "960")))
  (when start-browser
    (open-browser)))

;; (defun start ()
;;   (initialize #'on-new-browser)
;;   (setf *initializedp* t)
;;   ;;(open-browser)
;;   )
