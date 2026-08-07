(in-package :explorateur)

(defun setup-exquis ()
  (midi:register-midi-connection :exquis "MIDI Exquis in" :input)
  (midi:clear-dispatch-table :exquis)
  (midi:register-midi-callback :exquis 176 42 (lambda (status data1 data2)
                                                (declare (ignore status data1))
                                                (format t "~&2nd rotary value ~d." data2))))

(defun init-connections ()
  (midi:register-midi-connection :triple-midi-kbd-a "MIDI kbd A" :input)
  (midi:register-midi-connection :triple-midi-kbd-b "MIDI kbd B" :input)
  (midi:register-midi-connection :triple-midi-kbd-c "MIDI kbd C" :input)
  (setup-exquis)
  (midi:register-midi-connection :effect-out "MIDI effects out" :output)
  (format t "~&MIDI setup done.")
  )


(defun main ()
  (ui:init)
  (init-connections))

;; TODO Delete, once the main concepts have been migrated to a clean implementation.
;; (defun init-midi ()
;;   ;; can't happen immediately after opening jackmidi ports.
;;   (flet ((map-midi-kbd (id port-name)
;;            (dotimes (channel 16)
;;              (dotimes (pitch 128)
;;                (let ((copy-channel channel)
;;                      (copy-pitch pitch))
;;                  (midi:register-midi-callback id (+ 144 channel) pitch
;;                                               (lambda (status data1 data2)
;;                                                 (declare (ignore status data1 data2))
;;                                                 (format t "~&Note on from ~a, channel ~a, pitch ~a.~%"
;;                                                         port-name copy-channel copy-pitch)))
;;                  (midi:register-midi-callback id (+ 128 channel) pitch
;;                                               (lambda (status data1 data2)
;;                                                 (declare (ignore status data1 data2))
;;                                                 (format t "~&Note off from ~a, channel ~a, pitch ~a.~%"
;;                                                         port-name copy-channel copy-pitch))))))))
;;     (map-midi-kbd :triple-midi-kbd-a "MIDI kbd A")
;;     (map-midi-kbd :triple-midi-kbd-b "MIDI kbd B")
;;     (map-midi-kbd :triple-midi-kbd-c "MIDI kbd C")
;;     ))
