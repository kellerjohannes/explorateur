(in-package :explorateur)

(defparameter *midi-out* (jackmidi:open :direction :output :port-name "Organteq bridge"))

(defun send-midi (status data1 data2)
  (jackmidi:write-short *midi-out* (jackmidi:message status data1 data2) 3))

(defun play-organteq (midi-pitch &optional (duration-in-s 1))
  (send-midi 144 midi-pitch 90)
  (incudine:at (+ (incudine:now) (* duration-in-s (incudine:rt-sample-rate)))
               #'send-midi 128 midi-pitch 90))

(defparameter *global-atomic-burst-duration* 0.02)

(defun set-burst-duration (duration-in-s)
  (setf *global-atomic-burst-duration* duration-in-s))

(defun get-burst-duration ()
  *global-atomic-burst-duration*)

(defun burst (origin-pitch &optional (total-duration-in-s 5))
  (let ((number-of-atoms 200)
        (range-above-center 5)
        (range-below-center 5))
    (dotimes (i number-of-atoms)
      (incudine:at (+ (incudine:now) (random (* (incudine:rt-sample-rate) total-duration-in-s)))
                   #'play-organteq
                   (+ origin-pitch
                      (- (random (+ range-above-center range-below-center))
                         range-below-center))
                   (get-burst-duration)))))

(defun play-score (filename)
  (funcall (incudine:regofile->function
            (merge-pathnames filename
                             (merge-pathnames "rego-scores/"
                                              (asdf:system-source-directory :explorateur))))))
