(in-package :explorateur.ui)

(defparameter *arciorgano-kbd* '((0 . 0)
                                 (1 . 1)
                                 (1 . 2)
                                 (2 . 0)
                                 (3 . 1)
                                 (3 . 2)
                                 (4 . 0)
                                 (5 . 2)
                                 (6 . 0)
                                 (7 . 1)
                                 (7 . 2)
                                 (8 . 0)
                                 (9 . 1)
                                 (9 . 2)
                                 (0 . 3)
                                 (1 . 4)
                                 (1 . 5)
                                 (2 . 3)
                                 (3 . 4)
                                 (3 . 5)
                                 (4 . 3)
                                 (5 . 3)))


(defparameter *total-depth* 65)
(defparameter *white-width* 12)
(defparameter *black-width* 8)

(defparameter *row-depths* (make-array 7 :initial-contents '(10 25 35 40 45 50 55)))


(defstruct key
  ordine
  axis
  whitep
  width
  left-front-leg
  left-leg-offset
  left-back-leg
  back-width
  right-front-leg
  right-leg-offset
  right-back-leg
  )


(defun reverse-keyboard-layout (keyboard-layout)
  (sort (loop for (axis occurrances) on keyboard-layout by #'cddr
              collect (cons axis (sort occurrances #'<)))
        #'<
        :key #'first))

(defun sort-keyboard-layout (keyboard-layout)
  (let ((result nil))
    (dolist (key-cons (sort keyboard-layout #'< :key #'car) (reverse-keyboard-layout result))
      (push (cdr key-cons) (getf result (car key-cons))))))

(defun high-pass-filter (lst limit)
  (remove-if (lambda (row) (<= row limit)) lst))

(defun next-lower-row (black-list previous-key next-key)
  (first (sort (copy-list (append (rest black-list)
                                  (high-pass-filter (rest previous-key) (first black-list))
                                  (high-pass-filter (rest next-key) (first black-list))))
               #'<)))

(defun calculate-black-length (black-list previous-key next-key)
  (let ((upper-end-index (next-lower-row black-list previous-key next-key)))
    (if upper-end-index
        (- (aref *row-depths* upper-end-index)
           (aref *row-depths* (first black-list)))
        (- *total-depth* (aref *row-depths* (first black-list))))))

(defun next-limiting-key (white-list neighbour-key)
  (first (sort (copy-list (append (rest white-list)
                                  (high-pass-filter (rest neighbour-key) (first white-list))))
               #'<)))

(defun calculate-white-front-leg (white-list neighbour-key)
  (let ((upper-end-index (next-limiting-key white-list neighbour-key)))
    (if upper-end-index
        (- (aref *row-depths* upper-end-index)
           (aref *row-depths* (first white-list)))
        (- *total-depth* (aref *row-depths* (first white-list))))))

(defun calculate-white-length (white-list)
  (if (null (rest white-list))
      (- *total-depth* (aref *row-depths* (first white-list)))
      (- (aref *row-depths* (first (rest white-list)))
         (aref *row-depths* (first white-list)))))

(defun parse-keyboard-layout (keyboard-layout)
  (let ((result))
    (labels ((black-loop (rest-black-list previous-key next-key axis)
               (format t "~&Black key loop axis ~a, ordine ~a." axis (first rest-black-list))
               (cond ((null rest-black-list) nil)
                     (t (let ((black-length (calculate-black-length rest-black-list
                                                                    previous-key
                                                                    next-key)))
                          (push (make-key :ordine (first rest-black-list)
                                          :axis axis
                                          :whitep nil
                                          :width *black-width*
                                          :left-front-leg black-length
                                          :right-front-leg black-length
                                          :back-width *black-width*)
                                result)
                          (black-loop (rest rest-black-list) previous-key next-key axis)))))
             (white-loop (rest-white-list previous-key next-key axis)
               (cond ((null rest-white-list) nil)
                     (t (let ((left-front-leg (calculate-white-front-leg rest-white-list
                                                                         previous-key))
                              (right-front-leg (calculate-white-front-leg rest-white-list
                                                                          next-key))
                              (white-length (calculate-white-length rest-white-list)))
                          (push (make-key :ordine (first rest-white-list)
                                          :axis axis
                                          :whitep t
                                          :width *white-width*
                                          :left-front-leg left-front-leg
                                          :right-front-leg right-front-leg
                                          :left-leg-offset (* 1/2 *black-width*)
                                          :right-leg-offset (* 1/2 *black-width*)
                                          :left-back-leg (- white-length left-front-leg)
                                          :right-back-leg (- white-length right-front-leg)
                                          :back-width (- *white-width* *black-width*))
                                result))
                        (white-loop (rest rest-white-list) previous-key next-key axis))))
             (parse-loop (rest-layout &optional previous-key)
               (cond ((null rest-layout) nil)
                     ((evenp (caar rest-layout))
                      (format t "~&White key detected, ~a." rest-layout)
                      (white-loop (cdar rest-layout) previous-key (cadr rest-layout)
                                  (caar rest-layout))
                      (parse-loop (rest rest-layout) (first rest-layout)))
                     ((oddp (caar rest-layout))
                      (format t "~&Black key detected, ~a." rest-layout)
                      (black-loop (cdar rest-layout) previous-key (cadr rest-layout)
                                  (caar rest-layout))
                      (parse-loop (rest rest-layout) (first rest-layout))))))
      (parse-loop (sort-keyboard-layout keyboard-layout)))
    result))


(defparameter *arciorgano-kbd* '((0 . 0)
                                 (1 . 1)
                                 (1 . 2)
                                 (2 . 0)
                                 (3 . 1)
                                 (3 . 2)
                                 (4 . 0)
                                 (0 . 3)
                                 (1 . 4)
                                 (1 . 5)
                                 (2 . 3)
                                 ))

(defparameter *arciorgano-kbd* '((0 . 0)
                                 (1 . 1)
                                 (1 . 2)
                                 (1 . 4)
                                 (1 . 6)
                                 (2 . 0)
                                 (3 . 1)
                                 (3 . 2)
                                 (3 . 4)
                                 (3 . 6)
                                 (4 . 0)
                                 (5 . 3)
                                 (5 . 5)
                                 (6 . 0)))

(defun create-keyboard (clog-obj keyboard-layout)
  (let* ((svg (create-svg-toplevel clog-obj :width 1200 :height 500))
         (scale 4)
         (padding 0.5)
         (x-offset 40)
         (y-offset 320))
    (dolist (key (parse-keyboard-layout keyboard-layout))
      (let ((shape (create-svg-ortho-shape svg
                                           :x-origin (+ x-offset
                                                        (* scale
                                                           (- (* (key-axis key) (* 1/2 *white-width*))
                                                              (if (key-whitep key)
                                                                  (* 1/2 *white-width*)
                                                                  (* 1/2 *black-width*))
                                                              )))
                                           :y-origin (+ y-offset
                                                        (* scale
                                                           (- (+ padding (aref *row-depths* (key-ordine key))))))
                                           :fill (if (key-whitep key) "transparent" "gray")
                                           :stroke "black"
                                           :deltas (mapcar (lambda (distance) (* distance scale))
                                                           (if (key-whitep key)
                                                               (list (- (key-width key) (* 2 padding))
                                                                     (- (- (key-right-front-leg key) (* 2 padding)))
                                                                     (- (key-right-leg-offset key))
                                                                     (- (key-right-back-leg key))
                                                                     (- (- (key-back-width key) (* 2 padding)))
                                                                     (key-left-back-leg key)
                                                                     (- (key-right-leg-offset key)))
                                                               (list (- (key-width key) (* 2 padding))
                                                                     (- (- (key-right-front-leg key) (* 2 padding)))
                                                                     (- (- (key-back-width key) (* 2 padding)))))))))
        (set-on-mouse-over shape (lambda (obj)
                                   (declare (ignore obj))
                                   (setf (svg-attribute shape "fill") "blue")))
        (set-on-mouse-leave shape (lambda (obj)
                                    (declare (ignore obj))
                                    (setf (svg-attribute shape "fill") "transparent"))))
      )))

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
