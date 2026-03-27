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
                                 (9 . 2)))

(defparameter *total-depth* 36)
(defparameter *key-width* 12)
(defparameter *black-width* 6)
(defparameter *depth-row-1* 16)
(defparameter *depth-row-2* 9)
(defparameter *depth-row-3* 9)


(defun create-keyboard (obj)
  (let* ((svg (create-svg-toplevel obj :width 1200 :height 500))
         (scale 2)
         ())))

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
