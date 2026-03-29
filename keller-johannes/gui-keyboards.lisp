(in-package :explorateur.ui)


(defparameter *clavemusicum*
  '(:name "Clavemusicum Omnitonum"
    :description "31 keys per octave in one manual, based on Vito Trasuntino."
    :total-depth 55 :white-width 12 :black-width 8 :row-depths (0 15 25 30 35 40 45)
    :layout ((0 . 0) ; C
             (1 . 1) (1 . 2) (1 . 4) (1 . 6)
             (2 . 0) ; D
             (3 . 1) (3 . 2) (3 . 4) (3 . 6) (4 . 0)
             (5 . 3) (5 . 5) ; E
             (6 . 0) ; F
             )))

(defparameter *arciorgano*
  '(:name "Arciorgano"
    :description "38 keys per octave in two manuals, based on Nicola Vicentino."
    :total-depth 70 :white-width 12 :black-width 9 :row-depths (0 15 25 35 50 60)
    :layout ((0 . 0) (0 . 3) ; C
             (1 . 1) (1 . 2) (1 . 4) (1 . 5)
             (2 . 0) (2 . 3) ; D
             (3 . 1) (3 . 2) (3 . 4) (3 . 5)
             (4 . 0) (4 . 3) ; E
             (5 . 2) (5 . 5)
             (6 . 0) (6 . 3) ; F
             )))


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
  right-back-leg)

(defun reverse-keyboard-layout (keyboard-layout)
  (sort (loop for (axis occurrances) on keyboard-layout by #'cddr
              collect (cons axis (sort occurrances #'<)))
        #'< :key #'first))

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

(defun calculate-black-length (black-list previous-key next-key row-depths total-depth)
  (let ((upper-end-index (next-lower-row black-list previous-key next-key)))
    (if upper-end-index
        (- (aref row-depths upper-end-index)
           (aref row-depths (first black-list)))
        (- total-depth (aref row-depths (first black-list))))))

(defun next-limiting-key (white-list neighbour-key)
  (first (sort (copy-list (append (rest white-list)
                                  (high-pass-filter (rest neighbour-key) (first white-list))))
               #'<)))

(defun calculate-white-front-leg (white-list neighbour-key row-depths total-depth)
  (let ((upper-end-index (next-limiting-key white-list neighbour-key)))
    (if upper-end-index
        (- (aref row-depths upper-end-index)
           (aref row-depths (first white-list)))
        (- total-depth (aref row-depths (first white-list))))))

(defun calculate-white-length (white-list row-depths total-depth)
  (if (null (rest white-list))
      (- total-depth (aref row-depths (first white-list)))
      (- (aref row-depths (first (rest white-list)))
         (aref row-depths (first white-list)))))

(defun parse-keyboard-layout (keyboard-layout white-width black-width row-depths total-depth)
  (let ((result))
    (labels ((black-loop (rest-black-list previous-key next-key axis)
               (cond ((null rest-black-list) nil)
                     (t (let ((black-length (calculate-black-length rest-black-list
                                                                    previous-key
                                                                    next-key
                                                                    row-depths
                                                                    total-depth)))
                          (push (make-key :ordine (first rest-black-list)
                                          :axis axis
                                          :whitep nil
                                          :width black-width
                                          :left-front-leg black-length
                                          :right-front-leg black-length
                                          :back-width black-width)
                                result)
                          (black-loop (rest rest-black-list) previous-key next-key axis)))))
             (white-loop (rest-white-list previous-key next-key axis)
               (cond ((null rest-white-list) nil)
                     (t (let ((left-front-leg (calculate-white-front-leg rest-white-list
                                                                         previous-key
                                                                         row-depths
                                                                         total-depth))
                              (right-front-leg (calculate-white-front-leg rest-white-list
                                                                          next-key
                                                                          row-depths
                                                                          total-depth))
                              (white-length (calculate-white-length rest-white-list
                                                                    row-depths
                                                                    total-depth)))
                          (push (make-key :ordine (first rest-white-list)
                                          :axis axis
                                          :whitep t
                                          :width white-width
                                          :left-front-leg left-front-leg
                                          :right-front-leg right-front-leg
                                          :left-leg-offset (* 1/2 black-width)
                                          :right-leg-offset (* 1/2 black-width)
                                          :left-back-leg (- white-length left-front-leg)
                                          :right-back-leg (- white-length right-front-leg)
                                          :back-width (- white-width black-width))
                                result))
                        (white-loop (rest rest-white-list) previous-key next-key axis))))
             (parse-loop (rest-layout &optional previous-key)
               (cond ((null rest-layout) nil)
                     ((evenp (caar rest-layout))
                      (white-loop (cdar rest-layout) previous-key (cadr rest-layout)
                                  (caar rest-layout))
                      (parse-loop (rest rest-layout) (first rest-layout)))
                     ((oddp (caar rest-layout))
                      (black-loop (cdar rest-layout) previous-key (cadr rest-layout)
                                  (caar rest-layout))
                      (parse-loop (rest rest-layout) (first rest-layout))))))
      (parse-loop (sort-keyboard-layout keyboard-layout)))
    result))

(defun get-max-axis (keyboard)
  (car (first (sort (copy-list (getf keyboard :layout)) #'> :key #'car))))

(defun create-keyboard (clog-obj keyboard &key (scale 3) (padding 0.5))
  (let* ((white-width (getf keyboard :white-width))
         (black-width (getf keyboard :black-width))
         (total-depth (getf keyboard :total-depth))
         (row-depths (make-array (length (getf keyboard :row-depths))
                                 :initial-contents (getf keyboard :row-depths)))
         (svg (create-svg-toplevel clog-obj :width (* (float scale)
                                                      (1+ (floor (get-max-axis keyboard) 2))
                                                      white-width)
                                            :height (* (float scale) total-depth))))
    (dolist (key (parse-keyboard-layout (getf keyboard :layout)
                                        white-width black-width row-depths total-depth))
      (let ((shape (create-svg-ortho-shape
                    svg
                    :x-origin (+ 1
                                 (* (float scale) 1/2 (getf keyboard :white-width))
                                 (* (float scale) (- (* (key-axis key) (* 1/2 white-width))
                                                     (if (key-whitep key)
                                                         (* 1/2 white-width)
                                                         (* 1/2 black-width)))))
                    :y-origin (+ 1
                                 (* (float scale) (getf keyboard :total-depth))
                                 (* (float scale) (- (+ padding
                                                        (aref row-depths (key-ordine key))))))
                    :fill (if (key-whitep key) "transparent" "gray")
                    :stroke "black"
                    :deltas (mapcar (lambda (distance) (* distance (float scale)))
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
                                    (setf (svg-attribute shape "fill") "transparent")))))))
