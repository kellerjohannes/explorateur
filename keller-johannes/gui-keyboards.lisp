(in-package :explorateur.ui)

(defparameter *std-12*
  '(:name "Standard keyboard"
    :description "Normal keyboard with 12 keys per octave."
    :total-depth 40 :white-width 12 :black-width 8 :row-depths (0 20)
    :layout ((0 . 0) ; C
             (1 . 1)
             (2 . 0) ; D
             (3 . 1)
             (4 . 0) ; E
             (6 . 0) ; F
             (7 . 1)
             (8 . 0) ; G
             (9 . 1)
             (10 . 0) ; A
             (11 . 1)
             (12 . 0) ; B♮

             (14 . 0) ; C
             (15 . 1)
             (16 . 0) ; D
             (17 . 1)
             (18 . 0) ; E
             (20 . 0) ; F
             (21 . 1)
             (22 . 0) ; G
             (23 . 1)
             (24 . 0) ; A
             (25 . 1)
             (26 . 0) ; B♮

             (28 . 0) ; C
             (29 . 1)
             (30 . 0) ; D
             (31 . 1)
             (32 . 0) ; E
             (34 . 0) ; F
             (35 . 1)
             (36 . 0) ; G
             (37 . 1)
             (38 . 0) ; A
             (39 . 1)
             (40 . 0) ; B♮

             (42 . 0) ; C
             (43 . 1)
             (44 . 0) ; D
             (45 . 1)
             (46 . 0) ; E
             (48 . 0) ; F
             (49 . 1)
             (50 . 0) ; G
             (51 . 1)
             (52 . 0) ; A
             (53 . 1)
             (54 . 0) ; B♮
             )))

(defparameter *bremen*
  '(:name "Cimbalo Cromatico Bremen"
    :description "Cimbalo Cromatico with 19 keys per octave."
    :total-depth 50 :white-width 12 :black-width 8 :row-depths (0 20 35)
    :layout (
             (0 . 0) ; C
             (1 . 1) (1 . 2)
             (2 . 0) ; D
             (3 . 1) (3 . 2)
             (4 . 0) ; E
             (5 . 2)
             (6 . 0) ; F
             (7 . 1) (7 . 2)
             (8 . 0) ; G
             (9 . 1) (9 . 2)
             (10 . 0) ; A
             (11 . 1) (11 . 2)
             (12 . 0) ; B♮
             (13 . 2)

             (14 . 0) ; C
             (15 . 1) (15 . 2)
             (16 . 0) ; D
             (17 . 1) (17 . 2)
             (18 . 0) ; E
             (19 . 2)
             (20 . 0) ; F
             (21 . 1) (21 . 2)
             (22 . 0) ; G
             (23 . 1) (23 . 2)
             (24 . 0) ; A
             (25 . 1) (25 . 2)
             (26 . 0) ; B♮
             (27 . 2)

             (28 . 0) ; C
             (29 . 1) (29 . 2)
             (30 . 0) ; D
             (31 . 1) (31 . 2)
             (32 . 0) ; E
             (33 . 2)
             (34 . 0) ; F
             (35 . 1) (35 . 2)
             (36 . 0) ; G
             (37 . 1) (37 . 2)
             (38 . 0) ; A
             (39 . 1) (39 . 2)
             (40 . 0) ; B♮
             (41 . 2)

             (42 . 0) ; C
             (43 . 1) (43 . 2)
             (44 . 0) ; D
             (45 . 1) (45 . 2)
             (46 . 0) ; E
             (47 . 2)
             (48 . 0) ; F
             (49 . 1) (49 . 2)
             (50 . 0) ; G
             (51 . 1) (51 . 2)
             (52 . 0) ; A
             (53 . 1) (53 . 2)
             (54 . 0) ; B♮
             (55 . 2)

             (56 . 0) ; C
             )))

(defparameter *clavemusicum*
  '(:name "Clavemusicum Omnitonum"
    :description "31 keys per octave in one manual, based on Vito Trasuntino."
    :total-depth 55 :white-width 12 :black-width 8 :row-depths (0 15 25 30 35 40 45)
    :layout (
             (0 . 0) ; C
             (1 . 1) (1 . 2) (1 . 4) (1 . 6)
             (2 . 0) ; D
             (3 . 1) (3 . 2) (3 . 4) (3 . 6)
             (4 . 0) ; E
             (5 . 3) (5 . 5)
             (6 . 0) ; F
             (7 . 1) (7 . 2) (7 . 4) (7 . 6)
             (8 . 0) ; G
             (9 . 1) (9 . 2) (9 . 4) (9 . 6)
             (10 . 0) ; A
             (11 . 1) (11 . 2) (11 . 4) (11 . 6)
             (12 . 0) ; B♮
             (13 . 3) (13 . 5)

             (14 . 0) ; C
             (15 . 1) (15 . 2) (15 . 4) (15 . 6)
             (16 . 0) ; D
             (17 . 1) (17 . 2) (17 . 4) (17 . 6)
             (18 . 0) ; E
             (19 . 3) (19 . 5)
             (20 . 0) ; F
             (21 . 1) (21 . 2) (21 . 4) (21 . 6)
             (22 . 0) ; G
             (23 . 1) (23 . 2) (23 . 4) (23 . 6)
             (24 . 0) ; A
             (25 . 1) (25 . 2) (25 . 4) (25 . 6)
             (26 . 0) ; B♮
             (27 . 3) (27 . 5)

             (28 . 0) ; C
             (29 . 1) (29 . 2) (29 . 4) (29 . 6)
             (30 . 0) ; D
             (31 . 1) (31 . 2) (31 . 4) (31 . 6)
             (32 . 0) ; E
             (33 . 3) (33 . 5)
             (34 . 0) ; F
             (35 . 1) (35 . 2) (35 . 4) (35 . 6)
             (36 . 0) ; G
             (37 . 1) (37 . 2) (37 . 4) (37 . 6)
             (38 . 0) ; A
             (39 . 1) (39 . 2) (39 . 4) (39 . 6)
             (40 . 0) ; B♮
             (41 . 3) (41 . 5)

             (42 . 0) ; C
             (43 . 1) (43 . 2) (43 . 4) (43 . 6)
             (44 . 0) ; D
             (45 . 1) (45 . 2) (45 . 4) (45 . 6)
             (46 . 0) ; E
             (47 . 3) (47 . 5)
             (48 . 0) ; F
             (49 . 1) (49 . 2) (49 . 4) (49 . 6)
             (50 . 0) ; G
             (51 . 1) (51 . 2) (51 . 4) (51 . 6)
             (52 . 0) ; A
             (53 . 1) (53 . 2) (53 . 4) (53 . 6)
             (54 . 0) ; B♮
             (55 . 3) (55 . 5)

             (56 . 0) ; C
             )))

(defparameter *arciorgano*
  '(:name "Arciorgano"
    :description "38 keys per octave in two manuals, based on Nicola Vicentino."
    :total-depth 70 :white-width 12 :black-width 9 :row-depths (0 15 25 35 50 60)
    :layout (
             (0 . 0) (0 . 3) ; C
             (1 . 1) (1 . 2) (1 . 4) (1 . 5)
             (2 . 0) (2 . 3) ; D
             (3 . 1) (3 . 2) (3 . 4) (3 . 5)
             (4 . 0) (4 . 3) ; E
             (5 . 2) (5 . 5)
             (6 . 0) (6 . 3) ; F
             (7 . 1) (7 . 2) (7 . 4) (7 . 5)
             (8 . 0) (8 . 3) ; G
             (9 . 1) (9 . 2) (9 . 4) (9 . 5)
             (10 . 0) (10 . 3) ; A
             (11 . 1) (11 . 2) (11 . 4) (11 . 5)
             (12 . 0) (12 . 3) ; B♮
             (13 . 2) (13 . 5)

             (14 . 0) (14 . 3) ; C
             (15 . 1) (15 . 2) (15 . 4) (15 . 5)
             (16 . 0) (16 . 3) ; D
             (17 . 1) (17 . 2) (17 . 4) (17 . 5)
             (18 . 0) (18 . 3) ; E
             (19 . 2) (19 . 5)
             (20 . 0) (20 . 3) ; F
             (21 . 1) (21 . 2) (21 . 4) (21 . 5)
             (22 . 0) (22 . 3) ; G
             (23 . 1) (23 . 2) (23 . 4) (23 . 5)
             (24 . 0) (24 . 3) ; A
             (25 . 1) (25 . 2) (25 . 4) (25 . 5)
             (26 . 0) (26 . 3) ; B♮
             (27 . 2) (27 . 5)

             (28 . 0) (28 . 3) ; C
             (29 . 1) (29 . 2) (29 . 4) (29 . 5)
             (30 . 0) (30 . 3) ; D
             (31 . 1) (31 . 2) (31 . 4) (31 . 5)
             (32 . 0) (32 . 3) ; E
             (33 . 2) (33 . 5)
             (34 . 0) (34 . 3) ; F
             (35 . 1) (35 . 2) (35 . 4) (35 . 5)
             (36 . 0) (36 . 3) ; G
             (37 . 1) (37 . 2) (37 . 4) (37 . 5)
             (38 . 0) (38 . 3) ; A
             (39 . 1) (39 . 2) (39 . 4) (39 . 5)
             (40 . 0) (40 . 3) ; B♮
             (41 . 2) (41 . 5)

             (42 . 0) (42 . 3) ; C
             (43 . 1) (43 . 2) (43 . 4) (43 . 5)
             (44 . 0) (44 . 3) ; D
             (45 . 1) (45 . 2) (45 . 4) (45 . 5)
             (46 . 0) (46 . 3) ; E
             (47 . 2) (47 . 5)
             (48 . 0) (48 . 3) ; F
             (49 . 1) (49 . 2) (49 . 4) (49 . 5)
             (50 . 0) (50 . 3) ; G
             (51 . 1) (51 . 2) (51 . 4) (51 . 5)
             (52 . 0) (52 . 3) ; A
             (53 . 1) (53 . 2) (53 . 4) (53 . 5)
             (54 . 0) (54 . 3) ; B♮
             (55 . 2) (55 . 5)

             (56 . 0) (56 . 3) ; C
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





(defun create-hexagonal-keyboard (container rows columns angle
                                  &key (key-width 20) (scale 2.5) (padding 5))
  (let ((svg (create-svg-toplevel container :width (* scale key-width columns)
                                            :height (* scale key-width rows))))


    (let ((offset 0))
      (dotimes (row rows)
        (dotimes (column columns)
          (let ((shape (create-svg-hex-shape svg
                                             :side-length (- (* scale key-width) padding)
                                             :stroke "black"
                                             :fill "transparent"
                                             :x-origin (+ offset (* column (* scale key-width)))
                                             :y-origin (* row (* scale key-width)))))
            (set-on-mouse-over shape (lambda (obj)
                                       (declare (ignore obj))
                                       (setf (svg-attribute shape "fill") "blue")))
            (set-on-mouse-leave shape (lambda (obj)
                                        (declare (ignore obj))
                                        (setf (svg-attribute shape "fill") "transparent")))))))))
