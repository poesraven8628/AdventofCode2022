(ql:quickload "str")

(defun get-all-input (input)
  (with-open-file (stream input)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun parse-input (input &aux ret line word)
  (dolist (e input (reverse ret))
    (setf line nil)
    (push (dolist (el (str:split " -> " e) (reverse line))
	    (setf word nil)
	    (push (dolist (wo (str:split "," el) (reverse word))
		    (push (parse-integer wo) word))
		  line)) ret)))

(defun get-size (a &aux (mx 0) (my 0))
  (dolist (obj a (list (* 2 (+ 5 mx)) (+ 3 my)))
    (dolist (pos obj)
      (if (> (first pos) mx) (setf mx (first pos)))
      (if (> (second pos) my) (setf my (second pos))))))

(defun make-map (size)
  (make-array size :initial-element #\.))

(defun draw-h-line (sx sy ex ey m)
  (do ((x sx (+ 1 x))
       (y sy))
      ((equal x (+ 1 ex)) m)
    (setf (aref m x y) #\#)))

(defun draw-v-line (sx sy ex ey m)
  (do ((x sx)
       (y sy (+ 1 y)))
      ((equal y (+ 1 ey)) m)
    (setf (aref m x y) #\#)))

(defun draw-map (s m &aux x y nx ny)
  (dolist (seg s m)
    (dotimes (i (length seg))
      (setf x (first (nth i seg)))
      (setf y (second (nth i seg)))
      (unless (equal (+ i 1) (length seg))
	(setf nx (first (nth (+ i 1) seg)))
	(setf ny (second (nth (+ i 1) seg)))
	(cond ((> nx x) (draw-h-line x y nx ny m))
	      ((< nx x) (draw-h-line nx ny x y m))
	      ((> ny y) (draw-v-line x y nx ny m))
	      ((< ny y) (draw-v-line nx ny x y m)))))))

(defun draw-bottom (m)
  (draw-h-line 0 (- (second (array-dimensions m)) 1)
	       (- (first (array-dimensions m)) 1)
	       (- (second (array-dimensions m)) 1) m))

(defun setup (s &aux p m)
  (setf p (parse-input (get-all-input s)))
  (setf m (make-map (get-size p)))
  (setf m (draw-map p m))
  m)

(defun can-enter? (x y m)
  (if (equal #\. (aref m x y)) t nil))

(defun sand-fall (x y m)
  (unless (can-enter? x y m) (return-from sand-fall (values m nil)))
  (do ((nx x x)
       (ny (+ 1 y) (+ 1 y)))
      ((equal ny (second (array-dimensions m))) (values m nil))
    (unless (can-enter? nx ny m)
      (decf nx)
      (unless (can-enter? nx ny m)
	(incf nx 2)
	(unless (can-enter? nx ny m)
	  (decf nx)
	  (setf (aref m x y) #\o)
	  (return-from sand-fall (values m t)))))
    (setf x nx)
    (setf y ny)))

(defun do-sand (x y m)
  (do ((times 0 (+ 1 times)))
      (())
    (multiple-value-bind (map cont) (sand-fall x y m)
      (setf m map)
      (when (null cont)
	(return-from do-sand times)))))

(defun do-sand-2 (x y m)
  (draw-bottom m)
  (do-sand x y m))

(do-sand 500 0 (setup "input.txt"))
(do-sand-2 500 0 (setup "input.txt"))
