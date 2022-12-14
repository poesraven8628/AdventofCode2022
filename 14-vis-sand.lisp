(ql:quickload "str")
(ql:quickload "zpng")

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
      (render-frame m (format nil "vis/~4,'0d.png" times))
      (when (null cont)
	(return-from do-sand times)))))

(defun do-sand-2 (x y m)
  (draw-bottom m)
  (do-sand x y m))

(defun draw-vis (sx sy ex ey m file)
  (let ((png (make-instance 'zpng:pixel-streamed-png
                             :color-type :truecolor-alpha
                             :width (- ex sx)
                             :height (- ey sy))))
    (with-open-file (stream file
			    :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create
			    :element-type '(unsigned-byte 8))
      (zpng:start-png png stream)
      (dotimes (j (second (array-dimensions m)))
	(dotimes (i (first (array-dimensions m)))
	  (when (and (< i ex)
		     (< j ey)
		     (>= i sx)
		     (>= j sy))
	    (cond ((equal #\. (aref m i j))
		   (zpng:write-pixel (list 255 255 255 255) png))
		  ((equal #\# (aref m i j))
		   (zpng:write-pixel (list 0 0 0 255) png))
		  ((equal #\x (aref m i j))
		   (zpng:write-pixel (list 255 0 0 255) png))
		  ((equal #\o (aref m i j))
		   (zpng:write-pixel (list 255 255 0 255) png))))))
      (zpng:finish-png png))))

(defun render-frame (m file)
  (let* ((h (second (array-dimensions m)))
	 (sx (- 500 h))
	 (sy 0)
	 (ex (+ 500 h))
	 (ey h))
    (draw-vis sx sy ex ey m file)))

(do-sand 500 0 (setup "input.txt"))
(do-sand-2 500 0 (setup "input.txt"))
