(ql:quickload "str")

(defun get-all-input (input)
  (with-open-file (stream input)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun parse-move (s)
  (let ((m (str:split " " s)))
    (cond ((equal "U" (first m)) (list 'u (parse-integer (second m))))
	  ((equal "R" (first m)) (list 'r (parse-integer (second m))))
	  ((equal "D" (first m)) (list 'd (parse-integer (second m))))
	  ((equal "L" (first m)) (list 'l (parse-integer (second m)))))))

(defun parse-all-move (s)
  (mapcar #'parse-move s))

(defstruct rope
  hx
  hy
  tx
  ty
  history)

(defun init-rope ()
  (make-rope :hx 0 :hy 0
	     :tx 0 :ty 0
	     :history (make-hash-table :test 'equal)))

(defun move-head (dir ro)
  (case dir
    (u (incf (rope-hy ro)))
    (r (incf (rope-hx ro)))
    (d (decf (rope-hy ro)))
    (l (decf (rope-hx ro))))
  ro)

(defun move-tail (ro)
  (let ((dx (- (rope-hx ro) (rope-tx ro)))
	(dy (- (rope-hy ro) (rope-ty ro))))
    (cond ((and (zerop dx)
		(equal dy 2))
	   (incf (rope-ty ro)))
	  ((and (zerop dx)
		(equal dy -2))
	   (decf (rope-ty ro)))
	  ((and (equal dx 2)
		(zerop dy))
	   (incf (rope-tx ro)))
	  ((and (equal dx -2)
		(zerop dy))
	   (decf (rope-tx ro)))
	  ((or (and (equal dx -2)
		    (equal dy -1))
	       (and (equal dx -2)
		    (equal dy -2))
	       (and (equal dx -1)
		    (equal dy -2)))
	   (decf (rope-tx ro))
	   (decf (rope-ty ro)))
	  ((or (and (equal dx 1)
		    (equal dy -2))
	       (and (equal dx 2)
		    (equal dy -2))
	       (and (equal dx 2)
		    (equal dy -1)))
	   (incf (rope-tx ro))
	   (decf (rope-ty ro)))
	  ((or (and (equal dx -2)
		    (equal dy 1))
	       (and (equal dx -2)
		    (equal dy 2))
	       (and (equal dx -1)
		    (equal dy 2)))
	   (decf (rope-tx ro))
	   (incf (rope-ty ro)))
	  ((or (and (equal dx 1)
		    (equal dy 2))
	       (and (equal dx 2)
		    (equal dy 2))
	       (and (equal dx 2)
		    (equal dy 1)))
	   (incf (rope-tx ro))
	   (incf (rope-ty ro)))))
  (setf (gethash (str:concat
		  (write-to-string (rope-tx ro)) ","
		  (write-to-string (rope-ty ro)))
		 (rope-history ro)) 'y)
  ro)

(defun do-move (m ro)
  (let ((dir (first m))
	(far (second m)))
    (dotimes (i far ro)
      (setf ro (move-tail (move-head dir ro))))))

(defun do-all-move (m ro)
  (mapcar #'(lambda (e) (do-move e ro)) m)
  ro)

(defun move-long-head (dir roa)
  (setf (nth 0 roa) (move-tail (move-head dir (first roa))))
  (dotimes (i (- (length roa) 1))
    (setf (rope-hx (nth (+ i 1) roa)) (rope-tx (nth i roa)))
    (setf (rope-hy (nth (+ i 1) roa)) (rope-ty (nth i roa)))
    (setf (nth (+ i 1) roa) (move-tail (nth (+ i 1) roa))))
  (setf (rope-hx (nth (- (length roa) 1) roa)) (rope-tx (nth (- (length roa) 2) roa)))
  (setf (rope-hy (nth (- (length roa) 1) roa)) (rope-ty (nth (- (length roa) 2) roa)))
  (setf (nth (- (length roa) 1) roa) (move-tail (nth (- (length roa) 1) roa)))
  roa)

(defun init-long-rope ()
  (let ((roa nil))
    (dotimes (i 9 roa)
      (push (init-rope) roa))))

(defun do-long-move (m roa)
  (let ((dir (first m))
	(far (second m)))
    (dotimes (i far roa)
      (setf roa (move-long-head dir roa)))))

(defun do-all-long-move (m roa)
  (mapcar #'(lambda (e) (do-long-move e roa)) m)
  roa)

(do-all-move (parse-all-move (get-all-input "input.txt")) (init-rope))
(do-all-long-move (parse-all-move (get-all-input "input.txt")) (init-long-rope))
