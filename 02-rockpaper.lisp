(defun get-all-guide (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun hand-score (hand)
  (cond ((equal hand "X") 1)
	((equal hand "Y") 2)
	((equal hand "Z") 3)))

(defun win-lose (them me)
  (let ((hand (concatenate 'string them me)))
    (cond ((equal hand "AX") 3) 
	  ((equal hand "AY") 6)
	  ((equal hand "AZ") 0)
	  ((equal hand "BX") 0)
	  ((equal hand "BY") 3)
	  ((equal hand "BZ") 6)
	  ((equal hand "CX") 6)
	  ((equal hand "CY") 0)
	  ((equal hand "CZ") 3))))

(defun doround (them me)
  (+ (hand-score me) (win-lose them me)))

(defun do-tourney (guide)
  (reduce #'+ (mapcar #'(lambda (e) (doround (subseq e 0 1) (subseq e 2))) guide)))

(defun hand-score-2 (them me)
  (let ((hand (concatenate 'string them me)))
    (cond ((equal hand "AX") 3)
	  ((equal hand "AY") 1)
	  ((equal hand "AZ") 2)
	  ((equal hand "BX") 1)
	  ((equal hand "BY") 2)
	  ((equal hand "BZ") 3)
	  ((equal hand "CX") 2)
	  ((equal hand "CY") 3)
	  ((equal hand "CZ") 1))))

(defun win-lose-2 (me)
  (cond ((equal me "X") 0)
	((equal me "Y") 3)
	((equal me "Z") 6)))

(defun doround-2 (them me)
  (+ (hand-score-2 them me) (win-lose-2 me)))

(defun dotourney-2 (guide)
  (reduce #'+ (mapcar #'(lambda (e) (doround-2 (subseq e 0 1) (subseq e 2))) guide)))

(do-tourney (get-all-guide "guide"))

(dotourney-2 (get-all-guide "guide"))
