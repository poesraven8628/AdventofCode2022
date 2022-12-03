(defun get-all-sacks (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun half-sacker (sack)
  (list (subseq sack 0 (/ (length sack) 2))
	(subseq sack (/ (length sack) 2))))

(defun find-common (sack)
  (do* ((a (first sack))
	(b (second sack))
	(letter (aref a 0))
	(length-b (length b)))
       ((> length-b (length b)) letter)
    (setf letter (aref a 0))
    (setf a (remove letter a))
    (setf b (remove letter b))))

(defun priority-value (item)
  (let ((pv " abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))
    (position item pv)))

(defun get-priority-total (sacks)
  (reduce #'+ (mapcar #'(lambda (e) (priority-value (find-common (half-sacker e)))) sacks)))

(defun find-trio-common (sack-a sack-b sack-c)
  (do* ((a sack-a)
	(b sack-b)
	(c sack-c)
	(letter (aref a 0))
	(length-b (length b))
	(length-c (length c)))
       ((and (> length-b (length b))
	     (> length-c (length c)))
	letter)
    (setf letter (aref a 0))
    (setf length-b (length b))
    (setf length-c (length c))
    (setf a (remove letter a))
    (setf b (remove letter b))
    (setf c (remove letter c))))

(defun elf-trios (sacks)
  (let ((set nil)
	(res nil))
    (dolist (s sacks res)
      (push s set)
      (when (equal (length set) 3)
	(push set res)
	(setf set nil)))))

(defun get-trio-priority-total (sacks)
  (reduce #'+
	  (mapcar #'(lambda (e) (priority-value
				 (find-trio-common (first e)
						   (second e)
						   (third e))))
		  (elf-trios sacks))))

(get-priority-total (get-all-sacks "data"))
(get-trio-priority-total (get-all-sacks "data"))

