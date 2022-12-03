(defun get-all-carried (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun split-by-elf (food)
  (let ((elf nil)
	(res nil))
    (dolist (e food res)
      (cond ((equal e "")
	     (push elf res)
	     (setf elf nil))
	    (t (push (parse-integer e) elf))))))

(defun total-elf (food)
  (mapcar #'(lambda (e) (reduce #'+ e)) food))

(defun most-carried (food)
  (apply #'max food))

(defun top-n-elves (n food)
  (do ((res nil)
       (elves food)
       (i 0 (+ i 1)))
      ((equal i n) res)
    (push (most-carried elves) res)
    (setf elves (delete (first res) elves))))

(defun total-elves (food)
  (reduce #'+ food))

(most-carried (total-elf (split-by-elf (get-all-carried "food"))))
(total-elves (top-n-elves 3 (total-elf (split-by-elf (get-all-carried "food")))))
