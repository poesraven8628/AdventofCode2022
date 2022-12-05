(ql:quickload "str")

(defun get-all-input (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun init-stacks ()
  (list nil
	'(P G R N)
	'(C D G F L B T J)
	'(V S M)
	'(P Z C R S L)
	'(Q D W C V L S P)
	'(S M D W N T C)
	'(P W G D H)
	'(V M C S H P L Z)
	'(Z G W L F P R)))

(defun tokenize-input (s)
  (let ((tokens (str:split " " s)))
    (list (parse-integer (second tokens))
	  (parse-integer (fourth tokens))
	  (parse-integer (sixth tokens)))))

(defun do-order (order stack)
  (dotimes (i (first order))
    (push (pop (nth (second order) stack))
	  (nth (third order) stack))))

(defun do-orders (orders stack)
  (mapcar #'(lambda (e) (do-order (tokenize-input e) stack)) orders)
  stack)

(defun get-top-of-stack (stack)
  (list (first (second stack))
	(first (third stack))
	(first (fourth stack))
	(first (fifth stack))
	(first (sixth stack))
	(first (seventh stack))
	(first (eighth stack))
	(first (ninth stack))
	(first (tenth stack))))

(defun do-order-2 (order stack &aux (temp nil))
  (dotimes (i (first order))
    (push (pop (nth (second order) stack)) temp))
  (dotimes (i (first order))
    (push (pop temp) (nth (third order) stack))))

(defun do-orders-2 (orders stack)
  (mapcar #'(lambda (e) (do-order-2 (tokenize-input e) stack)) orders)
  stack)

(get-top-of-stack (do-orders (get-all-input "crate") (init-stacks)))
(get-top-of-stack (do-orders-2 (get-all-input "crate") (init-stacks)))
