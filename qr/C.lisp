(defun C (input-name output-name)
  (with-open-file (file input-name)
    (with-open-file (output output-name
			    :direction :output
			    :if-does-not-exist :create
			    :if-exists :supersede)
      (let ((ncases (read file)))
	(dotimes (k ncases)
	  (let* ((n (read file))
		 (j (read file)))
	    (format output "Case #~D:~%" (1+ k))
	    (generate-jamcoins n j output)))))))

(defstruct (jamcoin-state
	     (:conc-name js-))
  b 					;bit-pattern
  r-values
  r-divisors
  r-max-divisors
)

(defun primep (n)
  (cond ((< n 2) nil)
	((< n 4) t)
	(t (let ((limit (truncate (sqrt n))))
	     (do ((div 2 (+ div 1)))
		 ((> div limit) t)
	       (multiple-value-bind (ignore remainder)
		   (truncate n div)
		 (declare (ignore ignore))
		 (when (zerop remainder)
		   (return nil))))))))

(defun generate-jamcoins (n need &optional (output-stream t))
  (generate-jamcoins-1 n 0 need output-stream))

(defun generate-jamcoins-1 (n start need output-stream)
  (when (> need 0)
    (multiple-value-bind (c next-start)
	(generate-candidate-batch n start (batch-size need))
      (assert (not (endp c)))
      (let ((still-needed (test-jamcoin-candidates c n need output-stream)))
	(and (> still-needed 0)
	     (generate-jamcoins-1 n next-start still-needed))))))

(defun batch-size (need)
  (* need 3)				;arbitrary
)

(defun generate-candidate-batch (n start size)
  (let ((seq (loop for k from start below (min (+ start size) (expt 2 (- n 2)))
		collect k)))
    (values seq (+ start size))))


(defun test-jamcoin-candidates (c n need output-stream)
  (let ((states (make-array (list (length c)))))
    (loop for k from 0 below (length c)
       do
	 (setf (aref states k)
	       (create-jamcoin-state (elt c k) n)))
    (loop for div from 2
       do
	 (when (primep div)
	   (let ((hope nil))
	     (loop for k from 0 below (length c)
		do
		  (when (aref states k)
		    (let ((result (test-jamcoin-against-divisor
				   (aref states k)
				   div)))
		      (cond ((eq result :never)
			     (setf (aref states k) nil))
			    ((eq result t)
			     (output-jamcoin (aref states k) output-stream)
			     (setf (aref states k) nil)
			     (decf need)
			     (when (<= need 0)
			       (return-from test-jamcoin-candidates 0)))
			    (t (setq hope t))))))
	     (unless hope
	       (return-from test-jamcoin-candidates need)))))))

(defun output-jamcoin (s output-stream)
  (format output-stream "~D ~{~D~^ ~}~%"
	  (aref (js-r-values s) 10)
	  (coerce (subseq (js-r-divisors s) 2) 'list)))

(defun create-jamcoin-state (b n)
  (let ((s (make-jamcoin-state
	    :b b
	    :r-values (make-array (list 11))
	    :r-divisors (make-array (list 11) :initial-element 0)
	    :r-max-divisors (make-array (list 11))))
	(bits-as-string (format nil "~B" b)))
    (do ((R 2 (+ R 1)))
	((> R 10))
      (let ((R-val (+ (expt R (- n 1)) 1
		      (* R (parse-integer bits-as-string :radix R)))))
	(setf (aref (js-r-values s) R) R-val)
	(setf (aref (js-r-max-divisors s) R) (truncate (sqrt R-val)))))
    s))

(defun test-jamcoin-against-divisor (s div)
  (let ((have-divisorless-bases nil))
    (do ((R 2 (+ R 1)))
	((> R 10) nil)
      (cond ((/= (aref (js-r-divisors s) R) 0))
	    ((> div (aref (js-r-max-divisors s) R))
	     (return-from test-jamcoin-against-divisor :never))
	    ((multiple-value-bind (quotient remainder)
		 (truncate (aref (js-r-values s) R) div)
	       (declare (ignore quotient))
	       (= remainder 0))
	     (setf (aref (js-r-divisors s) R) div))
	    (t (setq have-divisorless-bases t))))
    (not have-divisorless-bases)))
