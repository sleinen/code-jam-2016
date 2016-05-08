(defun A (basename)
  (with-open-file (file (format nil "~A.in" basename))
    (with-open-file (output (format nil "~A.out" basename)
			    :direction :output
			    :if-does-not-exist :create
			    :if-exists :supersede)
      (let ((ncases (read file)))
	(dotimes (k ncases)
	  (let ((N (read file)))
	    (let ((P-i (make-array (list N) :element-type '(unsigned-byte 16))))
	      (dotimes (k N)
		(setf (aref P-i k) (read file)))
	      (format output "Case #~D:~{ ~A~}~%" (1+ k)
		      (mapcar #'lettrify (solve P-i)))))
	  )))))

(defun lettrify (list)
  (map 'string #'(lambda (i) (code-char (+ i (char-code #\A)))) list))

(defun solve (P-i)
  (let ((sum (reduce #'+ P-i)))
    (do ((solution '()))
	((= sum 0) (reverse solution))
      (let ((max (reduce #'max P-i)))
	(let ((max-count (count max P-i)))
	  (cond ((or (= max-count 1) (= max-count 3))
		 (let ((max-i (position max P-i)))
		   (push (list max-i) solution)
		   (decf (aref P-i max-i))
		   (decf sum 1)))
		(t ;(= max-count 2)
		 (let* ((max-i1 (position max P-i))
			(max-i2 (position max P-i :start (1+ max-i1))))
		   (push (list max-i1 max-i2) solution)
		   (decf (aref P-i max-i1))
		   (decf (aref P-i max-i2))
		   (decf sum 2)))
		(t (error "Can't deal with"))))))
    ))