
(defun B (basename)
  (with-open-file (file (format nil "~A.in" basename))
    (with-open-file (output (format nil "~A.out" basename)
			    :direction :output
			    :if-does-not-exist :create
			    :if-exists :supersede)
      (let ((ncases (read file)))
	(dotimes (k ncases)
	  (let ((n (read file)))
	    (let ((nstrips (- (* n 2) 1)))
	      (let ((counts (make-array 2501 :initial-element 0)))
		(dotimes (i nstrips)
		  (dotimes (j n)
		    (incf (aref counts (read file)))))
		(let ((solution '()))
		  (dotimes (k 2501)
		    (when (oddp (aref counts k))
		      (push k solution)))
		  (assert (= (length solution) n))
		  (format output "Case #~D:~{ ~A~}~%" (1+ k) (nreverse solution)))))))))))

