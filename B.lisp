(defun B (basename)
  (with-open-file (file (format nil "~A.in" basename))
    (with-open-file (output (format nil "~A.out" basename)
			    :direction :output
			    :if-does-not-exist :create
			    :if-exists :supersede)
      (let ((ncases (read file)))
	(dotimes (k ncases)
	  (let* ((B (read file))
		 (M (read file)))
	    (let ((matrix (slides B M)))
	      (if matrix
		  (progn
		    (format output "Case #~D: POSSIBLE~%~{~A~%~}" (1+ k)
			    (mapcar #'(lambda (x) (format nil "~v,'0B" B x))
				    matrix))
		    )
		  (format output "Case #~D: IMPOSSIBLE~%" (1+ k)))))
	  )))))

(defun maxpaths (B)
  (ash 1 (max (- B 2) 0)))

(defun slides (B M)
  (cond ((= B 1)
	 (ecase M (1 (list 0))))
	((> M (maxpaths B))
	 nil)
	(t
	 (cons (if (< M (maxpaths B)) (ash M 1) (- (* M 2) 1))
	       (slides (1- B) (maxpaths (1- B)))))))
