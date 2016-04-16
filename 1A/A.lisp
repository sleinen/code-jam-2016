
(defun A (basename)
  (with-open-file (file (format nil "~A.in" basename))
    (with-open-file (output (format nil "~A.out" basename)
			    :direction :output
			    :if-does-not-exist :create
			    :if-exists :supersede)
      (let ((ncases (read file)))
	(dotimes (k ncases)
	  (let ((S (read-line file)))
	    (format output "Case #~D: ~A~%" (1+ k) (lastword S))))))))

(defun lastword (w)
  (do ((sol (string (char w 0)) sol)
       (k 1 (1+ k)))
      ((>= k (length w)) sol)
    (let ((c (char w k)))
      (setq sol
	    (if (char< c (char sol 0))
		(concatenate 'string sol (string c))
		(concatenate 'string (string c) sol))))))
