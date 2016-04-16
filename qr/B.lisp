(defun B (input-name output-name)
  (with-open-file (file input-name)
    (with-open-file (output output-name
			    :direction :output
			    :if-does-not-exist :create
			    :if-exists :supersede)
      (let ((ncases (read file)))
	(dotimes (k ncases)
	  (let ((stack (read-line file)))
	    (format output "Case #~D: ~A~%" (1+ k) (nmaneuvers stack))))))))

(defun nmaneuvers (stack)
  (let ((npancakes (length stack)))
    (let ((last-pancake (char stack (1- npancakes))))
      (nman-1 stack (1- npancakes) last-pancake (if (char= last-pancake #\-) 1 0)))))

(defun nman-1 (stack k current count)
  (cond ((= k 0) count)
	((eql (char stack (1- k)) current)
	 (nman-1 stack (1- k) current count))
	(t (nman-1 stack (1- k) (char stack (1- k)) (1+ count)))))
