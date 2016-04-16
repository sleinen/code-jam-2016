(defun D (input-name output-name)
  (with-open-file (file input-name)
    (with-open-file (output output-name
			    :direction :output
			    :if-does-not-exist :create
			    :if-exists :supersede)
      (let ((ncases (read file)))
	(dotimes (caseno ncases)
	  (let* ((k (read file))
		 (c (read file))
		 (s (read file)))
	    (format output "Case #~D: ~A~%" (1+ caseno) (gold k c s))))))))

(defun gold (k c s)
  (let ((min-s (truncate k c)))
    (assert (eq (< s min-s) (< (* c s) k)))
    (if (< s min-s)
	"IMPOSSIBLE"
	(select-tiles k c))))

(defun select-tiles (k c)
  (format nil "~{~D~^ ~}"
	  (mapcar #'(lambda (seq)
		      (compute-position seq k c))
		  (split-seq (iota k) c))))

(defun compute-position (seq k c)
  (1+ (compute-position-1 seq k 0)))

(defun compute-position-1 (seq k acc)
  (if (endp seq)
      acc
      (compute-position-1 (rest seq) k (+ (* acc k) (first seq)))))

(defun iota (k)
  (if (< k 1)
      '()
      (append (iota (1- k)) (list (1- k)))))

(defun split-seq (seq k)
  (nreverse (split-seq-1 seq k '())))

(defun split-seq-1 (seq k acc)
  (cond ((>= k (length seq))
	 (cons seq acc))
	(t (split-seq-1 (subseq seq k)
			k
			(cons (subseq seq 0 k) acc)))))
