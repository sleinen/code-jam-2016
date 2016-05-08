(defun A (basename)
  (with-open-file (file (format nil "~A.in" basename))
    (with-open-file (output (format nil "~A.out" basename)
			    :direction :output
			    :if-does-not-exist :create
			    :if-exists :supersede)
      (let ((ncases (read file)))
	(dotimes (k ncases)
	  (let ((S (read-line file)))
	    (format output "Case #~D: ~{~A~}~%" (1+ k) (decode S)))
	  )))))

(defvar numbers)
(setq numbers #("ZERO"
		"ONE"
		"TWO"
		"THREE"
		"FOUR"
		"FIVE"
		"SIX"
		"SEVEN"
		"EIGHT"
		"NINE"))

(defun ccounts ()
  (let ((chars '()))
    (dotimes (k (length numbers))
      (let ((number (aref numbers k)))
	(dotimes (i (length number))
	  (pushnew (char number i) chars))))
    (dolist (char chars)
      (let ((count 0))
	(dotimes (k (length numbers))
	  (let ((number (aref numbers k)))
	    (let ((c (count char number)))
	      (unless (zerop c)
		;(warn "~A has ~D ~A" number c char)
		(incf count)))))
	(warn "~A occurs in ~D numbers" char count)))
    chars))

(defun decode (s)
  (let ((counts '())
	(result '()))
    (labels ((ct (char)
	       (let ((a (assoc char counts)))
		 (if a (cdr a) 0)))
	     (found-number (digit n)
	       (when (> n 0)
		 (dotimes (k n)
		   (push digit result))
		 (let ((number (aref numbers digit)))
		   (dotimes (i (length number))
		     (let ((char (char number i)))
		       (let ((a (assoc char counts)))
			 (setf (cdr a) (- (cdr a) n)))))))))
      (dotimes (k (length s))
	(let ((char (char s k)))
	  (let ((count (assoc char counts)))
	    (if count
		(setf (cdr count) (1+ (cdr count)))
		(setq counts (cons (cons char 1) counts))))))
      (found-number 0 (ct #\Z))
      (found-number 2 (ct #\W))
      (found-number 4 (ct #\U))
      (found-number 6 (ct #\X))
      (found-number 8 (ct #\G))
      (found-number 1 (ct #\O))
      (found-number 3 (ct #\T))
      (found-number 5 (ct #\F))
      (found-number 7 (ct #\S))
      (found-number 9 (ct #\I)))
    (assert (every #'(lambda (a) (zerop (cdr a))) counts))
    (sort result #'<)))
