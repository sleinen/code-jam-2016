;;; Counting Sheep

(defun A (input-name output-name)
  (with-open-file (file input-name)
    (with-open-file (output output-name
			    :direction :output
			    :if-does-not-exist :create
			    :if-exists :supersede)
      (let ((ncases (read file)))
	(dotimes (k ncases)
	  (let ((base (read file)))
	    (format output "Case #~D: ~A~%" (1+ k) (count-sheep base))))))))

(defun count-sheep (base)
  (if (zerop base)
      "INSOMNIA"
      (count-1 base 1 '(0 1 2 3 4 5 6 7 8 9))))

(defun product (n i) (* n i))

(defun digits-in-product (n i)
  (digits-in-number (product n i)))

(defun digits-in-number (n)
  (let ((string-repr (format nil "~D" n))
	(digits '()))
    (loop for k from 0 below (length string-repr)
       do
	 (let ((digit (- (char-code (char string-repr k))
			 (char-code #\0))))
	   (pushnew digit digits)))
    digits))

(defun count-1 (base n not-seen-yet)
  (let ((new-digits (digits-in-product base n)))
    (let ((next-not-seen-yet
	   (set-difference
	    not-seen-yet
	    new-digits)))
      (if (null next-not-seen-yet)
	  (product base n)
	(count-1 base (+ n 1) next-not-seen-yet)))))
