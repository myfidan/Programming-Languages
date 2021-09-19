
;;get 2 numbers from file
(defun read-file(filename)
	(with-open-file (stream filename)
		(loop for elem = (read stream nil)
			while elem
			collect elem
		)
	)
)	

;;write results to file
(defun write-file(output write-string num)
    (format output "~A ~A" num write-string)	
)

;; Check given num prime or not
(defun check-prime(num)
	(let ((prime t))
		(loop for i from 2 to (- num 1)
			do (if (eql (mod num i) 0) (setf prime nil))
			)
	(if (< num 2) nil prime)	
	))

;;check gicen num semiprime or not
(defun check-semiprime(num)
	(let ((semiprime nil))
		(loop for i from 2 to (- num 1)
			do (if (and (check-prime i) (check-prime (/ num i)) (eql (mod num i) 0) (eql (mod num (/ num i)) 0)) (setf semiprime t))
		)
	semiprime
	))

;;Calculate all prime and semi prime numbers which in that boundries
(defun calculate-prime-and-semiprime(boundries)
	;;I want to clear my file whenever user try to run program, hence I open file with supersede mode just for clear it
	(with-open-file (output "primedistribution.txt" :direction :output :if-exists :supersede))
	(if (or (equal boundries nil) (equal (length boundries) 1)) (return-from calculate-prime-and-semiprime 0))
	(with-open-file (output "primedistribution.txt"
                        :direction :output
                        :if-exists :append)
		(let ((count 0))
			
			(loop for i from (car boundries) to (car (cdr boundries))
				do  (if (check-semiprime i) (progn (if (not (equal count 0)) (format output "~%")) (progn (incf count) (write-file output "is Semi-prime" i))))
					(if (check-prime i) (progn (if (not (equal count 0)) (format output "~%")) (progn (incf count) (write-file output "is Prime" i))))
					
			) 
		)
	)
)
(calculate-prime-and-semiprime (read-file "boundries.txt"))