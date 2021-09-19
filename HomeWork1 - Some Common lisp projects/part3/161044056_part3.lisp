
;; Read integers from file
(defun read-integers(filename)
	(with-open-file (stream filename)
		(loop for elem = (read stream nil)
			while elem
			collect elem
		)
	)
)	

;;Write file
(defun write-file(output num)
		(format output " ~A" num)
	)

;; Calculate collatz seuqneces recurively
(defun collatz-sequence(output num)
	(if (not (eql num 1))
		(if (eql (mod num 2) 0) (progn (write-file output (/ num 2)) (collatz-sequence output (/ num 2))) (progn (write-file output (+ (* num 3) 1)) (collatz-sequence output (+ (* num 3) 1))))
	))

;;Main loop call collatz-sequence for max 5 integer which read in file
(defun wrapper-collatz(mylist)
	;;I want to clear my file whenever user try to run program, hence I open file with supersede mode just for clear it
	(with-open-file (output "collatz_outputs.txt"
                        :direction :output
                        :if-exists :supersede))

	(with-open-file (output "collatz_outputs.txt" 
						:direction :output
                        :if-exists :append)
	(let ((counter 0)) 
		(dolist (l mylist)
			(format output "~A: ~A"l l)
     		(collatz-sequence output l)
     		(setf counter (+ counter 1))
			(if (eql counter 5) (return))
			(format output "~%"))
		))
)

(wrapper-collatz (read-integers "integer_inputs.txt"))