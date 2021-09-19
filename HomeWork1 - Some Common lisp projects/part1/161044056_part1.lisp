
(defvar single-list '())
;;get list by reading first line in txt file
(defun read-file(filename)
	(with-open-file (stream filename)
		(loop for elem = (read stream nil)
			while elem
			collect elem
		)
	)
)

;;recursive convert list to a single list without any nested list
(defun convert-single-list(file-list)
	(if (listp file-list) (if (consp file-list) (progn (convert-single-list (car file-list)) (convert-single-list (rest file-list)))) (push file-list single-list)))


;;Write single list to file
(defun print-single-list-to-file(filename)
	(with-open-file (output filename
                        :direction :output
                        :if-exists :supersede)
		(loop for i from 0 to (- (length single-list) 1)
			do (if (equal i (- (length single-list) 1)) (format output "~A" (nth i (reverse single-list))) (format output "~A " (nth i (reverse single-list))))
		)	
	)
)

;;test program
(defun test(inputfile outputfile)
	(convert-single-list (read-file inputfile))
	
	(print-single-list-to-file outputfile)
	)


(test "nested_list.txt" "flattened_list.txt")