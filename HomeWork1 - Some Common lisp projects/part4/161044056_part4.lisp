
;;This function read file character by character and construct a hashmap
;Which its key contains characters and values frequency of that character
(defun read-to-hashmap(filename)
	(with-open-file (stream filename)
		(let ((hashtable (make-hash-table :test 'equal)))
			(loop for file-char = (read-char stream nil)
				while file-char
				do (incf (gethash file-char hashtable 0))
			)
			hashtable
		)
	)
)

;;With this function I convert my hashtable to a list
;; Because I will use list for my Tree form
(defun convert-hashtable-to-list(hashtable)
	(let ((mylist '()))
		(with-hash-table-iterator (next-entry hashtable)
			(loop (multiple-value-bind (more key value) (next-entry)
	            (unless more (return nil))
	        	(push  (list (list (list key) value)) mylist)
	        ))
		)
	(reverse mylist)	
	)
)



;;Helper function for compare elements by their frequency
;; I use this function for sorting mylist
(defun compare-by-freq(a b)
	(< (nth 1 (car a)) (nth 1 (car b)))
)


;;This is the main function for creating a huffman tree
;;This function merge 2 smallest freq node and so on, while
;; length > 1. If legth = 1 than it means there is just one tree
;; and its our huffman tree, return it
(defun create-huffman-tree(mylist)
	;First check list empty or not because file may have been empty, if list nil than file
	;empty just return dont crerate a huffman tree
	(if (not (equal mylist nil)) 
		(if (equal (length mylist) 1)
			(car mylist)
			(create-huffman-tree (sort (cons (merge-two-node (nth 0 mylist) (nth 1 mylist)) (cdr (cdr mylist))) 'compare-by-freq))
		)	
	)	
)

;; Merge 2 node Then return a new node with 3 element
;; First element is internal merge node, second one left child a
;; and third one is right child b
(defun merge-two-node(a b)

	(let ((internal-node '()))
		(push (list (append (car (car a)) (car (car b))) (+ (second (car a)) (second (car b)))) internal-node)
		(push a internal-node)
		(push b internal-node)

		(reverse internal-node)
	)
)

;; Its basic tree search
;; Search given char in my huffman tree 
;; While searching add templist For left side -> 0 for right side -> 1
;; After search complete return templist, templist now containe Huffman code of given character
(defun find-huffman-code(mychar mylist templist stream)
	
	(if (equal (length (car (car mylist))) 1)
		(if (equal (car (car (car mylist))) mychar) (write-huffman-code-file stream (reverse templist)))
		(progn
			(find-huffman-code mychar (second mylist) (cons 0 templist) stream)
			(find-huffman-code mychar (third mylist) (cons 1 templist) stream)
		)
	)
	
)

;; Write huffman code to file
(defun write-huffman-code-file(stream templist)
	(if (equal templist nil) (format stream "0")) ;;if file only has 1 character
	(loop for i from 0 to (- (length templist) 1)
		do (if (not (equal i (- (length templist) 1))) (format stream "~A " (nth i templist)) (format stream "~A" (nth i templist)))
	)
)

;;Driver code
(defun test-huffman(open-file-name write-file-name)
	;;Whenever User start program I want to clear my file, Hence I open file 2 time,
	;;first with supersede mod for clearing then with append mode for writing
	(with-open-file (output write-file-name :direction :output :if-exists :supersede))

	(with-open-file (output write-file-name
						:direction :output
                        :if-exists :append)
		(let ((huffman-tree (create-huffman-tree (sort (convert-hashtable-to-list (read-to-hashmap open-file-name)) 'compare-by-freq))) (count 0))
			(dolist (l (reverse (sort (convert-hashtable-to-list (read-to-hashmap open-file-name)) 'compare-by-freq)))
				(if (not (equal count 0)) (format output "~%"))
				(incf count)	
				(cond ((equal (car (car (car l))) #\Space) (format output "Space: "))
	        	((equal (car (car (car l))) #\Newline) (format output "Newline: "))
	        	(t (format output "~A: "(car (car (car l))))))

				(find-huffman-code (car (car (car l))) huffman-tree '() output)
			)
		)
	)
)


(test-huffman "paragraph.txt" "huffman_codes.txt")
