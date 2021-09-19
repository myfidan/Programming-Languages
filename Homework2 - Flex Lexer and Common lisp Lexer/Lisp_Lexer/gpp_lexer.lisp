
(defvar prev_token nil)
(defvar dbl_star_check 0)
(defvar quotes 0)

(defvar operators (list "+" "-" "/" "*" "(" ")" "**" "\"" ","))
(defvar keywords (list "and" "or" "not" "equal" "less" "nil" "list"
	"append" "concat" "set" "deffun" "for" "if" "exit" "load" "disp" "true" "false"))


(defun lexer(lexer-word output)
	(setf lexer-word (string-downcase lexer-word))
	(let ((i 0) (iden-array (make-array 2 :element-type 'character :adjustable t :fill-pointer 0)))
		(loop for c across lexer-word
			do 
			 ;; This part for separete * and **
			(if (equal 1 (in-list c operators)) 
				(progn 
					(if (> (length iden-array) 0) (check-kw-id iden-array output))
					(setf (fill-pointer iden-array) 0) ;; reset vector
					(if (equal c #\*) 
						(if (and (< i (- (length lexer-word) 1)) (equal (char lexer-word (+ i 1)) #\*)) (check-operator (string c) #\* output) (check-operator (string c) nil output)) 	
						(check-operator (string c) nil output))
				)
				(if (or (and (< (char-code c) 58) (> (char-code c) 47)) (and (< (char-code c) 123) (> (char-code c) 96)) (equal (char-code c) 95))
					(vector-push-extend c iden-array)
					(progn
						(if (> (length iden-array) 0) 
							(progn (check-kw-id iden-array output) (setf (fill-pointer iden-array) 0))
						)
						(check-error-char c output)
					)
				)
				
			)
			(incf i)
		)
		(if (> (length iden-array) 0) (check-kw-id iden-array output))
	)
)

(defun check-error-char(c output)
	(format output "ERROR ~A cannot be tokenized~%" c)
)

(defun in-list(elem my-list)
	(dolist (l my-list)
		(if (equal (string elem) l) (return-from in-list 1))
	)
	0
)

(defun is-keyword (elem)
	(dolist (l keywords)
		(if (equal elem l) (return-from is-keyword t))
	)
	(not t)
)

(defun check-kw-id(check-string output)
	(if (is-keyword check-string) 
		(format output "KW_~A~%" (string-upcase check-string)) 
		(progn 
			(let ((is-value 0))
				(loop for c across check-string
					do
					(if (not (digit-char-p c)) (setf is-value 1))	
				)
				(if (equal is-value 1) (progn (if (digit-char-p (char check-string 0)) (format output "ERROR ~A cannot be tokenized~%" check-string) (format output "IDENTIFIER~%")) ) (progn (if (and (> (length check-string) 1) (equal (char check-string 0) #\0))(format output "ERROR ~A cannot be tokenized~%" check-string) (format output "VALUE~%")) ) )	
			)
		)
	)
)

(defun check-operator(c next output)
	(if (equal next #\*) (progn (format output "~A~%" "OP_DBLMULT") (incf dbl_star_check) (return-from check-operator 1)))

	(cond ((equal c "+") (format output "~A~%" "OP_PLUS"))
         ((equal c "-") (format output "~A~%" "OP_MINUS"))
         ((equal c "/") (format output "~A~%" "OP_DIV"))
         ((equal c "*") (if (equal dbl_star_check 1) (decf dbl_star_check) (format output "~A~%" "OP_MULT")))
         ((equal c "(") (format output "~A~%" "OP_OP"))
         ((equal c ")") (format output "~A~%" "OP_CP"))
         ((equal c "\"") (if (equal (mod quotes 2) 0) (progn (format output "~A~%" "OP_OC") (incf quotes)) (progn (format output "~A~%" "OP_CC") (incf quotes)))) 
         ((equal c ",") (format output "~A~%" "OP_COMMA"))
         (t (format output "~A~%" "ERROR")))
)

(defun check-comment (comment-arr)
	(if (and (> (length comment-arr) 1) (equal (char comment-arr (- (length comment-arr) 1)) #\;) (equal (char comment-arr (- (length comment-arr) 2)) #\;)) (return-from check-comment 1) (return-from check-comment 0))
)


(defun split-words(words output)
	(let ((string-array (make-array 2 :element-type 'character :adjustable t :fill-pointer 0)) (comment 0))
		(loop for c across words
			do
			(if (and (equal c #\Newline) (equal comment 1)) (setf comment 0))
			(if (and (not (equal c #\Space)) (not (equal c #\Newline)) (not (equal c #\tab)) (not (equal (check-comment string-array) 1)))  
				(if (equal comment 0) (vector-push-extend c string-array)) 
				(if (equal comment 0)
					(progn 
					(if (equal (check-comment string-array) 1) (progn (if (> (length string-array) 2) (lexer (subseq string-array 0 (- (length string-array) 2)) output)) (format output "COMMENT~%") (progn (setf comment 1) (setf (fill-pointer string-array) 0)))) ;; (list 1 2);;this is a comment bunu çöz
					(progn (if (> (length string-array) 0) (progn (lexer string-array output) (setf (fill-pointer string-array) 0)))) 
				))
			)
		)
		
		(if (equal comment 0) 
			(progn 
			(if (equal (check-comment string-array) 1) (progn (if (> (length string-array) 2) (lexer (subseq string-array 0 (- (length string-array) 2)) output)) (format output "COMMENT~%") (return-from split-words 1)))
			(if (> (length string-array) 0) (progn (lexer string-array output) (setf (fill-pointer string-array) 0)))
		))
		;;(print string-array)
	)
)

(defun file-mode(filename output)
	
	(with-open-file (stream (car filename))
	    (let ((contents (make-string (file-length stream))))
	      (read-sequence contents stream)
	      contents
	      	(setf prev_token nil)
	    	(split-words contents output)
	    )
    )
)

(defun interpreter-mode(output)
	
		(setq readstring nil)
		(loop while(not (equal readstring ""))
			do  (format t "> ")
				(setq readstring (read-line))
				(setq comment-check (search ";;" readstring))
				(if (not (equal comment-check nil)) (setf readstring (subseq readstring 0 (+ comment-check 2))))
	      		(setf prev_token nil)
				(split-words readstring output)
		)
)

(defun gppinterpreter(&rest filename)
	(with-open-file (output "parsed_lisp.txt" :direction :output :if-exists :supersede)
		(if (equal filename nil) (interpreter-mode output) (file-mode filename output))
	)
)

(defun run-lexar()
		
		(if (equal (car *args*) nil) 
			(gppinterpreter)
			(gppinterpreter (car *args*))
		)
	
)

(run-lexar)