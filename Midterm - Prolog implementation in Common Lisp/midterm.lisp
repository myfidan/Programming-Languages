
(setf allFile nil)

(defun read-prolog-clauses(filename)
	(with-open-file (stream filename)
		(loop for elem = (read stream nil)
			while elem
			collect elem
		)
	)
)

(defun find-quary(clauses)
	(if (equal (length clauses) 0)
		nil
		(if (equal (car (car clauses)) nil)
			(append (list (car clauses))  (find-quary (cdr clauses)))
			(find-quary (cdr clauses))
		)
	)
)

(defun unification(queryParam predicateParam)
	(let ((unificationList '()))
		(if (equal (length queryParam) (length predicateParam))
			(progn ;; Check paramaters one by one 
				(loop for x in queryParam for y in predicateParam
					do
					(cond
						((and (numberp x) (numberp y) (equal x y)) ) ;;if two param is number
						((and (stringp x) (stringp y) (lower-case-p (char x 0)) (lower-case-p (char y 0)) (equal x y)) ) ;; if two param is string param
						((and (stringp x) (stringp y) (lower-case-p (char x 0)) (upper-case-p (char y 0))) (setf unificationList (cons (cons y (cons x '())) unificationList))) ;; query string param , predicate variable
						((and (stringp x) (stringp y) (upper-case-p (char x 0)) (lower-case-p (char y 0)))  (setf unificationList (cons (cons y (cons x '())) unificationList))) ;; query variable, predicate string param
						((and (stringp x) (stringp y) (upper-case-p (char x 0)) (upper-case-p (char y 0))) (setf unificationList (cons (cons y (cons x '())) unificationList))) ;; both vairable
						((and (stringp x) (numberp y) (upper-case-p (char x 0))) (setf unificationList (cons (cons y (cons x '())) unificationList))) ;; query variable , predicate number
						((and (numberp x) (stringp y) (upper-case-p (char y 0))) (setf unificationList (cons (cons y (cons x '())) unificationList))) ;; query number, predicate variable
						(t (return-from unification nil))
					)
				)
				(if (equal (length unificationList) 0) t unificationList)
			)
			nil ;; if lengths of 2 list not equal then just return false
		)
	)
)

(defun find_corresponding_variable(unificationList var)
	(loop for x in unificationList
		do
		(if (equal (car x) var)
			(return-from find_corresponding_variable (car (cdr x)))
		)
	)
	var
)

(defun check_is_fact (query)
	
	(loop for y in allFile
		do
		(if (and (equal (car y) query) (equal (car (cdr y)) nil)) 
			(return-from check_is_fact t)
		)
	)
	(return-from check_is_fact nil)
)

(defun resolution(query x)
	
	(if (> (length x) 0)
		(if (equal (car query) (car (car (car x))))
			(progn ;(print (car x)) 
				(setf unificationList nil)
				(setf unificationList (unification (car (cdr query)) (car (cdr (car (car x))))))
				(if (check_is_fact  query) (return-from resolution t))
				(if (equal unificationList nil) () 
						(if (equal unificationList t) 
							(progn
								
								;(print unificationList)  ;;Here I have variables and full predicate
								;(print  query)
								(loop for y in (car (cdr (car x)))
									do
									
									(return-from resolution (resolution y allFile))
								
								)
							)
							(progn
								
								(loop for y in (car (cdr (car x))) ;; body elemanları tek tek dön
									do
									
									(setf newArguments nil)
									(loop for arguments in (car (cdr y))
										do
										(if (and (stringp arguments) (upper-case-p (char arguments 0)))
											(progn 
												(push (find_corresponding_variable unificationList arguments) newArguments)
											)
											(push arguments newArguments)
										)
									)
									
									(setf y (list (car y) (reverse newArguments)))
									;(print unificationList)
									(setf temp unificationList)
									( setf res_return (resolution y allFile))
									(if (equal res_return nil) (return-from resolution (resolution query (cdr x))))
									;(print unificationList)
									(setf unificationList temp)
									;(print "ceck")
								
								)
								(return-from resolution t)
							)
						)
				)
				(return-from resolution (resolution query (cdr x)))
			)
			(return-from resolution (resolution query (cdr x)))
		)
		(return-from resolution nil)
	)
)

(setf x (read-prolog-clauses "midterm_input.txt"))
(setf allFile x)
;(print x)
;(print "---------")
;(print (find-quary x))
;(print (car (cdr (car (find-quary x)))))
;(print "---------")
(print (resolution (car (cdr (car (find-quary x)))) x))
