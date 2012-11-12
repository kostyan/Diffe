;; the prototype function mydiff, release you can see down
(defun mydif (l x) ())

;; these functions differentiation l-expression for x
(defun dif* (l x)
	;; resultate is: (+ (* a' b) (* a b'))
	(list 
		(list (mydif (first l) x) '* (third l))
		 '+ 
		(list (mydif (third l) x) '* (first l))	
	)
)
 
(defun dif+ (l x)
	;; resultate is: (+ a' b')
	(list (mydif (first l) x) '+ (mydif (third l) x))
)

(defun dif- (l x)
	;; resultate is: (- a' b')
	(list (mydif (first l) x) '- (mydif (third l) x))
)

(defun dif/ (l x)
	; resultate is: (/ (- (* a' b) (* a b')) (^ b 2))
	(list
		(list
			(list (mydif (first l) x) '* (third l) )
			'-
			(list (mydif (third l) x) '* (first l) )
		)
		'/
		(list (third l) '^ 2)
	)
)

(defun difln (l x)
	;; resultate is: (/ 1 a')
	(list (mydif (first l) x) '/ l)
)

(defun difsin (l x)
	;; resultate is: (* a' (cos a))
	(list (mydif (first l) x) '* (append '(cos)  l))
)

(defun difcos (l x)
	;; resultate is: (* a' (- sin a))
	(list (mydif (first l) x) '* (list '- (append '(sin)  l)))
)

(defun diftg (l x)
	(list (mydif (first l) x) '/ (list '^ (append '(cos)  l) 2))
)

(defun difctg (l x)
	(list ( list '- (mydif (first l) x)) '/ (list '^ (append '(sin)  l) 2))
)

(defun difasin (l x)
	(list (mydif (first l) x) '/ (list '^ (list '- 1 (list '^ l 2)) (list '/ 1 2) ))
)

(defun difacos (l x)
	(list (list '- (mydif (first l) x)) '/ (list '^ (list '- 1 (list '^ l 2)) (list '/ 1 2) ))
)

(defun difatg (l x)
	(list (mydif (first l) x) '/ (list '+ 1 (list '^ l 2)) )
)

(defun difactg (l x)
	(list (list '- (mydif (first l) x)) '/ (list '+ 1 (list '^ l 2)) )
)

(defun dif^ (l x)
	(list (list (first l) '^ (third l)) '* 
		(mydif (list (third l) '* (list 'ln (first l))) x)
	)
)

;; this function any simplify l-expression
(defun simplify(l)
	(cond
		((atom l) l)
		((eql (second l) '*) 
			(cond
				((eql (first l) 0) 0)
				((eql (third l) 0) 0)
				((eql 1 (first l)) (simplify (third l)))
				((eql 1 (third l)) (simplify (first l)))
				((and (numberp (first l)) (numberp (third l))) (* (first l) (third l)))
				(t (list (simplify (first l)) '* (simplify (third l))))
			)
		)
		((eql (second l) '+)
			(cond
				((eql (first l) 0) (simplify (third l)))
				((eql (third l) 0) (simplify (first l)))
				((and (numberp (first l)) (numberp (third l))) (+ (first l) (third l)))
				(t (list (simplify (first l)) '+ (simplify (third l)))	)
			)
		)
		((eql (second l) '-)
			(cond
				((eql (third l) 0) (simplify (first l)))
				((eql (first l) (third l)) 0)
				((and (numberp (first l)) (numberp (third l))) (- (first l) (third l)))
				(t 
					(cond
						((null (third l)) (list '- (simplify (first l))) )
						(t (list (simplify (first l)) '- (simplify (third l))))
					)
				)
			)
		)
		((eql (second l) '/)
			(cond
				((eql (first l) 0) 0)
				((eql (first l) (third l)) 1)
				((and (numberp (first l)) (numberp (third l))) (/ (first l) (third l)))
				(t (list (simplify (first l)) '/ (simplify (third l))))
			)
		)
		(t l)
	)
)


;; main function for differentiation l-expression for x
(defun mydif (l x)
	(simplify (simplify
		(cond 
			((atom l) (if (eq l x) 1 0))
			((null (second l)) (first l))
			((eq (second l) '+) (dif+ l x))
			((eq (second l) '*) (dif* l x))
			((eq (second l) '-) (dif- l x))
			((eq (second l) '/) (dif/ l x))
			((eq (first l) 'ln) (difln (cdr l) x))
			((eq (first l) 'sin) (difsin (cdr l) x))
			((eq (first l) 'cos) (difcos (cdr l) x))
			((eq (first l) 'tg) (diftg (cdr l) x))
			((eq (first l) 'ctg) (difctg (cdr l) x))
			((eq (first l) 'asin) (difasin (cdr l) x))
			((eq (first l) 'acos) (difacos (cdr l) x))
			((eq (first l) 'atg) (difatg (cdr l) x))
			((eq (first l) 'actg) (difactg (cdr l) x))
			((eq (second l) '^) (dif^ l x))
			(t '(illegal operation))
		)
	))
)
