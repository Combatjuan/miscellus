;-------------------------------------------------------------------------------
; Generally useful things.
;-------------------------------------------------------------------------------
; Because I'm lazy.
(define (p ...) (displayln ...))

; racket apparently doesn't define nil.
; So we can fill in code portions from the book, let's create analogous defines.
(define nil '())
; Used often in examples.  Probably called something else in racket.
(define (square x) (* x x))

; From 2.33, but used elsewhere.
(p "Enumerate")
(define (enumerate-interval from to)
	(if (> from to)
		nil
		(cons from (enumerate-interval (+ 1 from) to))))
(p (enumerate-interval 1 10))

(p "Filter")
(define (filter predicate sequence)
	(cond
		((null? sequence)
			nil)
		((predicate (car sequence))
			(cons (car sequence) (filter predicate (cdr sequence))))
		(else
			(filter predicate (cdr sequence)))))
(p (filter odd? (enumerate-interval 1 10)))

(p "Accumulate")
(define (accumulate operation identity sequence)
	(if (null? sequence)
		identity
		(operation (car sequence) (accumulate operation identity (cdr sequence)))))
(p (accumulate * 1 (enumerate-interval 1 5)))


;-------------------------------------------------------------------------------
(p "----------------------------------------")
(p "Excercise 2.17")
(define (last-pair l)
	(if (null? (cdr l))
		(car l)
		(last-pair (cdr l))
	)
)
(p (last-pair (list 1 2 3)))

;-------------------------------------------------------------------------------
(p "----------------------------------------")
(p "Exercise 2.18")
(define (reverse l)
	(if (null? (cdr l))
		(list (car l))
		(append (reverse (cdr l)) (list (car l))) 
	)
)

(p (reverse (list 1 2 3 4 5 6)))

;-------------------------------------------------------------------------------
(p "----------------------------------------")
(p "Exercise 2.20")

(define (my-filter l predicate?)
	(if (null? (cdr l))
		(if (predicate? (car l))
			(list (car l))
			(list)
		)
		(if (predicate? (car l))
			(append (list (car l)) (my-filter (cdr l) predicate?))
			(my-filter (cdr l) predicate?)
		)
	)
)
; FIXME:
; Try changing my-filter to 'filter'.
; It must be a built-in but isn't getting overwritten for some reason?

; Syntax highlighting says positive? is already a thing.
; But I'm redefining it here anyway.  In retrospect, not sure why.
(define (positive? n)
	(if (> n 0) true false)
)

(p (my-filter (list 1 -5 0 3 8 -14) positive?))

(define (same-parity n . ns)
	(define (mod2? n) (if (= (modulo n 2) 0) true false))
	(define (nmod2? n) (if (= (modulo n 2) 0) false true))
	(if (mod2? n)
		(append (list n) (my-filter ns mod2?))
		(append (list n) (my-filter ns nmod2?))
	)
)
(p (same-parity 1 2 3 4 5 6 7))
(p (same-parity 2 3 4 5 6 7))


(define (my-filter l predicate?)
	(if (null? (cdr l))
		(if (predicate? (car l))
			(list (car l))
			(list)
		)
		(if (predicate? (car l))
			(append (list (car l)) (my-filter (cdr l) predicate?))
			(my-filter (cdr l) predicate?)
		)
	)
)

;-------------------------------------------------------------------------------
(p "----------------------------------------")
(p "Exercise 2.25")

(p (car (cdr (car (cdr (cdr (list 1 3 (list 5 7) 9)))))))
(p (car (car (list (list 7)))))
(p (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))))))))))))))


;-------------------------------------------------------------------------------
(p "----------------------------------------")
(p "Exercise 2.27 'Deep Reverse'")
(p "  ((1 2) (3 4)) -> ((4 3) (2 1))")

(define (my-reverse l)
	(if (null? (cdr l))
		(list (car l))
		(append (my-reverse (cdr l)) (list (car l))) 
	)
)

; ([.|.] --------> [ |>])
;   v               v
; ([1|.] > [2|/]) ([3|.] > [4|.] > [5|/])

;!(define (deep-reverse tree) 
;!	(p tree)
;!	(cond
;!		((null? tree) '())
;!		((pair? tree) (append (deep-reverse (cdr tree)) (deep-reverse (car tree))))
;!		(else (list tree))
;!	)
;!)

(define (deep-reverse t) 
	(cond
		((null? t) nil)
		((pair? (car t))
			(append
				(deep-reverse (cdr t))
				(list (deep-reverse (car t)))))
		(else
			(append
				(deep-reverse (cdr t))
				(list (car t))))))

; [[1 [2 -]] [[3 [4 [5 -]]] -]]
; ->
; [[5 [4 [3 -]]] [[2 [1 -]]]]
(p (deep-reverse '((1 2) (3 4 5))))


;-------------------------------------------------------------------------------
(p "----------------------------------------")
(p "Exercise 2.28 'Fringe'")
(p "  ((1 2) (3 4)) -> (1 2 3 4)")
(define (fringe t)
	(cond
		((null? t) nil)
		((pair? (car t)) (append (fringe (car t)) (fringe (cdr t))))
		(else (append (list (car t)) (fringe (cdr t))))))

(p (fringe '((1 2) (3 4))))
(p (fringe '((1 2) (3 (4 5) (6 7 (8 (9)))))))

(define enumerate-tree fringe)


;-------------------------------------------------------------------------------
(p "----------------------------------------")
(p "Exercise 2.29 'Mobile")

;!(define (make-mobile left right)
;!	(list left right))
;!
;!(define (make-branch length structure)
;!	(list length structure))

(define (make-mobile left right)
	(cons left right))

(define (make-branch length structure)
	(cons length structure))

;-------
(define (left-branch m) (car m))

;!(define (right-branch m) (car (cdr m)))
(define (right-branch m) (cdr m))

(define (branch-length b) (car b))

;!(define (branch-structure b) (car (cdr b)))
(define (branch-structure b) (cdr b))

(define (total-weight m)
	(define (branch-weight b)
		(cond
			((null? b) 0)
			((pair? (branch-structure b)) (total-weight (branch-structure b)))
			(else (branch-structure b))))
	(if (pair? m)
		(+ (branch-weight (left-branch m)) (branch-weight (right-branch m)))
		m))

(p "simple mobile:")
(define simple-mobile
	(make-mobile
		(make-branch 10 5)
		(make-branch 2 25)))
(p simple-mobile)
(p (total-weight simple-mobile))

(p "complex mobile:")
(define complex-mobile
	(make-mobile
		(make-branch 16 (make-mobile
			(make-branch 2 3)
			(make-branch 3 2)))
		(make-branch 8 (make-mobile
			(make-branch 2 6)
			(make-branch 3 4)))))
(p complex-mobile)
(p (total-weight complex-mobile))

(p "unbalanced mobile:")
(define unbalanced-mobile
	(make-mobile
		(make-branch 4 (make-mobile
			(make-branch 1 3)
			(make-branch 3 1)))
		(make-branch 2 (make-mobile
			(make-branch 6 1)
			(make-branch 2 3)))))
(p unbalanced-mobile)
(p (total-weight unbalanced-mobile))

(p (total-weight (branch-structure (left-branch simple-mobile))))

(define (balanced? m)
	(define (branch-torque b)
		(* (branch-length b) (total-weight (branch-structure b))))
	(cond
		((not (pair? m)) true)
		(else
			(and
				(= (branch-torque (left-branch m)) (branch-torque (right-branch m)))
				(balanced? (branch-structure (left-branch m)))
				(balanced? (branch-structure (right-branch m)))
			)
		)
	)
)

(p (balanced? simple-mobile))
(p (balanced? complex-mobile))
(p (balanced? unbalanced-mobile))

; Part D
; Very little.  See commented out version.


;-------------------------------------------------------------------------------
(p "----------------------------------------")
(p "Exercise 2.31 'map-tree'")

(define (map-tree f t)
	(cond 
		((null? t) nil)
		((pair? (car t))
			(append
				(list (map-tree f (car t)))
				(map-tree f (cdr t))))
		(else
			(append
				(list (f (car t)))
				(map-tree f (cdr t))))))

(p (map-tree square '((1 2) (3 4 (5) (6 7)))))


;-------------------------------------------------------------------------------
(p "----------------------------------------")
(p "Exercise 2.32 'subsets'")

(define (subsets s)
	(p s)
	(if (null? s)
		(list nil)
		(let ((rest (subsets (cdr s))))
			(append rest (map (lambda (x) (cons (car s) x)) rest)))))

(p (subsets '(1 2 3 4)))
; Was tired.
; Threw stuff against the wall for a while wondering what would stick.  Then thought about it.
; "Obviously what I need here is a function.  And obviously what I need here involves (car s).
; Indeed, I need to combine car s with each of the subsets.
; Could it really be as simple as a function that does that?
; WHOA!  IT WORKED!

;-------------------------------------------------------------------------------
(p "----------------------------------------")
(p "Exercise 2.33 'subsets'")

(define (my-map p sequence)
	(accumulate (lambda (x y) (cons (p x) y) ) nil sequence))

(p (my-map square '(1 2 3)))
; Again.  Win!

(p (enumerate-tree '(1 (2 3))))

(define (my-append a b)
	(accumulate cons nil (fringe (cons a b))))
(p (my-append '(1 2 3) '(4 5 6))) ; '(1 2 3 4 5 6)
; Again.  Win!

(define (my-length sequence)
	(accumulate (lambda (_ x) (+ 1 x)) 0 sequence))
(p (my-length '(1 2 3 4 5)))
; Almost win.  Wrong arity first time.


(p "----------------------------------------")
(p "Exercise 2.34 'Horner")

(define (horner x coefficients)
	(accumulate
		(lambda (this-coefficient higher-terms) (+ this-coefficient (* x higher-terms)))
		0
		coefficients))
(p (horner 2 '(1 2 3))) ; 1*2^0 + 2*2^1 + 3*2^2 = 1 + 4 + 12 = 17
; As a sidenote, it'd be neat to reverse the sequence so it can be presented Ax^2 + Bx + C with sequence '(A B C)
; instead of '(C B A).  Maybe that's how cool mathematicians roll.

; Interesting footnote about proof of Horner's Law being the first proof of optimal algorithms.

(p "----------------------------------------")
(p "Exercise 2.35 'More Leaf Counting")
(define (gotta-count-em-all sequence)
	(accumulate + 0 (map (lambda (x) 1) (fringe sequence))))
(p (gotta-count-em-all '(1 (2 3 (4) (5) (6 7))))) ; 7
; Boom. Headshot.

