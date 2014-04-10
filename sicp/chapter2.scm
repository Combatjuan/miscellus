;-------------------------------------------------------------------------------
; Generally useful things.
;-------------------------------------------------------------------------------
; Because I'm lazy.
(define (p ...) (displayln ...))

(define (header s)
	(fprintf (current-output-port) "~n----------------------------------------~n- ~a~n----------------------------------------~n" s))

; Because it makes output nicer.
(define (check e v)
	(if (eq? (eval e) v)
		(fprintf (current-output-port) "~s -> ~s~n" e v)
		(fprintf (current-output-port) "Error: ~s <> ~s~n" e v)))

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
(header "Excercise 2.17")
(define (last-pair l)
	(if (null? (cdr l))
		(car l)
		(last-pair (cdr l))
	)
)
(p (last-pair (list 1 2 3)))

;-------------------------------------------------------------------------------
;!(define (sum l)
;!	(define (sum-iter l running-toral)
;!		(if (null? (cdr l))
;!			(cons l)
;!			(sum-iter (cdr l) (+ running-total cons(l))))))

;-------------------------------------------------------------------------------
(header "Exercise 2.18")
(define (my-reverse l)
	(if (null? (cdr l))
		(list (car l))
		(append (my-reverse (cdr l)) (list (car l))) 
	)
)

(define (my-reverse2 l)
	(if (null? l)
		nil
		(cons (my-reverse2 (cdr l)) (car l))))

;!(define (my-reverse3 l)
;!	(define (my-reverse3-iter l reversed)
;!		(if (null? (cdr l))
;!			(

(p (my-reverse (list 1 2 3 4 5 6)))
(p (my-reverse2 '(1 2 3 4 5 6 7)))
;!(p (my-reverse3 '(1 2 3 4 5 6 7)))

;-------------------------------------------------------------------------------
(header "Exercise 2.20")

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


;-------------------------------------------------------------------------------
(header "Exercise 2.25")

(p (car (cdr (car (cdr (cdr (list 1 3 (list 5 7) 9)))))))
(p (car (car (list (list 7)))))
(p (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))))))))))))))


;-------------------------------------------------------------------------------
(header "Exercise 2.27 'Deep Reverse'")
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
(header "Exercise 2.28 'Fringe'")
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
(header "Exercise 2.29 'Mobile")
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
(header "Exercise 2.31 'map-tree'")

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
(header "Exercise 2.32 'subsets'")

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
(header "Exercise 2.33 'subsets'")

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


(header "Exercise 2.34 'Horner'")

(define (horner x coefficients)
	(accumulate
		(lambda (this-coefficient higher-terms) (+ this-coefficient (* x higher-terms)))
		0
		coefficients))
(p (horner 2 '(1 2 3))) ; 1*2^0 + 2*2^1 + 3*2^2 = 1 + 4 + 12 = 17
; As a sidenote, it'd be neat to reverse the sequence so it can be presented Ax^2 + Bx + C with sequence '(A B C)
; instead of '(C B A).  Maybe that's how cool mathematicians roll.

; Interesting footnote about proof of Horner's Law being the first proof of optimal algorithms.

(header "Exercise 2.35 'More Leaf Counting'")
(define (gotta-count-em-all sequence)
	(accumulate + 0 (map (lambda (x) 1) (fringe sequence))))
(p (gotta-count-em-all '(1 (2 3 (4) (5) (6 7))))) ; 7
; Boom. Headshot.


(header "Exercise 2.40 'Unique Pairs'")
;       1      2      3     4
;1    -   (1, 2) (1, 3) (1, 4)
;2    -      -   (2, 3) (1, 4)
;3    -      -      -   (1, 4)
;4    -      -      -      -  

(header "Exercise 2.42 'Three Adderands")
; find all (i, j, k) where i != j != k and i < n, j < n, k < n, and i + j + k = s for a provided n and s.

; Start at n.  That's k.  Subract k from s.  Find all i + j = s - k.  Start with j < k, i = 0 and go while i < j.
; n = 6, k = 10
; 6, 4, 0
; 6, 3, 1
; 5, 4, 2

(define (two-adderands m n)
	(define (two-adderands-iter m n i)
		(if (> n i)
			(cons (cons (- m i) i) (two-adderands-iter m n (+ 1 i)))
			(nil)))
	(two-adderands-iter m n 0)
)


;===============================================================================
; 2.3, 2.5
;===============================================================================
(header "Exercise 2.53 Evaluating Quote")

(define (memq item list) ; Returns true if items is in list else false
	(if (null? list)
		false
		(if (eq? item (car list))
			true
			(memq item (cdr list)))))

(p (memq 1 '(1 2 3 4 5)))
(p (memq 6 (list 1 2 3 4 5)))


(p (list 'a 'b 'c)) ; -> (a b c)
(p (list (list 'george))) ; -> ((george))
(p (cdr '((x1 x2) (y1 y2)))) ; -> (y1 y2) ; Nope, cdr of a list is a list with one elemnt.  So ((y1 y2))
(p (cadr '((x1 x2) (y1 y2)))) ; -> (y1 y2)

(p (pair? (car '(a short list)))) ; -> false 
(p (memq 'red '((red shoes) (blue socks)))) ; -> false
(p (memq 'red '(red shoes blue socks))) ; true


(header "Exercise 2.54 Implement equal? (List equality)")

(define (equal? A B)
	(cond
		((and (null? A) (null? B)) true)
		((null? A) false)
		((null? B) false)
		(else (if (eq? (car A) (car B))
			(equal? (cdr A) (cdr B))
			false))))

(p (equal? '(1 2 3) '(1 2 3))) ; true
(p (equal? '(1 2 3) '(1 2 4))) ; false
(p (equal? '(1 2 3) '(1 2))) ; false
(p (equal? '(1 2) '(1 2 3))) ; false
(p (equal? '(5 2) '(1 2))) ; false
(p (equal? '() '())) ; true

(header "Exercise 2.55 Quotable quotes")

(p (car ''abracadabra))
; -> (p (car (quote (quote abracadabra))))
; -> (p (car (quote (quote abracadabra))))
; -> (p (car '(quote abracadabra)))


(header "Symbolic Algebra")

(define (deriv exp var)
	(cond
		((number? exp) 0)
		((variable? exp)
			(if (same-variable? exp var) 1 0))
		((sum? exp)
			(make-sum (deriv (addend exp) var) (deriv (augend exp) var)))
		((product? exp)
			(make-sum 
				(make-product
					(multiplier exp)
					(deriv (multiplicand exp) var))
				(make-product
					(deriv (multiplier exp) var)
					(multiplicand exp))))
		(else (error "Unknown expression type -- DRIV" exp))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2) (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2) (list '+ a1 a2))

(define (make-product m1 m2) (list '* m1 m2))

(define (sum? x)
	(and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (caddr s))

(define (product? x)
	(and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))

; Initial tests
(p (deriv '(+ x 3) 'x))
(p (deriv '(* x y) 'x))
(p (deriv '(* (* x y) (+ x 3)) 'x))

; I typed the above 45 lines exactly correctly.  Woot.

; Now some code to make the output 'simpler'
(define (eq-number? x n) (and (number? x) (eq? x n)))

; Make multiplication by 0 and 1 reasonable.
(define (make-product m1 m2)
	(cond
		((or (eq-number? m1 0) (eq-number? m2 0)) '0)
		((eq-number? m1 1) m2)
		((eq-number? m2 1) m1)
		((and (number? m1) (number? m2)) (* m1 m2))
		(else (list '* m1 m2))))

; Make addition of 0 and constant addition reasonable.
(define (make-sum a1 a2)
	(cond
		((eq-number? a1 0) a2)
		((eq-number? a2 0) a1)
		((and (number? a1) (number? a2)) (+ a1 a2))
		(else (list '+ a1 a2))))

; Better?
(p "New and improved")
(p (deriv '(+ x 3) 'x))
(p (deriv '(* x y) 'x))
(p (deriv '(* (* x y) (+ x 3)) 'x))


(header "Exercise 2.56")

; Add support for exponentiation.
(define (** x n)
	(define (**-iter x n v)
		(if (= n 0) v
			(**-iter x (- n 1) (* x v))))
	(**-iter x n 1))

(check '(+ 2 2) 4)
(check '(+ 2 2) 5)

; Test
(p "Testing exponentiation")
(check '(** 2 10) 1024)
(check '(** 5 0) 1)
(check '(** 3 4) 81)
(check '(** 1000000 1) 1000000 )

(define (exponentiation? x)
	(and (pair? x) (eq? (car x) '**)))

(define (base s) (cadr s))

(define (exponent s) (caddr s))

(define (make-exponent b e) (list '** b e))



