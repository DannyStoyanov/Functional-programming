; nv - neutral value
(define (accumulate op nv a b term next)
  (if ( > a b) nv
      (op (term a) (accumulate op nv (next a) b term next))))

(define (id x) x)
(define (1+ x) (+ x 1))

(define (sum a b)
  (accumulate + 0 a b id 1+))

; Task 1:
(define (!! n)
  (accumulate * 1
              (if (odd? n) 1 2) n
              id (lambda (x) (+ x 2))))

(define (prime? n) #f) ;to-do

; Count the number of numbers in
; [a, b] the predicate p? is true
(define (count p? a b)
  (accumulate + 0
              a b
              (lambda (x) (if (p? x) 1 0)) 1+))

; Task 2:
(define (fact n)
  (accumulate * 1
              1 n
              id 1+))
(define (nchk n k)
  (/ (fact n) (* (fact (- n k) (fact k)))))

; Task 3:
; *nchk n k) = n*(n-1)...*(n-(k-1))/k*(k-1)..*1
(define (nchk* n k)
  (accumulate * 1
              0 (- k 1)
              (lambda (i) (/ (- n i) (- k i))) 1+))

; Task 4:
(define (2^ n)
  (accumulate * 1
              1 n
              (lambda (x) 2) 1+))

(define (2^* n)
  (accumulate + 0
              0 n
              (lambda (k) (nchk* n k)) 1+))
  
; Task 5:
(define (all? p? a b)
;  (accumulate (lambda (x y) (and x y)) #t
;              a b
;              p?
;              1+)
  (if (> a b) #t
      (and (p? a)
           (all? p? (+ a 1) b)))
)
(define (complement p?)
  (lambda (x) (not (p? x))))
(define (any? p? a b)
  (not (all? (complement p?) a b)))
; 1 2 3 4 5
;
; (and (p? 1) (and (p? 2) (and (p? 3) ...)))

(define (filter-accum p? op nv a b term next)
  (cond ((> a b) nv)
        ((p? a) (op (term a)
                    (filter-accum p? op nv (next a) b term next)))
        (else       (filter-accum p? op nv (next a) b term next))))


(define (!!* n)
  (filter-accum (if (even?) even? odd?)
                * 1
                1 n
                id 1+))

; Task 6:
(define (divisors-sum n)
  (filter-acum (lambda (x) (zero? (remainder n x)))
               + 0
               1 n
               id 1+))

(define (divisors-sum* n)
  (accumulate + 0
              1 n
              (lambda (x) (if (zero? (remainder n x)) x 0)) 1+))

; Task 8:
(define (prime? n)
  (and (> n 1)
  (zero? (count (lambda (x) (zero? (remainder n x))) 2 (sqrt n)))))

(define (prime?* n)
  (and (> n 1)
       (not (any? (lambda (x) (zero? (remainder n x)))
                  2
                  (sqrt( n))))))

; Task 9:
(define (compose f g)
  (lambda (x) (f (g x))))
(define (repeat n f)
  (accumulate compose id
              1 n
              (lambda (x) f) 1+))

