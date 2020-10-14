; Task 1:
(define (divisors-sum n)
  (define (helper i sum)
    (cond ((> i n) sum)
          ((= 0 (remainder n i))
           (helper (+ i 1) (+ sum i)))
          (else (helper (+ i 1) sum))))
  ;(if (and (positive? n)
  ;         (integer? n))
  (helper 1 0)
  ;#f)
)
(define (last-digit n)
  (remainder n 10))

; Task 2:
(define (fast-expt x n)
  (define (sq x) (* x x))
  (cond ((= n 0) 1)
        ((even? n) (sq (fast-exp x (quotient n 2))))
        (else (* x (sq (fast-expt x (quotient n 2))))))
)

; Task 3:
(define (count-digit d n)
  (define (helper newN count)
    (cond ((= newN 0) count)
          ((= d (last-digit newN))
             (helper (quotient newN 10) (+ count 1)))
          (else (helper (quotient newN 10) count))))
  (if (= n 0) (if (= d 0) 1 0)
     (helper n 0))
)



; ------
(define (perfect? n)
  (= (divisors-sum n) (* 2 n)))

; Task 4:
(define (palindrome? n)
  (= (reverse-int n)))
(define (reverse-int n)
  (define (helper newN res)
    (if (= newN 0) res
        (helper (quotient newN 10)
                (+ (* res 10) (last-digit newN))))
   )
  (helper n 0)
)