; Task 1:
(define (sum-in-range a b)
  (define (helper i b sum)
    (if (= i b)
        sum
        (helper (+ i 1) b (+ i sum))
     )
  )
  (helper a b 0) 
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

; Task 4 & 5:
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

; Task 6:
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

; Task 7:
(define (perfect? n)
  (= (divisors-sum n) (* 2 n)))

; Task 8:
(define (prime? n)
  (define (helper i n)
    (cond ((= n 0) #f)
          ((= n 1) #f)
          ((= n 2) #t)
          ((even? n) #f)
          ((= 0 (remainder n i)) #f)
          ((> i (/ n 2)) #t)
          (else (helper (+ i 2) n))
    )
  )
  (helper 3 n)
)

; Task 9:
(define (increasing? n)
  (define (helper i n)
    (cond ((= n 0) #t)
          ((<= i (last-digit n)) #f)
          (else (helper (last-digit n) (quotient n 10)))
    )
  )
  (helper (last-digit n) (quotient n 10))
)

; Task 10
(define (toBinary n)
  (if (= n 0)
      0
      (+ (remainder n 2) (* 10 (toBinary (quotient n 2)))))
)

; Task 10 - iterative
(define (toBinary* n)
  ; Invariant: pos, index of current bit of the resul
  ; which we will get before n/2
  (define (helper n res pos)
    (if (= n 0) res
        (helper (quotient n 2)
                (+ res (* (remainder n 2) (expt 10 pos)))
                (+ pos 1)))
  )
  (helper n 0 0)
)

; Task 11
(define (toDecimal n)
  (define (helper n pos)
    (cond ((= n 0) 0)
          ((= (remainder n 2) 1) (+ (expt 2 pos) (helper (quotient n 10) (+ pos 1))))
          (else (helper (quotient n 10) (+ pos 1)))
    )
  )
  (helper n 0)
)