; Task 1:
(define (toBinary n)
  (if (= n 0)
      0
      (+ (remainder n 2) (* 10 (toBinary (quotient n 2))))
  )
)

; iterative
(define (toBinary* n)
  ; Invariant: pos, index of current bit of the resul
  ; which we will get before n/2
  (define (helper n res pos)
    (if (= n 0) res
        (helper (quotient n 2)
                (+ res (* (remainder n 2) (expt 10 pos)))
                (+ pos 1))
        )
  )
  (helper n 0 0)
)
   
; Task 2:
(define (toDecimal n)
  (define (helper n pos)
    (cond ((= n 0) 0)
          ((= (remainder n 2) 1) (+ (expt 2 pos) (helper (quotient n 10) (+ pos 1))))
          (else (helper (quotient n 10) (+ pos 1)))
    )
  )
  (helper n 0)
)

; Task 3:
(define (constantly c)
  (lambda (x) c))

; Task 4:
(define (flip f)
  (lambda (x y) (f y x)))

; Task 5:
(define (complement p?)
  (lambda (x) (not (p? x))))

; Task 6:
(define (compose f g)
  (lambda (x) (f (g x))))

(define ^2 (lambda (x) (* x x))) ;(define (^2 x) (* x x))
(define 1+ (lambda (x) (+ x 1)))
(define id (lambda (x) (x)))

; Task 7:
(define (repeat n f)
  (if (= n 1) f
      (compose f (repeat ( - n 1) f))
      ;(lambda (x)
      ;  (f ((repeat (- n 1) f) x)))
  )
)

; Task 8:
(define dx 0.01)
(define (derive f)
  (lambda (x) (/ (- (f (+ x dx)) (f x)) dx)))

; Task 9:
(define (derive-n n f)
  (if (= n 1) (derive f)
      ;(= n 0) f
      (derive (derive-n (- n 1) f))
  )
)

(define (derive-n* n f)
  ((repeat n derive) f)) 



