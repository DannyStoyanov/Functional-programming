; Task 2:
; Даниел Здравков Стоянов 45574

(define (toBinary n) ; decimal -> binary
  (if (= n 0)
      0
      (+ (remainder n 2) (* 10 (toBinary (quotient n 2))))
  )
)
(define (toDecimal n) ; binary -> decimal
  (define (helper n pos)
    (cond ((= n 0) 0)
          ((= (remainder n 2) 1) (+ (expt 2 pos) (helper (quotient n 10) (+ pos 1))))
          (else (helper (quotient n 10) (+ pos 1)))
    )
  )
  (helper n 0)
)

(define (printSet set) ; takes decimal
  (define n (toBinary set))
  (display "{")
  (define (helper pos n)
    (cond ((= n 0) (begin (display "}")
                            (display "\n")
                            (toBinary set)))
          ((= (remainder n 2) 1) (if (<= n 10)
                                     (begin (display pos)
                                                      (helper (+ pos 1) (quotient n 10)))
                                     (begin (display pos)
                                        (display ", ")
                                        (helper (+ pos 1) (quotient n 10))))) 
          ((= (remainder n 2) 1) (begin (display pos)
                                        (display ", ")
                                        (helper (+ pos 1) (quotient n 10))))
          (else (helper (+ pos 1) (quotient n 10)))
          )
    )
  (helper 0 n)
  )
          

; (set-add set elem)
(define (set-add* set elem) ; takes decimal
  (define n (toBinary set))
  ;(display n)
  (define (helper pos n)
    (cond ((= n 0) 0)
          ((= pos elem) (+ 1 (* 10 (helper (+ pos 1) (quotient n 10)))))
          (else (+ (remainder n 2) (* 10 (helper (+ pos 1) (quotient n 10)))))
          )
    )
  (helper 0 n)
  )

(define (set-add set elem)
  (define x (set-add* set elem))
  ;(printSet (toDecimal x))
  (toDecimal x)
  )
  

;(set-remove set elem)
(define (set-remove* set elem) ; takes decimal
  (define n (toBinary set))
  ;(display n)
  (define (helper pos n)
    (cond ((= n 0) 0)
          ((= pos elem) (+ 0 (* 10 (helper (+ pos 1) (quotient n 10)))))
          (else (+ (remainder n 2) (* 10 (helper (+ pos 1) (quotient n 10)))))
          )
    )
  (helper 0 n)
  )

(define (set-remove set elem)
  (define x (set-remove* set elem))
  ;(printSet (toDecimal x))
  (toDecimal x)
  )

;(set-contains? set elem)
(define (set-contains? set elem) ; takes decimal
  ;(printSet set)
  (define n (toBinary set))
  ;(display n)
  (define (helper pos n)
    (cond ((= n 0) #f)
          ((= pos elem) (if (= (remainder n 2) 1) #t #f))
          (else (helper (+ pos 1) (quotient n 10)))
          )
    )
  (helper 0 n)
  )


;(set-empty? set)
(define (set-empty? set) ; takes decimal
  ;(printSet set)
  (define n (toBinary set))
  ;(display n)
  (define (helper pos n)
    (cond ((= n 0) #t)
          ((= (remainder n 2) 1) #f)
          (else (helper (+ pos 1) (quotient n 10)))
          )
    )
  (helper 0 n)
  )

;(set-size set)
(define (set-size set) ; takes decimal
  ;(printSet set)
  (define n (toBinary set))
  ;(display n)
  (define (helper pos n sum)
    (cond ((= n 0) sum)
          ((= (remainder n 2) 1) (helper (+ pos 1) (quotient n 10) (+ sum 1)))
          (else (helper (+ pos 1) (quotient n 10) sum))
          )
    )
  (helper 0 n 0)
  )

;(set-intersect s1 s2)
(define (set-intersect* s1 s2) ; takes decimal
  ;(printSet s1)
  ;(printSet s2)
  (define n1 (toBinary s1))
  (define n2 (toBinary s2))
  (define (helper n1 n2)
    (cond ((or (= n1 0) (= n2 0)) 0)
          ((and (= (remainder n1 2) 1) (= (remainder n2 2) 1))
           (+ 1 (* 10 (helper (quotient n1 10) (quotient n2 10)))))
          (else (* 10 (helper (quotient n1 10) (quotient n2 10))))
          )
    )
  (helper n1 n2)
  )

(define (set-intersect s1 s2)
  (define x (set-intersect* s1 s2))
  ;(printSet (toDecimal x))
  (toDecimal x)
  )

;(set-union s1 s2)
(define (set-union* s1 s2)
  ;(printSet s1)
  ;(printSet s2)
  (define n1 (toBinary s1))
  (define n2 (toBinary s2))
  (define (helper n1 n2)
    (cond ((and (= n1 0) (= n2 0)) 0)
          ((or (= (remainder n1 2) 1) (= (remainder n2 2) 1))
           (+ 1 (* 10 (helper (quotient n1 10) (quotient n2 10)))))
          (else (* 10 (helper (quotient n1 10) (quotient n2 10))))
          )
    )
  (helper n1 n2)
  )

 (define (set-union s1 s2)
  (define x (set-union* s1 s2))
  ;(printSet (toDecimal x))
  (toDecimal x)
  )

;(set-difference s1 s2)
 (define (set-difference* s1 s2)
  ;(printSet s1)
  ;(printSet s2)
  (define n1 (toBinary s1))
  (define n2 (toBinary s2))
   (define (helper n1 n2)
     (cond ((= n1 0) 0)
           ((and (= (remainder n1 2) 1) (= (remainder n2 2) 0))
            (+ 1 (* 10 (helper (quotient n1 10) (quotient n2 10)))))
           (else (* 10 (helper (quotient n1 10) (quotient n2 10))))
           )
     )
   (helper n1 n2)
   )
 
  (define (set-difference s1 s2)
  (define x (set-difference* s1 s2))
  ;(printSet (toDecimal x))
  (toDecimal x)
  )

;(knapsack c n w p)











 