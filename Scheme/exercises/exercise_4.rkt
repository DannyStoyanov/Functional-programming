; other stuff:

; tuple:
;(define p (cons 4 "some_string"))
; (car p) -> 1st element
; (cdr p) -> 2nd element

; list:

; empty list '()

; 2 3 5 as a list:
; cons
; 2 cons
;   3 cons
;     5 '()

(define l1 (cons 2 ( cons 3 (cons 5 '()))))
(define l2 '(2 3 5))
(define l3 (list (+ 2 0) 3 5))

;(define (length lst)
;  (if (null? lst) 0
;      (+ 1 (length (cdr lst)))))

(define head car)
(define tail cdr)

; Task 0:
(define (member? x lst)
  (cond ((null? lst) #f)
        ((= (head lst) x) #t)
  (else (member? x (tail list)))))
  ;(and (not (null? x lst))
  ;     (or (= head lst) x)
  ;     (member? x (tail lst))))

;(define (map f lst)
;  (if (null? lst) '()
;      (cons (f (head lst))
;            (map f (tail lst)))))
(define (map* f lst)
  (define (helper lst res)
    (if (null? lst) res
        (helper (tail lst)
                ;(cons (f (head lst)) res)
                (append res (list (f (head lst))))
                )))
  (helper lst '())
  )
; helper '(1 2 3) '()
; helper '(2 3) '(1)
; helper '(3) '(2 1)

;(map even? '(1 2 3 4 5))
;(map length '(1 2 3))

(define (filter p? lst)
  (cond ((null? lst) '())
        ((p? (head lst)) (cons (head lst)
                               (filter p? (tail lst))))
        (else (filter p? (tail lst)))))

(define (length* lst)
  (define (helper lst res)
    (if (null? lst) res
        (helper (tail lst) (+ res 1))))
  (helper lst 0)
  )

; Task 1:
(define (take n lst) ; reverse order
  (define (helper n lst res)
    (if (or (null? lst) (= n 0)) res
         (helper (- n 1) (tail lst) (cons (head lst) res))))
    (helper n lst '())
  ) 
; 2nd version:
(define (take* n lst)
  (if (or (= n 0) (null? lst))
      '()
      (cons (head lst)
            (take* (- n 1) (tail lst)))))

; Task 2:
(define (drop n lst)
  (if (or (= n 0) (null? lst))
      lst
      (drop (- n 1) (tail lst))))

; Task 3:
;(define (zip lst1 lst2)
;  (if (or (null? lst1) (null? lst2))
;      '()
;      (cons (cons (head lst1) (head lst2))
;            (zip (tail lst1) (tail lst2)))))

; Task 4:
(define (zipWith f lst1 lst2)
  (if (or (null? lst1) (null? lst2))
      '()
      (cons (f (head lst1) (head lst2))
            (zipWith f (tail lst1) (tail lst2)))))

(define (zip lst1 lst2)
  (zipWith cons lst1 lst2))

; Task 5:
(define (sorted? lst)
  (if (or
       (null? lst)
       (null? (tail lst)))
      #t
      (and (<= (head lst) (head (tail lst)))
           (sorted? (tail lst)))))

; if we need length in function:
(define (something lst)
  (define (helper lst n) ; (length lst) = n
    (...))
  (helper lst (length lst)))




















