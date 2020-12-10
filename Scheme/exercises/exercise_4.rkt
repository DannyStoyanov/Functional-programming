#lang racket
; Exercise N4

(define head car) ; приема списък от "неща" и връща първото "нещо"
(define tail cdr) ; приема списък от неща и връща също списък от нещата без първото

; Намира дължина на списък:
(define (length lst)
  (if (null? lst) 0
      (+ 1 length (tail lst))))

; Проверява дали елемента х се съдържа в списък:
(define (member x lst)
  (cond [(null? lst) #f] 
        [(equal? (head lst) x) #t]  ; заб.: вградената member тук връща lst
        [else (member x (tail lst))]))

; Task 1:
; Взима първите n елемента на списък:
(define (take n lst)
  (if (or (= n 0) (null? lst))
      '()
      (cons (head lst)
            (take (- n 1) (tail lst)))))

; Премахва първите n елемента на списък:
(define (drop n lst)
  (if (or (= n 0) (null? lst))
      lst
      (drop (- n 1) (tail lst))))

; Task 2:
; Проверява дали предикатът е изпълнен за всеки елемент от списък:
(define (all? p? lst)
  (if (null? lst)
      #t
      (and (p? (head lst)) (all? p? (tail lst)))))

; Проверява дали предикатът е изпълнен за някой елемент от списък:
(define (any? p? lst)
  (if (null? lst)
      #f
      (or (p? (head lst)) (any? p? (tail lst)))))

; Task 3:
; Комплектова два по два елементите на два списъка:
(define (zip lst1 lst2)
  (if (or (null? lst1) (null? lst2))
      '()
      (cons (cons (head lst1)
                  (head lst2))
            (zip (tail lst1) (tail lst2)))))

; Task 4:
(define (zipWith f lst1 lst2)
   (if (or (null? lst1) (null? lst2))
       '()
       (cons (f (head lst1) (head lst2))
             (zipWith f (tail lst1) (tail lst2)))))

(define (zip* lst1 lst2)
  (zipWith cons lst1 lst2))

; Task 5:
; Проверява дали даден списък е сортиран
(define (sorted? lst)
  (or (null? lst)
      (null? (tail lst))
      (and (<= (head lst) (head (tail lst)))
           (sorted? (tail lst)))))

; Task 6:
; Връща списък от уникалните елементи на даден списък:
(define (uniques lst)
  (define (helper lst res)
    (cond [(null? lst) res]
          [(member (head lst) res) (helper (tail lst) res)]
          [else (helper (tail lst) (cons (head lst) res))])
  )
  (helper lst '())
)

; ------------
; Side notes:

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












