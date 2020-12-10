#lang racket
; Exercise N5

(define head car) 
(define tail cdr) 

; Намира дължина на списък:
(define (length lst)
  (if (null? lst) 0
      (+ 1 length (tail lst))))

; Проверява дали елемента х се съдържа в списък:
(define (member x lst)
  (cond [(null? lst) #f] 
        [(equal? (head lst) x) #t]  ; заб.: вградената member тук връща lst
        [else (member x (tail lst))]))

; Task 6:
; solution 1:
(define (uniques lst)
  (if (null? lst) '()
      (let [(rest  (uniques (tail lst)))]
      (if (member (head lst) rest)
          rest
          (cons (head lst) rest)))))

; solution 2:
(define (uniques* lst)
  (if (null? lst) '()
      (cons (head lst)
            (uniques* (filter (lambda (x) (not (equal? x (head lst))))
                            (tail lst))))))

; solution 3:
(define (uniques** lst)
  (define (helper lst res)
    (cond [(null? lst) res]
          [(member (head lst) res) (helper (tail lst) res)]
          [else (helper (tail lst) (cons (head lst) res))]))
  (helper lst '())
  )

; Task 7:
(define (insert val lst)
  (cond [(null? lst) (list val)]
        [(> val (head lst)) (cons (head lst) (insert val (tail lst)))]
        [else (cons val lst)]))

; Task 8:
; Обобщено "насъбиране" на елементи от списък:
; "комбинираме" главата на дадения списък с
; резултата от насъбирането на опашката му.
(define (foldr op nv lst)
  (if (null? lst) nv
      (op (head lst)
          (foldr op nv (tail lst)))))

(define (insertion-sort lst)
  (foldr insert '() lst))

; Task 9:
; дали i1 е подинтервал na i2
(define (sub? i1 i2)
  (and (>= (head i1) (head i2))
       (<= (tail i1) (tail i2))))

; дължина на интервал
(define (int-length i)
  (- (tail i) (head i)))

; > за интервали
(define (>-int i1 i2)
  (if (> (int-length i1) (int-length i2)) i1 i2))

; максимален по дължина подинтервал
(define (max-interval lst)
  ; При търсене на мин/макс взимаме първия елемент
  ; като първоначален и обхождаме останалите.
  (foldr >-int (head lst) (tail lst)))

(define (filter p? lst)
  (cond [(null? lst) '()]
        [(p? (head lst)) (cons (head lst)
                               (filter p? (tail lst)))]
        [else (filter p? (tail lst))]))

(define (longest-interval-subsets lst)
  (define longest (max-interval lst)) ; за да не го преизчисляваме непрекъснато
  (filter (lambda (i) (sub? i longest)) lst))

; Task 10:
(define id (lambda (x) x))

(define (compose f g)
  (lambda (x)  (f (g x))))

(define (compose-n . fns)
  (foldr compose id fns))

; Task 11:
(define (group-by f lst)
  (define returned (uniques (map f lst)))
  (define (elements-for x)
    (filter (lambda (el) (equal? x (f el))) lst))
    (map (lambda (x)
           (list x (elements-for x)))
         returned))

; Task 12:
(define (any? p? lst)
  (or (null? lst)
      (or (p? (head lst)) (any? (tail lst)))))

; Функция, която приема произволен брой аргументи, но поне един (!)
; apply is our best friend here
(define (zipWith* f . lsts)
  (if (or (null? lsts) (any? null? lsts))
      '()
      (cons (apply f (map head lsts))
            (apply zipWith* f (map tail lsts)))))


; Side notes

(define (maximum lst)
  (foldr max (head lst) (tail lst)))

(define (uniques*** lst)
  (foldr (lambda (el res)
           (if (member el res) res (cons el res)))
         '()
         lst))

(define (length** lst)
  (foldr (lambda (el res) (+ 1 res)) 0 lst))

(define (map** f lst)
  (foldr (lambda (el res) (cons (f el) res)) '() lst))

(define (filter* p? lst)
  (foldr (lambda (el res) (if(p? el) (cons el res) res)) '() lst))

(define (quickSort lst)
  (if (or (null? lst) (null? (tail lst)))
      lst
      (let [(pivot (head lst))
            (rest (tail lst))]
        
      (append (quickSort (filter (lambda (x) (< x pivot))
                                 rest))
              (list pivot)
              (quickSort (filter (lambda (x) (>= x pivot))
                                 rest))))))











      
      