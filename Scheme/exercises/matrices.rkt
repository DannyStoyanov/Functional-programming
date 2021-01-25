#lang racket/base
; Базови функции за матрици

; Примерна матрица:
(define matrix '( (1 2 3)
                  (4 5 6)
                  (7 8 9)))

; Намира броя редове на матрица:
(define (number-rows m)
  (length m))

; Намира броя колони на матрица:
(define (number-cols m)
  (if (null? m)
      0
      (length (car m))))

; Връща първият ред на матрица:
(define (first row m)
  (car m))

; Премахва първия ред на матрица:
(define (remove-first m)
  (cdr m))

; Премахва първата колона на матрица:
(define (remove-first-col m)
  (map cdr m))

; Връща n-ти ред на матрица:
(define (get-nth-row m n)
  (list-ref m n))

; Връща n-та колона на матрица:
(define (get-nth-col m n)
  (map (lambda (row) (list-ref row n)) m))

; Връща елемент на позиция (x, y):
(define (get-element m x y)
  (list-ref (list-ref m x) y))

(define (remove-nth m n)
  (cond [(null? m) m]
        [(= n 0) (cdr m)]
        [else (cons (car m)
                    (remove-nth (cdr m) (- n 1)))]))
        
; Премахва n-ти ред на матрица:
(define (remove-row m n)
  (remove-nth m n))

; Премахва n-та колона на матрица:
(define (remove-col m n)
  (map (lambda (row) (remove-nth row n)) m))

; Транспонира матрица:
(define (transpose m)
  (apply map list m))