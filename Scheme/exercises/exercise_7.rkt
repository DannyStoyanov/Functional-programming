#lang racket
(define head car)
(define tail cdr)

(define (head-rows m) (head m))
(define (head-cols m) (map head m))
(define (tail-rows m) (tail m))
(define (tail-cols m) (map tail m))
(define (null-m? m) (or (null? m) (null? (head m))))

(define (sub-range i j lst)
  (drop (take lst j) i))

(define (sub-matrix i1 j1 i2 j2 m)
  (let [(the-rows (sub-range i1 i2 m))]
    (map (lambda (row) (sub-range j1 j2 row)) the-rows)))

(define (any? p? lst)
  (and (not (null? lst))
       (or (p? (head lst))
           (any? p? (tail lst)))))

(define (all p? lst)
  (not (any? (lambda (x) (not (p? x)) lst))))

(define (all-submatrices m)
  (define num-rows (length m))
  (define num-cols (length (head m)))
  (map (lambda (rows)
         (map (lambda (col) (cons row col)) (range 0 num-cols)))
       (range 0 rum-rows)))

(define (max-by-size ms)
  (foldr (lambda (el res)
           (if (> (length el) (length res)) el res))
         (head ms)
         (tail ms)))


  

(define (find-submatrix ps m)
  (define (is-Ok el) ; дали числото el удовлетворява накой предикат
    (any? (lambda (p) (p el)) ps))
  (define (is-Ok-mat m)
    (all? (lambda (row) (all? is-Ok row)) m))
  (define good-submatrices
    (filter is-Ok-mat (all-submatrices m)))
  (if (null? good-submatrices)
      #f 
      (max-by-size good-subamatrices))
)

