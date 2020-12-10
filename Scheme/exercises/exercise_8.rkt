#lang racket/base
; Exercise N8

(define space #\space)
(define (char-space? x)
  (if (equal? space x) #t #f))

(define str "a6* {} ")
(char-space? (string-ref str 0))
(define (is? s? str pos)
  (s? (string-ref str pos)))

; HM1 notes:
(define (div2 n) (quotient n 2))
(define (mod2 n) (remainder n 2))

(define set-empty? zero?)
(define set-empty 0)
(define (last-bit? s)
  (= (mod2 s) 1))

;(define (set-intersect s1 s2)
;  (if (or (set-empty s1) (set-empty se))
;      set-empty
;      (+ (* 2 (set-intersect (div2 s1)
;                             (div2 s2)))
;         (if (and (last-bit? s1) (last-bit? s1))
;             1
;             0))))
;
;(define (set-union s1 s2)
;  (if (or (set-empty? s1) (set-empty? s2))
;      (+ s1 s2)
;      (+ (* 2 (set-intersect (div2 s1)
;                             (div2 s2)))
;         (if (or (last-bit? s1) (last-bit? s1))
;             1
;             0))))

(define (set-sth s1 s2 combine bits-op)
  (if (or (set-empty? s1) (set-empty? s2))
  (combine s1 s2)
  (+ (* 2 (set-intersect (div2 s1)
                         (div2 s2)))
     (if (bits-op (last-bit? s1) (last-bit? s2))
         1
         0))))

(define (set-intersect s1 s2)
  (set-sth s1 s2
           (lambda (s1 s2) set-empty)
           (lambda (b1 b2) (and b1 b2))))