#lang racket

Arrange-Act-Assert
(let ((r1 (cons-rational 1 2))
      (r2 (cons-rational 2 3)))
  (equal? (cons-rational 1 3)
          (multiply-rational r1 r2))
)

r1 -> 1/2
r2 -> 2/3

result = r1 * r2


(require rackunit "test.rkt")

(test-suite
 "tests with selectors"
 
 (test-case
  "numerator retrieves correct values"
  (check-equal? (numerator (cons-rational 10 20)) 10)
  )

 (test-case
  "denomirator retrieves correct values"
  (check-equal? (denomerator (cons-rational 10 20)) 20)
  )
 )

(test-case
 "numerator retrieves correct values"
 (check-true (rational? (cons-rational 10 20)))
 )

(test-suite
 "tests with check-false"
 
 (test-case
  "   "
  (check-false (rational? "123"))
  )

 (test-case
  "   "
  (check-false (rational? 5))
  )
 )