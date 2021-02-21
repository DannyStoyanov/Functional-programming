; Task 3:
; Даниел Здравков Стоянов 45574

; От discord:
; преобразува до низ
;(string #\a) --> "a"
; намира дължина на низ
;(string-length "abc") --> 3
; извлича елемент намиращ се на даден индекс
;(string-ref "abc" 1) --> #\b
; конкатенира два низа
;(string-append "abc" "def") --> "abcdef"

;(char->integer)
;(char-whitespace? (string-ref "hello world" 0))
;(define (char-digit? c) (and (char>=? c #\0) (char<=? c #\9)))
;(char-digit? (string-ref "ba" 0))


;(define (reverse-string str)
;  (define len (string-length str))
;  (define (helper i res)
;    (if(< i 0)
;       res
;       (helper (- i 1) (string-append res (string (string-ref str i))))
;       )
;    )
;  (helper (- len 1) "")
;  )

; check whether the symbol is digit:
(define (char-digit? c) (and (char>=? c #\0) (char<=? c #\9)))

; check whether the symbol is operation:
(define plus #\+ )
(define minus #\- )
(define mulp #\* )
(define div #\/ )
(define power #\^ )
(define (char-operation? op)
  (or (eq? op plus) (eq? op minus) (eq? op div) (eq? op mulp) (eq? op power))
  )

; check whether the symbol is space
(define (char-space? x)
  (if(equal? x #\space) #t #f))

(define (skip-digits str index) ; returns the index after the whole number
  (define len (string-length str))
  (define (helper i)
    (cond ((= (+ i 1) len) (+ i 1))
          ((char-digit? (string-ref str (+ i 1))) (helper (+ i 1)))
          (else (+ i 1)))
    )
  (helper index) 
)

(define (skip-spaces str index) ; returns the index after the sequence of spaces
  (define len (string-length str))
  (define (helper i)
    (cond ((= (+ i 1) len) (+ i 1))
          ((char-space? (string-ref str (+ i 1))) (helper (+ i 1)))
          (else (+ i 1)))
    )
  (helper index) 
)

; (expr-valid? expr)
; flag = #t --> whether the last symbol is number 
; flag = #f --> whether the last symbol is operation
(define (expr-valid? expr)
  (define len (string-length expr))
  (define (helper i flag)
    (cond ((equal? expr "") #t)
          ((>= i len) flag) ; last symbol is number(aka flag = #t) & end of expr --> valid expr!
          ((equal? flag #t) (cond ((char-digit? (string-ref expr i)) #f) ; two numbers in a row(with space between) --> invalid
                                  ((char-space? (string-ref expr i)) (helper (skip-spaces expr i) flag) ) ; keep flag for the next symbol
                                  ((char-operation? (string-ref expr i)) (helper (+ i 1) #f) )
                                  ))
          ((equal? flag #f) (cond ((char-digit? (string-ref expr i)) (helper (skip-digits expr i) #t) )
                                  ((char-space? (string-ref expr i)) (helper (skip-spaces expr i) flag) ) ; keep flag for the next symbol
                                  ((char-operation? (string-ref expr i)) #f) ; two operations in a row(opt. with space between) --> invalid
                                  ))
          )
    )
  (helper 0 #f) ; starting with #f --> expecting number in order to be valid
  )


; clear whitespaces:
(define (filter-whitespaces str)
  (define len (string-length str))
  (define (helper i res)
    (cond ((>= i len) res)
          ((char-whitespace? (string-ref str i)) (helper (+ i 1) res))
          (else (helper (+ i 1) (string-append
                                   res
                                   (string (string-ref str i)))))
          )
    )
  (helper 0 "")
)

(define (priority op)
  (cond ((or (equal? op mulp) (equal? op div) (equal? op power)) 2)
        ((or (equal? op plus) (equal? op minus)) 1)
        ))

  
; (reverse-polish) - main functionality of expr-rp
(define (reverse-polish expr)
  (define len (string-length expr))
 ;(define (helper i str ...)
  expr
)

; (expr-rp expr)
(define (expr-rp expr)
  (if (not (expr-valid? expr))
      #f
      (let ( (str (filter-whitespaces expr))
            )
        (reverse-polish str)
      ) 
  )
)


; (evaluate) - main functionality of expr-eval
(define (evaluate expr)
  (define len (string-length expr))
 ;(define (helper i str ...)
  expr
)

(define (eval a b op)
  (op b a))


; (expr-eval expr)
(define (expr-eval expr)
  (if (not (expr-valid? expr))
      #f
      (let ( (str (filter-whitespaces expr))
            )
        (evaluate str)
      ) 
  )
) 