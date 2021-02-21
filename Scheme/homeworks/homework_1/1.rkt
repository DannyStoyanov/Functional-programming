; Task 1:
; Даниел Здравков Стоянов 45574

(define (accumulate op term init a next b)
  (define (loop i)
    (if (<= i b)
        (op (term i) (loop (next i)) )
        init
        )
    )
  (loop a)
  )

(define (accumulate* op term init a next b)
  (define (loop i)
    (if (>= i a)
        (op (term i) (loop (next i)))
        init
        ))
  (loop b)
  )


; Useful definitions:
(define vert-line #\│)
(define space " ")
(define (id x) x)
(define (1+ x) (+ x 1))
(define (4- x) (- x 4))

; Simple line drawing:
(define (drawline len)
  (define c  #\─)
  (define (op x result) (display c))
  (accumulate op id 0 1 1+ len)
  )

; Row components:
(define (draw-left-up-corner level)
  (define (op x result)
    (display vert-line)
    (display space)
    )
  (accumulate op id 0 2 1+ level)
  (display #\┌)
  )

(define (draw-right-up-corner level)
  (display #\┐)
  (define (op x result)
    (display space)
    (display vert-line)
    )
  (accumulate op id 0 2 1+ level)
  )

(define (draw-left-down-corner level)
  (define (op x result)
    (display vert-line)
    (display space)
    )
  (accumulate op id 0 2 1+ level)
  (display #\└)
  )

(define (draw-right-down-corner level)
  (display #\┘)
  (define (op x result)
    (display space)
    (display vert-line)
    )
  (accumulate op id 0 2 1+ level)
  )

; Single row drawing:
(define (draw-up-row level len)
  (draw-left-up-corner level)
  (drawline len)
  (draw-right-up-corner level)
  )

(define (draw-down-row level len)
  (draw-left-down-corner level)
  (drawline len)
  (draw-right-down-corner level)
  )
; -----------------------------
(define (loop-down-half level)
    (define (op x result)
    (draw-down-row x result)
    (display "\n")
    (+ result 4)
    )
  (accumulate op id 1 1 1+ level)
  )

; Reverse accumulate function(reverse interval from b to a):
(define (accumulate* op term init a next b)
  (define (loop i)
    (if (>= i a)
        (op (term i) (loop (next i)))
        init
        ))
  (loop b)
  )

(define (calc-line-init n)
  (+ (* (- n 1) 4) 1))

(define (1- x) (- x 1))
(define (loop-up-half level)
    (define (op x result)
    (draw-up-row x result)
    (display "\n")
    (- result 4)
    )
  (accumulate* op id (calc-line-init level) 1 1- level)
  )

; main function:
(define (squares n)
  (if (> n 0)
      (begin
        (+ (loop-up-half n) (loop-down-half n))
        (display #\ )) 
      (display #\ )
      ))
                     

  
