#lang racket/base
(require racket/stream)
(provide (all-defined-out))
; Домашно №2
; Даниел Здравков Стоянов 45574
; Специалност: Информатика
; Курс: 3
; Група: 1

; Проверява дали символа е число:
(define (char-digit? c) (and (char>=? c #\0) (char<=? c #\9)))

; Връща индекса след число:
(define (skip-number str index) 
  (define len (string-length str))
  (define (helper i)
    (cond [(= (+ i 1) len) (+ i 1)]
          [(char-digit? (string-ref str (+ i 1))) (helper (+ i 1))]
          [else (+ i 1)])
  )
  (helper index) 
)

; Проверява дали симво е интервал:
(define (char-space? x)
  (if(equal? x #\space) #t #f))

; Връща индекса след поредица от интервали:
(define (skip-spaces str index) 
  (define len (string-length str))
  (define (helper i)
    (cond [(= (+ i 1) len) (+ i 1)]
          [(char-space? (string-ref str (+ i 1))) (helper (+ i 1))]
          [else (+ i 1)])
  )
  (helper index)
)

; Проверява дали символът е начало на дърво:
(define (char-start-bracket? x)
  (if(equal? x #\{) #t #f))

; Проверява дали символът е край на дърво:
(define (char-end-bracket? x)
  (if(equal? x #\}) #t #f))

; Проверява дали символът е празно дърво:
(define (char-star? x)
  (if(equal? x #\*) #t #f))

; аналог на (char-symbol? (string-ref str i))
(define (is? type? pos str)
  (if (type? (string-ref str pos)) #t #f))

; Връща индекса след първата отваряща скоба:
(define (get-first-bracket str)
  (define len (string-length str))
  (define (helper i)
    (cond [(>= i len) (- i 1)]
          [(is? char-space? i str) (helper (+ i 1))]
          [(is? char-start-bracket? i str) (+ i 1)]
          [else (- len 1)])
  )
  (helper 0)
)

; Връща индекс на следващ елемент
; на дърво от символен низ
(define (find-next pos str)
  (define len (string-length str))
  (define (helper i)
    (cond [(>= i len) (- i 1)]
          [(is? char-space? i str) (helper (+ i 1))]
          [else i])
  )
  (helper pos)
)

; Прескача дадено дърво като връща индекса след неговия край: 
(define (skip-subtree str index)     
  (define len (string-length str))
  (define (helper i counter)
    (cond [(>= (+ i 1) len) i]
          [(is? char-start-bracket? i str) (helper (+ i 1) (+ counter 1))] ; броим колко скоби сме отворили 
          [(is? char-end-bracket? i str) (if(= (- counter 1) 0)            ; на текущото дърво, за да може,
                                                   (+ i 1)                          ; ако има поддървета да
                                                   (helper (+ i 1) (- counter 1)))] ; прескочим и тях 
          [else (helper (+ i 1) counter)])
  )
  (helper index 0)
)

; Преброява дали дърво се състои от 3 елемента:
; - връща истина, ако са 3 и след всяка отваряща скоба има корен(число)
; - връща лъжа в противен случай
(define (count-elements pos str)
  (define len (string-length str))
  (define (helper i counter) 
    (cond [(or (>= i len) (is? char-end-bracket? i str)) (if(= counter 3) #t #f)] ; --> при знак за край на дърво проверяваме броят на елементите
          [(is? char-digit? i str) (helper (skip-number str i) (+ counter 1))]    ; --> при число увеличаваме елементите с 1
          [(is? char-start-bracket? i str) (if(and (char-digit? (string-ref str (find-next (+ i 1) str))) (count-elements (+ i 1) str))  ; --> при елемент поддърво 
                                                       (helper (skip-subtree str i) (+ counter 1))                                       ;     проверяваме елементите му
                                                       #f)]                                                                              ;     дали са 3 на брой и дали има корен(число)
          [(is? char-space? i str) (helper (skip-spaces str i) counter)]          ; --> при интервал броят на елементите се запазва
          [(is? char-star? i str) (helper (+ i 1) (+ counter 1))]                 ; --> при звезда(празно дърво) увеличаваме елементите с 1
          [else #f])
  )
  (if (= pos 0) (helper (get-first-bracket str) 0) (helper pos 0))
)

; Проверява следните невалидни случаи:
; '{'     следващ елемент     '{'
; '{'     следващ елемент     '*'
; 'число' следващ елемент 'число'
; '*'     следващ елемент 'число'
; '}'     следващ елемент 'число'
(define (filter-str pos str)
  (define len (string-length str))
  (define (helper i)
    (cond [(>= i len) #t]
          [(is? char-start-bracket? i str) (if(or(char-start-bracket? (string-ref str (find-next (+ i 1) str)))
                                                 (char-star? (string-ref str (find-next (+ i 1) str))))
                                              #f
                                              (helper (+ i 1)))]
          [(is? char-digit? i str) (if(char-digit? (string-ref str (find-next (skip-number str i) str)))
                                      #f
                                      (helper (+ i 1)))]
          [(is? char-star? i str) (if(char-digit? (string-ref str (find-next (+ i 1) str)))
                                     #f
                                     (helper (+ i 1)))]
          [(is? char-end-bracket? i str) (if(char-digit? (string-ref str (find-next (+ i 1) str)))
                                            #f
                                            (helper (+ i 1)))]
          [else (helper (+ i 1))])
  )
  (helper pos)
)

; Премахава интервалите от даден символен низ:
(define (filter-whitespaces str)
  (define len (string-length str))
  (define (helper i res)
    (cond ((>= i len) res)
          ((is? char-space? i str) (helper (+ i 1) res))
          (else (helper (+ i 1) (string-append
                                   res
                                   (string (string-ref str i)))))
          )
    )
  (helper 0 "")
)

; Проверява дали символният низ е празно дърво - *:
(define (empty-tree-str? str)
  (define newStr (filter-whitespaces str))
  (if (equal? newStr "*") #t #f))

; Проверява дали подаденият ѝ като аргумент символен низ е коректно представяне на дърво:
(define (tree? str)
  (if (equal? str "")
      #f
      (or (empty-tree-str? str) (and (count-elements 0 str) (filter-str 0 str)))
  )
)

; Взима символ по символ докато среща цифри и образува стринг от тях.
; Връща числото като стринг:
(define (get-number pos str)
  (define (helper i res)
    (if (is? char-digit? i str)
        (helper (+ i 1) (string-append res (string (string-ref str i))))
        res)
  )
  (helper pos "")
)

; Двигателя на string->tree.
; Контруира двоично дърво от символен низ като резултата е списък:
(define (string-to-tree pos str)
  (define (helper i res)
    (cond [(is? char-end-bracket? i str) res]
          [(is? char-digit? i str) (helper (skip-number str i)
                                                    (append res (cons (string->number (get-number i str)) '())) )]
          [(is? char-star? i str) (helper (+ i 1)
                                                   (append res (cons '() '())) )]
          [(is? char-start-bracket? i str) (helper (skip-subtree str i)
                                                            (append res (cons (string-to-tree (+ i 1) str) '()) ))])
    )
  (helper pos '())
)

; Конструира и връща двоично дърво по подаденото му текстово представяне str:
(define (string->tree str)
  (if (not (tree? str)) ; проверка за валидността на символния низ
      #f
      (let [(new-str (filter-whitespaces str))] ; премахваме интервалите
        (if (empty-tree-str? new-str)
            '()
            (string-to-tree 1 new-str)) ; контруира двоично дърво от символен низ 
        )
      )
  )

; Помощни дефиниции за лист:
(define head car)
(define tail cdr)
(define (list-tree? t)
  (or (null? t)
      (and (list? t)
           (= (length t) 3))
      (list-tree? (cadr t))
      (list-tree? (caddr t))))

; Двигателя на tree->string.
; Контруира символен низ от зададено двоично дърво tree:
(define (tree-to-string tree)
  (define (helper lst res)
    (cond [(null? lst) res]
          [(null? (head lst)) (helper (tail lst) (string-append res
                                                                " *"))]
          [(number? (head lst)) (helper (tail lst) (string-append
                                                    res
                                                    (number->string (head lst))))]
          [(list-tree? (head lst)) (helper (tail lst) (string-append res
                                                                     " "
                                                                     (tree->string (head lst))))])
    )
  (helper tree "")
)

;Преобразува двоичното дърво tree до неговото представяне като символен низ:
(define (tree->string tree)
  (if (null? tree)
      "*"
      (string-append "{" (tree-to-string tree) "}")))


; Помощни функции и дефиниции за двоично дърво:
(define empty-tree '())
(define (make-tree root left right) (list root left right))
(define (make-leaf root) (make-tree root empty-tree empty-tree))
(define root-tree car)
(define left-tree cadr)
(define right-tree caddr)
(define empty-tree? null?)
(define (is-leaf? tree)
  (and (empty-tree? (left-tree tree))
       (empty-tree? (right-tree tree))))

; Намира височината на двоично дърво:
(define (height t)
  (if (empty-tree? t)
      0
      (+ 1 (max (height (left-tree t))
                (height (right-tree t))))))

; Проверява дали двоичното дърво tree е балансирано по височина. Връща #t или #f:
(define (balanced? tree)
  (if (empty-tree? tree)
      #t
      (if (<= (abs (- (height (left-tree tree)) (height (right-tree tree)) )) 1)
          #t
          #f))
)

(define (max-el tree)
  (cond [(is-leaf? tree) (root-tree tree)]
        [(empty-tree? (left-tree tree)) (max (root-tree tree) (max-el (right-tree tree)))]
        [(empty-tree? (right-tree tree)) (max (root-tree tree) (max-el (left-tree tree)))]
        [else (max (root-tree tree)
                   (max-el (left-tree tree))
                   (max-el (right-tree tree)))]))

(define (min-el tree)
  (cond [(is-leaf? tree) (root-tree tree)]
        [(empty-tree? (left-tree tree)) (min (root-tree tree) (min-el (right-tree tree)))]
        [(empty-tree? (right-tree tree)) (min (root-tree tree) (min-el (left-tree tree)))]
        [else (min (root-tree tree)
                   (min-el (left-tree tree))
                   (min-el (right-tree tree)))]))

(define (search-el tree comp)
  (cond [(is-leaf? tree) (root-tree tree)]
        [(empty-tree? (left-tree tree)) (comp (root-tree tree) (search-el (right-tree tree) comp))]
        [(empty-tree? (right-tree tree)) (comp (root-tree tree) (search-el (left-tree tree) comp))]
        [else (comp (root-tree tree)
                   (search-el (left-tree tree) comp)
                   (search-el (right-tree tree) comp))]))

; Проверява дали двоичното дърво tree е двоично наредено дърво. Връща #t или #f:
(define (ordered? tree)
  (cond [(or (empty-tree? tree) (is-leaf? tree)) #t]
        [(empty-tree? (right-tree tree)) (and (>= (root-tree tree) (max-el (left-tree tree)))
                                              (ordered? (left-tree tree)))]
        [(empty-tree? (left-tree tree)) (and (< (root-tree tree) (min-el (right-tree tree)))
                                             (ordered? (right-tree tree)))]
        [else (and (> (root-tree tree) (max-el (left-tree tree)))
                   (<= (root-tree tree) (min-el (right-tree tree)))
                   (ordered? (left-tree tree))
                   (ordered? (right-tree tree)))]))


; -----------------------------
; Помощни функции за visualize:

; Преброява колко цифрено е едно число:
(define (count-digits n)
  (if (<= n 9)
      1
      (+ 1 (count-digits (quotient n 10)))))

; Връща списък от k-то ниво на дръво:
(define (tree-level k t)
  (cond [(empty-tree? t) '()] 
        [(zero? k) (list (root-tree t))]
        [else (append (tree-level (- k 1) (left-tree t))
                      (tree-level (- k 1) (right-tree t)))]))

; Връща бройката от всички цифри от списъка с числа: {1 23 47 113} -> 8
(define (numbers->digits lst)
  (define (helper l res)
    (cond [(null? l) res]
          [else (helper (tail l) (+ res (count-digits (head l))))]))
  (helper lst 0)
)

; Намира ширината на дървото в брой цифри
(define (max-width t)
  (define (helper k res)
    (cond [(empty-tree? t) res]
          [(>= k (height t)) res]
          [else (helper (+ k 1) (max res (numbers->digits (tree-level k t))))])
  )
  (helper 0 0)
)

; Идеята ми беше да принтирам максималния брой отстъп за всяко ребро
; като по-лесна версия на задачата като намеря ширината(която
; да използвам за реброто между корена и дясното поддърво) и
; височина(която да използвам за реброто между корена и лявото
; поддърво).

; -----------------------------
; Inorder --> Ляво-->Корен-->Дясно
(define (inorder tree)
  (if (empty-tree? tree)
      '()
      (append (inorder (left-tree tree))
              (list (root-tree tree))
              (inorder (right-tree tree)))))

; Preorder  - Корен-->Ляво-->Дясно
(define (preorder tree)
  (if (empty-tree? tree)
      '()
      (append (list (root-tree tree))
              (preorder (left-tree tree))       
              (preorder (right-tree tree)))))

; Postorder - Ляво-->Дясно-->Корен
(define (postorder tree)
  (if (empty-tree? tree)
      '()
      (append (postorder (left-tree tree))       
              (postorder (right-tree tree))
              (list (root-tree tree)))))

; Конструира поток от дърво със зададен ред на обхождане:
(define (tree->stream tree order)
  (cond [(or (equal? tree #f) (equal? (tree? (tree->string tree)) #f)) #f] ; equal? tree #f когато ползвам (string->tree)  тестовете
        [(equal? order 'inorder) (stream-first (stream (inorder tree)))]
        [(equal? order 'preorder) (stream-first (stream (preorder tree)))]
        [(equal? order 'postorder) (stream-first (stream (postorder tree)))]
        [else #f]
  ))