#lang racket
; Работа със списъци:
(define head car)
(define tail cdr)

; Намира дължина на лист - итеративно:
(define (length-iter L)
  (define (loop lst res)
    (if (null? lst)
        res
        (loop (tail lst) (+ res 1)))
  )
  (loop L 0)
)

; Рекурсивно решение:
(define (length-rec L)
  (if (null? L)
      0
      (+ 1 (length-rec (tail L)))
  )
)

; Генерира списък от всички цели числа в интервала [a, b]
(define (interval a b)
  (if (> a b)
      '()
      (cons a
            (interval (+ a 1) b)))
)

; дясно асоциативна 
(define (accumulate op term init a next b)
  (define (loop i)
    (if (<= i b)
        (op (term i) (loop (next i)))
        init))
  (loop a)
) 

(define id (lambda (x) x))
(define 1+ (lambda (x) (+ x 1)))
(define (interval* a b)
  (accumulate cons id '() a 1+ b)
)

; Генерира списък от всички четни числа в интервала [a, b]:
(define (collect-even a b)
  (cond [(> a b) '()]
        [(even? a) (cons a (collect-even (+ a 1) b))]
        [else (collect-even (+ a 1) b)]))

(define (collect-even* a b)
  (define (op i result)
    (if (even? i)
        (cons i result)
        result))
  (accumulate op id '() a 1+ b))

(define (collect a b)
  (cond ((> a b) '())
        ((even? a) (cons a
                         (collect (+ a 1) b)))
        (else (collect (+ a 1) b))
  )
)

(define (reverse* L)
  (define (loop lst res)
    (if (null? lst)
        res
        (loop (tail lst) (cons (head lst) res)))
   )
  (loop L '())
)

(define (append-el L1 L2)
  (define (loop L result)
    (if (null? L)
        result
        (loop (tail L) (cons (head L) result))
    )
  )
  (loop (reverse* L1) L2)
)

(define x '(1 ("Hlsd") (2 (3) 4) (a b c))) 
(define (flatten lst)
  (cond [(null? lst) '()]
        [(pair? (head lst)) (append (flatten (head lst))
                                    (flatten (tail lst)))]
        [else (cons (head lst)
                    (flatten (tail lst)))]
  )
)

; -------------------------------------------
; map, filter, foldr, any?, all?, drop, take, range,
; dropWhile, takeWhile, list-ref, zip, zipWith

; В стандартната реализация на racket
; може да подадем няколко списъкa, но с
; еднаква дължина и map прилага операцията
; като върху първите елементи на всички
; списъци и така върху останалите
(define (map op lst)
  (if (null? lst)
      '()
      (cons (op (head lst))
                (map op (tail lst)) )))

; Връща списък от онези елементи на
; списъка, за които е верен предиката
(define (filter pred? lst)
  (cond [(null? lst) '()]
        [(pred? (head lst)) (cons (head lst)
                                  (filter pred? (tail lst)))]
        [else (filter pred? (tail lst))])
)

(define (foldr op nv lst)
  (if (null? lst) nv
      (op (head lst)
          (foldr op nv (tail lst)))))

(define (any? p? lst)
  (if (null? lst)
      #t
      (or (p? (head lst))
           (any? p? (tail lst)))))

(define (all? p? lst)
  (if (null? lst)
      #t
      (and (p? (head lst))
           (all? p? (tail lst)))))
      
(define (drop n lst)
  (cond [(null? lst) '()]
        [(= n 0) lst]
        [else (drop (- n 1) (tail lst))]))

(define (take n lst)
  (cond [(null? lst) '()]
        [(= n 0) '()]
        [else (cons (head lst) (take (- n 1) (tail lst)))]))

(define (range* start end)
  (if (>= start end)
      '()
      (cons start
            (range (+ start 1) end))))

(define (dropWhile lst p?)
  (cond [(null? lst) '()]
        [(p? (head lst)) (dropWhile (tail lst) p?)]
        [else lst]))

(define (reverse** lst)
  (foldl cons '() lst))

(define (takeWhile lst p?)
  (cond [(null? lst) '()]
        [(p? (head lst)) (cons (head lst) (takeWhile (tail lst) p?))]
        [else '()]))

(define (list-ref lst pos)
  (cond [(null? lst) '()]
        [(= pos 1) (head lst)]
        [else (list-ref (tail lst) (- pos 1))]))

(define (zip L1 L2)
  (if (or (null? L1) (null? L2))
      '()
      (cons (cons (head L1) (head L2))
            (zip (tail L1) (tail L2)))))

;---------------------------------------------
(define (member? x lst)
  (cond [(null? lst) #f]
        [(equal? x (head lst)) #t]
        [else (member? x (tail lst))]))

(define (uniques* L)
  (define (helper lst res)
    (cond [(null? lst) res]
          [(member? (head lst) res) (helper (tail lst) res)]
          [else (helper (tail lst) (cons (head lst) res))]))
  (helper L '())
)

(define (group-by f L)
  (define returned (uniques* (map f L)))
  (define (elements-for x)
    (filter (lambda (el) (equal? x (f el))) L))
  (map (lambda (x)
         (list x (elements-for x)))
       returned))
;---------------------------------------------

;XxXxXxXxXxXxXxXxX--Matrices--XxXxXxXxXxXxXxXxX

; Exercise N6

(define (head-rows m) (head m))
(define (head-cols m) (map head m))
(define (tail-rows m) (tail m))
(define (tail-cols m) (map tail m))
(define (null-m? m) (or (null? m) (null? (head m))))

(define m '((1 2 3 4 5 6)
            (2 3 4 5 8 9)
            (0 2 6 4 8 2)
            (5 2 8 4 9 0)))

; Task 1:
(define (sub-range i j lst)
  (drop (take lst j) i))

(define (sub-matrix i1 j1 i2 j2 m)
  (let [(the-rows (sub-range i1 i2 m))]
    (map (lambda (row) (sub-range j1 j2 row)) the-rows)))

; Task 2:
(define (foldr-matrix op-rows nv-rows op-elems nv-elems m)
  (foldr 
   (map (lambda (row) (foldr op-elems nv-elems row) m) )))


; Binary trees:
(define (tree? t)
  (or (null? t)
      (and (list? t)
           (= (length t) 3)
           (tree? (cadr t))
           (tree? (caddr t)))))
(define empty-tree '())
(define (make-tree root left right) (list root left right))     
(define (make-leaf root) (make-tree root empty-tree empty-tree)) 
(define root-tree car)
(define left-tree cadr)
(define right-tree caddr)
(define empty-tree? null?)


(define test-tree
  (make-tree 68
             (make-tree 72
                        (make-leaf 103)
                        (make-leaf 206))
             (make-tree 31
                        (make-tree 45
                                   (make-leaf 16)
                                   (make-leaf 23))
                        empty-tree)))

; Task 4:
; Намира сумата на дърво:
(define (tree-sum tree)
  (if (empty-tree? tree)
      0
      (+ (root-tree tree)
         (tree-sum (left-tree tree))
         (tree-sum (right-tree tree)))))

; Task 5:
; Извежда списък от k-то ниво на дърво:
(define (tree-level k tree)
  (cond [(empty-tree? tree) '()]
        [(zero? k) (list (root-tree tree))]
        [else (append (tree-level (- k 1) (left-tree tree))
                      (tree-level (- k 1) (right-tree tree)))]))

; Task 6:
; Намира височината на дърво:
(define (height* tree)
  (if (empty-tree? tree)
      0
      (+ 1 (max (height* (left-tree tree))
                (height* (left-tree tree))))))

; Извежда списък от списъци с нивата на дървото:
(define (all-levels t)
  (map (lambda (i) (tree-level i t)) (range* 0 (+ (height* t) 1))))

; Task 7:
(define (tree-map f t)
  (if (empty-tree? t)
      t
      (make-tree (f (root-tree t))
                 (tree-map f (left-tree t))
                 (tree-map f (right-tree t)) )))

; Task 8:
; Връща лист от обхождането: ляво-корен-дясно
(define (inorder t)
  (if (empty-tree? t)
      '()
      (append (inorder (left-tree t))
              (list (root-tree t))
              (inorder (right-tree t)))))

; Връща лист от обхождането: корен-ляво-дясно
(define (preorder  t)
  (if (empty-tree? t)
      '()
      (append (list (root-tree t))
              (preorder (left-tree t))   
              (preorder (right-tree t)))))

; Връща лист от обхождането: ляво-дясно-корен
(define (postorder t)
  (if (empty-tree? t)
      '()
      (append (postorder (left-tree t))
              (postorder (right-tree t))
              (list (root-tree t)))))

; Task 9:
; Проверява дали даден връх е листо:
(define (is-leaf? tree)
  (and (empty-tree? (left-tree tree))
       (empty-tree? (right-tree tree))))

; Вмъква стойността val в двоично наредено дърво:
(define (bst-insert val t)
  (cond [(empty-tree? t) (make-leaf val)]
        [(< val (root-tree t))
         (make-tree (root-tree t)
                    (bst-insert val (left-tree t))
                    (right-tree t))]
        [else (make-tree (root-tree t)
                         (left-tree t)
                         (bst-insert val (right-tree t)))]))

; Task 10:
; Премахва всички листа от дърво:
(define (prune tree)
  (cond [(is-leaf? tree) empty-tree]
        [(empty-tree? (right-tree tree))
         (make-tree (root-tree tree)
                    (prune (left-tree tree))
                    empty-tree)]
        [(empty-tree? (left-tree tree))
         (make-tree (root-tree tree)
                    empty-tree
                    (prune (left-tree tree)))]
        [else (make-tree (root-tree tree)
                         (prune (left-tree tree))
                         (prune (right-tree tree)))]))

  
; Task 11:
; Да се напише функция (bloom tree x), която заменя
; всяко листосъс стойност x със следното дърво:
;+---------------+
;|       x       |
;|      / \      |
;|     x   x     |
;+---------------+
(define (bloom tree val)
  (cond [(empty-tree? tree) empty-tree]
        [(is-leaf? tree) (if (= val (root-tree tree))
                          (make-tree val (make-leaf val) (make-leaf val))
                          (make-leaf (root-tree tree)))]
        [else (make-tree (root-tree tree)
                         (bloom (left-tree tree) val)
                         (bloom (right-tree tree) val))]))

; -------------------------------
(define (avr-val x y)
  (/ (+ x y) 2))

; Функция сравняваща елементите на дърво чрез операция op
; (op трябва да е релация на частична наредба)
(define (find-by tree op)
  (cond [(is-leaf? tree) (root-tree tree)]
        [(empty-tree? (left-tree tree)) (op (root-tree tree) (find-by (right-tree tree) op))]
        [(empty-tree? (right-tree tree)) (op (root-tree tree) (find-by (left-tree tree) op))]
        [else (op (root-tree tree)
                  (find-by (left-tree tree) op)
                  (find-by (right-tree tree) op))]))

; Променя всеки връх със стойност from-val
; към стойност to-val на дърво:
(define (change-to-val tree from-val to-val)
  (cond [(empty-tree? tree) empty-tree]
        [(equal? (root-tree tree) from-val) (make-tree to-val
                                             (change-to-val (left-tree tree) from-val to-val)
                                             (change-to-val (right-tree tree) from-val to-val))]
        [else (make-tree (root-tree tree)
                         (change-to-val (left-tree tree) from-val to-val)
                         (change-to-val (right-tree tree) from-val to-val))]))

; List exercise:

; 1. (len lst), която намира дължината на списък
(define (len lst)
  (if (null? lst)
      0
      (+ 1 (len (tail lst)))))

; 2. (exists? lst p), която проверява дали съществува елемент в lst, за който е изпълнен предикатът p
(define (exists? lst p?)
  (cond [(null? lst) #f]
        [(p? (head lst)) #t]
        [else (exists? (tail lst) p?)]))

; 3. (member? lst x), която проверява дали елементът x се съдържа в списъка lst (какво значи "съдържа"?)
(define (member*? lst x)
  (cond [(null? lst) #f]
        [(equal? (head lst) x) #t]
        [else (member*? (tail lst) x)]))

; 4. (at n lst), която връща елементът, намиращ се на позиция n (броим от 0) в списъка lst или #f, ако позицията е извън списъка
(define (at n lst)
  (cond [(null? lst) #f]
        [(= n 0) (head lst)]
        [else (at (- n 1) (tail lst))]))

; 5. (map f lst), която прилага f върху всеки елемент на списъка lst
(define (map* f lst)
  (if (null? lst)
      '()
      (cons (f (head lst))
            (map* f (tail lst)))))

; 6. (filter p lst), която съставя нов списък, съдържащ само елементите на lst, за които е изпълнен предикатът p
(define (filter* p? lst)
  (cond [(null? lst) '()]
        [(p? (head lst)) (cons (head lst) (filter* p? (tail lst)))]
        [else (filter* p? (tail lst))]))

; 7. (push x lst), която добавя елемента x на края на списъка lst
(define (push x lst)
  (append lst (list x)))

; 8. (reverse lst), която връща списък с елементите на lst в обратен ред
(define (reverse*** lst)
  (foldl cons '() lst))

; 9. (insert x n lst), която вкарва елемента x на позиция
;    n в списъка lst (ако n е след края на l, вкарваме x накрая)
(define (insert x n lst)
  (cond [(null? lst) '()]
        [(and (null? lst) (not (= n 0))) (list x)]
        [(= n 0) (cons x (insert x (- n 1) lst))]
        [else (cons (head lst) (insert x (- n 1) (tail lst)))]))

; 10. (accumulate l op init), която пресмята (op l[0] (op l[1] (op l[2] ... (op l[n] init) ... )))
;     (ако имаме подаден празен списък, резултатът е init).
(define (accumulate* lst op init)
  (if (null? lst)
      init
      (op (head lst)
          (accumulate* (tail lst) op init))))
 
;----------------------------------------------








  





  
