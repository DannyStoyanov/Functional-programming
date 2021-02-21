#lang racket
(require racket/stream rackunit rackunit/gui racket/include)

; Домашно №2
; Даниел Здравков Стоянов 45574
; Специалност: Информатика
; Курс: 3
; Група: 1

(require "tree.rkt")

(define str:<trivial-tree> "*")
(define str:<leaf-tree> "{122 * *}") 
(define str:<correct-tree> "{5 {64 * *} {9 * {13 * *}} }")

(define str:<incorrect-number-of-elements-1> "{5 6 {*}}")
(define str:<incorrect-number-of-elements-2> "{5* }")
(define str:<incorrect-number-of-elements-4> "{5***}")
(define str:<incorrect-number-of-elements-5> "{5***{4*5*6*7}*890}")
     
(define str:<incorrect-elements-1> "5 6 *")        
(define str:<incorrect-elements-2> "{5 {6 **} 7}")
(define str:<incorrect-elements-3> "{5 * {***} }")
(define str:<incorrect-elements-4> "{ 5{s **}*}")

(define str:<incorrect-1> "{}")
(define str:<incorrect-2> "{*}")
(define str:<incorrect-3> "")

(define (tree->treestring tree)
  (define (helper lst res)
    (cond [(null? lst) res]
          [(number? (head lst)) (helper (tail lst) (string-append res (number->string (head lst))))]
          [(list? (head lst)) (helper (tail lst) (string-append res " " (tree->treestring-wrapper (head lst))))]
          [else (helper (tail lst) (string-append res " () "))]
          )
    )
  (helper tree "")
  )

(define (correct-count-elements str)
    (test-true
     (string-append "The following string " str " has 3 elements")
     (count-elements 0 str)))

(define (incorrect-count-elements str)
    (test-false
     (string-append "The following string " str " doesn't have the right amount of elements")
     (count-elements 0 str)))

(define (tree->treestring-wrapper tree)
  (string-append "(" (tree->treestring tree) ")"))

(define (correct-tree? str)
  (test-true
   (string-append "The following string " str " is correct tree")
   (tree? str)))

(define (incorrect-tree? str)
  (test-false
   (string-append "The following string " str " is incorrect tree")
   (tree? str)))

(define (correct-string->tree str res)
  (test-true
   (string-append "The following string " str " represents the tree '" (tree->treestring-wrapper res) " correctly")
   (equal? (string->tree str) res)))

(define (incorrect-string->tree str)
  (test-false
   (string-append "The following string " str " should fail on phase validation")
   (string->tree str)))

(define (correct-balanced-tree tree)
  (test-true
   (string-append "The following tree " tree " should be balanced")
   (balanced? (string->tree tree))))

(define (incorrect-balanced-tree tree)
  (test-false
   (string-append "The following tree " tree " should NOT be balanced")
   (balanced? (string->tree tree))))

(define (correct-ordered-tree tree)
  (test-true
   (string-append "The following tree " tree " should be ordered")
   (ordered? (string->tree tree))))

(define (incorrect-ordered-tree tree)
  (test-false
   (string-append "The following tree " tree " should NOT be ordered")
   (ordered? (string->tree tree))))

(define (correct-tree->string tree res)
  (test-true
   (string-append "The following tree '" (tree->treestring-wrapper tree) " represents the string " res " correctly")
   (equal? (tree->string tree) res)))

(define (correct-tree->stream tree res order)
  (test-true
   (cond[(equal? order 'inorder)
         (string-append "Inorder " tree " and " (tree->treestring-wrapper res) " should be equal")]
        [(equal? order 'preorder)
         (string-append "Preorder " (tree->treestring-wrapper (string->tree tree)) " and " (tree->treestring-wrapper res) " should be equal")]
        [(equal? order 'postorder)
        (string-append "Preorder " (tree->treestring-wrapper (string->tree tree)) " and " (tree->treestring-wrapper res) " should be equal")]
        [else (string-append "Error!")])
   (equal? (tree->stream (string->tree tree) order) res)))

(define (incorrect-tree->stream tree)
  (test-false
   (string-append "The following tree " tree " should fail on phase validation")
   (tree->stream (string->tree tree) 'inorder))) ; няма значение реда, защото така и не се стига до него


(test/gui
 
 (test-suite
  "find-next"
  (test-eqv? "First element is element itself" (find-next 0 str:<trivial-tree>) 0)
  (test-eqv? "Distinguish whitespaces from element" (find-next 2 str:<correct-tree>) 3)
  )

 (test-suite
  "skip-subtree"
  (test-eqv? "Subtree representing a leaf" (skip-subtree str:<correct-tree> 3) 11)
  (test-eqv? "Subtree with right empty tree and left subtree" (skip-subtree str:<correct-tree> 12) 26)
  )

 (test-suite
  "count-elements"
  (correct-count-elements str:<correct-tree>)

  (incorrect-count-elements str:<incorrect-number-of-elements-1>)
  (incorrect-count-elements str:<incorrect-number-of-elements-2>)
  (incorrect-count-elements str:<incorrect-number-of-elements-4>)
  (incorrect-count-elements str:<incorrect-number-of-elements-5>)
  )

 (test-suite
  "tree?"
  
  (test-case
   "String expression is correct tree"
   (correct-tree? str:<trivial-tree>)
   (correct-tree? str:<correct-tree>)
   )

  (test-case
   "String expression is incorrect tree"
   (incorrect-tree? str:<incorrect-number-of-elements-1>)
   (incorrect-tree? str:<incorrect-number-of-elements-2>)
   (incorrect-tree? str:<incorrect-number-of-elements-4>)
   (incorrect-tree? str:<incorrect-number-of-elements-5>)
     
   (incorrect-tree? str:<incorrect-elements-1>)
   (incorrect-tree? str:<incorrect-elements-2>)
   (incorrect-tree? str:<incorrect-elements-3>)
   (incorrect-tree? str:<incorrect-elements-4>)

   (incorrect-tree? str:<incorrect-1>)
   (incorrect-tree? str:<incorrect-2>)
   (incorrect-tree? str:<incorrect-3>)
   )
  )
 
 (test-suite
  "string->tree"
  
  (let [
        (list:<trivial-tree> '() ) ; "*"
        
        (str:<correct-leaf> "  {  5 * *}")
        (list:<correct-leaf> '(5 () ()) )
        
        (str:<correct-tree-1> "  {  5 {6 **}  { 7 * {8 ** }} }")
        (list:<correct-tree-1> '(5 (6 () ()) (7 () (8 () ()))) )
        
        (str:<correct-tree-2> "  {5 {44 **} {55**}}")
        (list:<correct-tree-2> '(5 (44 () ()) (55 () ())) )
        
        (str:<correct-tree-3> "  {5 {44 {48**}*} {55*{122 **}}}")
        (list:<correct-tree-3> '(5 (44 (48 () ()) ()) (55 () (122 () ()))) )
        ]
    
    (test-case
     "The string should fail on phase validation" 
     (incorrect-string->tree str:<incorrect-number-of-elements-1>)
     (incorrect-string->tree str:<incorrect-number-of-elements-2>)
     (incorrect-string->tree str:<incorrect-number-of-elements-4>)
     (incorrect-string->tree str:<incorrect-number-of-elements-5>)
     
     (incorrect-string->tree str:<incorrect-elements-1>)
     (incorrect-string->tree str:<incorrect-elements-2>)
     (incorrect-string->tree str:<incorrect-elements-3>)
     (incorrect-string->tree str:<incorrect-elements-4>)

     (incorrect-string->tree str:<incorrect-1>)
     (incorrect-string->tree str:<incorrect-2>)
     (incorrect-string->tree str:<incorrect-3>)
     )

    (test-case
     "The string is correct representation of a tree"
     (correct-string->tree str:<trivial-tree> list:<trivial-tree>) "*"
     (correct-string->tree "    *           " list:<trivial-tree>)
     (correct-string->tree str:<correct-leaf> list:<correct-leaf>)
     (correct-string->tree str:<correct-tree-1> list:<correct-tree-1>)
     (correct-string->tree str:<correct-tree-2> list:<correct-tree-2>)
     (correct-string->tree str:<correct-tree-3> list:<correct-tree-3>)
     (correct-string->tree "   {       122 * {3 * *}} " '(122 () (3 () ())))
     )
    
    )
  )

 (test-suite
  "balanced?"
  
  (test-case
   "Following trees should be balanced"
   (correct-balanced-tree str:<trivial-tree>) ; "*"
   (correct-balanced-tree str:<leaf-tree>)    ; "{1 * *}"
   (correct-balanced-tree str:<correct-tree>) ; "{5 {64 * *} {9 * {13 * *}} }" 
   (correct-balanced-tree "{1 {2 * *} {4 * {5 * *}} }")
   )

  (test-case
   "Following trees should NOT be balanced"
   (incorrect-balanced-tree "{5 {5 {5 {5 * *} *} *} *}")
   (incorrect-balanced-tree "{5 {2 * *} {3 * {6 {4 * *} *}} }")
   )
  )
  
 (test-suite
  "ordered?"
  
  (test-case
   "Following trees should be ordered"
   (correct-ordered-tree str:<trivial-tree>) ; "*"
   (correct-ordered-tree str:<leaf-tree>)    ; "{1 * *}"
   (correct-ordered-tree "{9 {4 {1 * *} {6 * *}} {12 {10 * *} {18 * *}} }")
   (correct-ordered-tree "{9 {9 * *} *}")       ; root <= root (left subtree) is correct
   (correct-ordered-tree "{9 {9 {9 * *} *} *}") ; root <= root (left subtree) is correct
   )
  
  (test-case
   "Following trees should NOT be ordered"
   (incorrect-ordered-tree "{8 {3 * *} {5 * *}")                      ; 5 is less than 8 but it is part of his right subtree
   (incorrect-ordered-tree "{5 {3 {1 * *} {7 {2 * *} *}} {16 * *} }") ; 7 is bigger than 5 but it is part of his left subtree
   (incorrect-ordered-tree "{8 {3 * *} {9 {6 * *} *} }")              ; 6 is less than 8 but it is part of his right subtree
   (incorrect-ordered-tree "{9 * {9 * {9 * *}} }")  ; root = root (right subtree) is incorrect
   (incorrect-ordered-tree "{9 * {10 {10 * {12 * *}} *} }")  ; root = root (right subtree) is incorrect
   )
  )

 (test-suite
  "tree->string"

  (let [
        (list:<trivial-tree> '() )       ; "*"
        (list:<leaf-tree> '(122 () ()) ) ; "{122 * *}"
        
        (str:<correct-tree-1> "{5 {6 * *} {7 * {8 * *}}}")
        (list:<correct-tree-1> '(5 (6 () ()) (7 () (8 () ()))) )
        
        (str:<correct-tree-2> "{5 {44 * *} {55 * *}}")
        (list:<correct-tree-2> '(5 (44 () ()) (55 () ())) )
        
        (str:<correct-tree-3> "{5 {44 {48 * *} *} {55 * {122 * *}}}")
        (list:<correct-tree-3> '(5 (44 (48 () ()) ()) (55 () (122 () ()))) )
        ]
  
    (test-case
     "The tree is correct representation of the string"
     (correct-tree->string list:<trivial-tree> str:<trivial-tree>)    
     (correct-tree->string list:<leaf-tree> str:<leaf-tree>)         
     (correct-tree->string list:<correct-tree-1> str:<correct-tree-1>)
     (correct-tree->string list:<correct-tree-3> str:<correct-tree-3>)
     )
    
    )
  )

 (test-suite
  "tree->stream"

  (test-case
   "Correct converting tree to stream"
   (correct-tree->stream "*" '() 'inorder)
   (correct-tree->stream "{1 {2 {4 * *} {5 * *}} {3 * *}}" '(4 2 5 1 3) 'inorder)
   (correct-tree->stream "{1 {2 {4 * *} {5 * *}} {3 * *}}" '(1 2 4 5 3) 'preorder)
   (correct-tree->stream "{1 {2 {4 * *} {5 * *}} {3 * *}}" '(4 5 2 3 1) 'postorder)
   )
  
  (test-case
   "Should fail on phase validation of the tree"
   (incorrect-tree->stream "{1 {2 {4 * *} {5 * *}} {3 * * *} 5}")
   (incorrect-tree->stream "{5 5 *}")
   (incorrect-tree->stream "{   *   }")
   (incorrect-tree->stream "{}")
   (incorrect-tree->stream "{***}")
   )
  )
  
 )