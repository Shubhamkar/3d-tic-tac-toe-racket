#lang racket

(include "minimax.rkt")

(define (increase-depth tree) ; increases the depth of the tree by 1
    (match tree
      ((leaf val) (all-plays tree))
      ((gnode val st) (gnode val (map increase-depth st)))))
(define (keep-nodes-with-winning-leaves tree) ; assumes tree to be a gnode
    (gnode (gnode-val tree)
           (filter has-winning-leaf? (gnode-subtrees tree))))
(define (has-winning-leaf? tree)
  (match tree
    ((leaf val) (= 10 (score val)))
    ((gnode val st) (ormap has-winning-leaf? st))))

(define depth-2 (increase-depth (increase-depth (leaf (cons board 0)))))
(define depth-3 (increase-depth depth-2))

(define search-object (set!-value (set!-value board '(2 2 0) 1) '(2 3 0) 1))
(define search-object-2 (set!-value board '(2 2 0) 1))

(define (search-tree tree obj)
  (cond 
    ((leaf? tree) (if (= (car (leaf-val tree)) obj) tree #f))
    ((gnode? tree)
     (define (search-list l)
       (displayln l)
       (cond ((null? l) #f)
             ((search-tree (car (gnode-subtrees tree)) obj)
              (search-tree (car (gnode-subtrees tree)) obj))
             (else (search-list (cdr l)))))
     (cond ((= (car (gnode-val tree)) obj) tree)
           (else (search-list (gnode-subtrees tree))))))) ; n for node

(define (display-st-board gn)
  (map (lambda (n) (display-board (car (get-val n)))) (gnode-subtrees gn)))

(define (display-st-val gn)
  (map (lambda (n) (get-val n)) (gnode-subtrees gn)))