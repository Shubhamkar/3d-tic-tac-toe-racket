;#lang racket

(require racket/set)
;(include "win.rkt") ; comment out this line later
;(include "symmetry.rkt")

(struct gnode (val subtrees) #:transparent)
(struct leaf (val) #:transparent)
(define (get-val node)
  (match node
    ((leaf val) val)
    ((gnode val subtrees) val)))

(define myturn #f)
(define (sign)
  (if myturn 1 -1))

(set! board (set!-value board '(0 0 0) -1))
(set! board (set!-value board '(1 1 1) -1))
(set! board (set!-value board '(2 2 2) -1))

;; play-n-turns is the main function
;; it uses the all-plays function to generate the play a move
;; and score function to evaluate that move

;; The win? function needs to be modified, perhaps,
;; coupled with the score function to evaluate partial success


(define (all-plays-2 leaf-b-lpp)
  ;; denoting all possible moves
  ;; lpp is for last-played position, used in win? function.
  (define (ap-h i)
    (cond ((= i 64) '())
          (else
           (define z (quotient i 16))
           (define y (quotient (remainder i 16) 4))
           (define x (remainder (remainder i 16) 4))
           (if (= 0 (get-value leaf-b-lpp (list x y z)))
               (cons (cons (set!-value board (list x y z) (sign)) i)
                     (ap-h (+ i 1)))
               (ap-h (+ i 1))))))
  (ap-h 0))

;; Expands a leaf into a gnode of depth 1
;; This function (sign) depends on myturn
(define (all-plays leaf-b-lpp)
  ;; denoting all possible moves
  ;; lpp is for last-played position, used in win? function.
  (define (ap-h i)
    (cond ((= i 64) '())
          (else
           (define z (quotient i 16))
           (define y (quotient (remainder i 16) 4))
           (define x (remainder (remainder i 16) 4))
           (if (= 0 (get-value (car (leaf-val leaf-b-lpp)) (list x y z)))
               (cons (leaf (cons (set!-value board (list x y z) (sign)) i))
                     (ap-h (+ i 1)))
               (ap-h (+ i 1))))))
  (define st (ap-h 0)) ; list of subtrees
  (gnode (leaf-val leaf-b-lpp) st))

(define (score board-lpp)
  (if (apply win? (list (car board-lpp)
                        (let ((i (cdr board-lpp)))
                          (define z (quotient i 16))
                          (define y (quotient (remainder i 16) 4))
                          (define x (remainder (remainder i 16) 4))
                          (list x y z))))
      (if myturn 10 -10)
      0))

(define (remove-all ele l)
  (cond ((null? l) '())
        ((equal? ele (car l)) (remove-all ele (cdr l)))
        (else (cons (car l) (remove-all ele (cdr l))))))


(define (play-n-turns-2 n bl)
  (if (= n 0)
      ;(void)
      ;(displayln (map score bl))
      bl
      (let ()
        (define new-bl
           (append*
            (map (lambda (ele)
                   (all-plays-2 (car ele)))
                 bl)))
        (set! myturn (not myturn))
        (play-n-turns-2 (- n 1) new-bl))))

(define (display-list vec-lpp)
  (define i 0)
  (map (lambda (ele)
         (display "i=") (display i)
         (display "; lpp=") (display (cadr ele))
         (displayln "; board=")
         (display-board (car ele))
         (newline)
         (set! i (+ i 1)))
       vec-lpp)
  (newline))

;; First goes to a depth of 1.
;; If there are any moves leading to a win, plays one of them randomly.


;; The implementation of minimax as its written is wrong
;; During the turn of the other player, paths that
;; the other player must take to prevent me from winning
;; have been ignored.

;; Changes are also needed in the win, score and all-plays
;; functions to make them state independent.
(define (play-n-turns n)
  (define orig-turn myturn)
;  (define depth-1 '())
;  (define win-list-1 '())
;  (define depth-2 '())
;  (define loss-list-2 '())
;  (define depth-3 '())
;  (define win-list-3 '())
;  (define depth-4 '())
  
  (define (increase-depth tree) ; increases the depth of the tree by 1
    (match tree
      ((leaf val) (all-plays tree))
      ((gnode val st) (gnode val (map increase-depth st)))))

  ;; The times at which score (and hence, has-losing-leaf
  ;; or has-winning-leaf) is called is important - myturn is the
  ;; state variable
  (define (has-losing-leaf? tree) ; returns #t if the tree has a winning tree
    ;; tree is supposed to be the gnode at depth 1
    (match tree
      ((leaf val) (= -10 (score val)))
      ((gnode val st) (ormap has-losing-leaf? st))))
  (define (has-winning-leaf? tree)
    (match tree
      ((leaf val) (= 10 (score val)))
      ((gnode val st) (ormap has-winning-leaf? st))))
  
  (define (remove-nodes-with-losing-leaves tree) ; assume tree is a gnode
    (gnode (gnode-val tree)
           (filter (lambda (ele) (not (has-losing-leaf? ele))) (gnode-subtrees tree))))
  (define (remove-nodes-with-winning-leaves tree) ; assume tree is a gnode
    (gnode (gnode-val tree)
           (filter (lambda (ele) (not (has-winning-leaf? ele))) (gnode-subtrees tree))))
  (define (keep-nodes-with-winning-leaves tree) ; assumes tree to be a gnode
    (gnode (gnode-val tree)
           (filter has-winning-leaf? (gnode-subtrees tree))))
  (define (keep-nodes-with-losing-leaves tree) ; assumes tree to be a gnode
    (gnode (gnode-val tree)
           (filter has-losing-leaf? (gnode-subtrees tree))))
  
  (define (minimax depth tree)
    (cond ((= depth n) ; maximum depth is reached
           (define mod-tree (if myturn
                                (keep-nodes-with-winning-leaves tree)
                                (remove-nodes-with-losing-leaves tree))) ; should this be remove?
           (if (null? (gnode-subtrees mod-tree))
                      (cdr (gnode-val (list-ref (gnode-subtrees tree)
                                                (random (length (gnode-subtrees tree))))))
                      (cdr (gnode-val (list-ref (gnode-subtrees mod-tree)
                                                (random (length (gnode-subtrees mod-tree))))))))
          (else
           (cond (myturn
                  (define mod-tree (keep-nodes-with-winning-leaves tree))
                  (if (null? (gnode-subtrees mod-tree))
                      (begin
                        (set! myturn (not myturn))
                        (minimax (+ depth 1) (increase-depth tree)))
                      ;; if I can win immediately, win
                      (cdr (get-val (list-ref (gnode-subtrees mod-tree) ; can also be leaf-val
                                                (random (length (gnode-subtrees mod-tree)))))))) 
                 ((not myturn)
                  (define mod-tree (remove-nodes-with-losing-leaves tree))
                  (if (null? (gnode-subtrees mod-tree))
                      ;; if all the moves result in my loss, play a move at random
                      (begin
                        (set! myturn (not myturn))
                        (minimax (+ depth 1) (increase-depth tree)))
                      (cdr (get-val (list-ref (gnode-subtrees mod-tree)
                                                (random (length (gnode-subtrees mod-tree)))))))))))) ; tree could be a modified-tree

  (define move (minimax 1 (increase-depth (leaf (cons board 0)))))
  (set! myturn orig-turn)
  (define z (quotient move 16))
  (define y (quotient (remainder move 16) 4))
  (define x (remainder (remainder move 16) 4))
  (list x y z))
  
;  (define (actual-move tree) ; if win is immediate, win!
;    (define temp-tree (keep-nodes-with-winning-leaves tree))
;    (if (null? (gnode-subtrees temp-tree))
;        #f ; an immediate win is not possible
;        (cdr (leaf-val (list-ref (gnode-subtrees temp-tree)
;                                 (random (length (gnode-subtrees temp-tree))))))))
;  (actual-move (increase-depth (leaf board last-played-pos)))

  ;; During my turn, if I cannot win, I'll not choose a move
  ;; that can result in my opponent's win.
  ;; Similarly, during the opponent's turn, if the opponent
  ;; cannot win, s/he will not choose a move that will result in my win.
  ;; Thus, at depth-1, we remove all nodes, that result in our defeat at depth-2.
  ;; At depth-2, 
  
;  (define (pnt2-h n tree)
;    (if (= n 0) tree
;        (begin
;          (define temp-tree  (increase-depth tree))
;          (cond (myturn
;                 (define temp2-tree (keep-nodes-with-winning-leaves temp-tree))
;                 (cond ((null? temp2-tree)
;                        (set! myturn (not myturn))
;                        (pnt2-h (- n 1) temp-tree))
;                       (else temp2-tree)))
;                ((not myturn)
;                 ())
;                (else (error "Cases are not exhaustive."))))
;  
;  (define (pnt-h n)
;    (cond ((= n 4) ;; this is the move of the greatest practical depth
;           (set! depth-4 (increase-depth depth-3))
;           (set! depth-4 (remove-nodes-with-losing-leaves depth-4))
;           (if (null? (gnode-subtrees depth-4))
;               (error "Situation not considered for your forced win")
;               (cdr (gnode-val (list-ref (gnode-subtrees depth-4) (random (length (gnode-subtrees depth-4))))))))
;          ((= n 3)
;           (set! depth-3 (increase-depth depth-2))
;           (set! depth-3 (remove-nodes-with-winning-leaves depth-3))
;           ;; During the previous turn (opponent's turn),
;           ;; the opponent would never play a move
;           ;; that will allow me to win at this turn
;           ;(set! win-list-3 (keep-nodes-with-winning-leaves depth-3))
;           (set! myturn (not myturn))
;           (if (null? (gnode-subtrees depth-3))
;               (error "No playable moves")
;               (pnt-h (+ n 1))))
;          ((= n 2) ; opponent's turn
;           (set! depth-2 (increase-depth depth-1))
;           (set! depth-2
;                 (remove-nodes-with-losing-leaves depth-2))
;           (set! myturn (not myturn))
;           (if (null? (gnode-subtrees depth-2))
;               (error "Situation not considered for your forced win")
;               ; perhaps, play a random move from depth-1
;               (pnt-h (+ n 1))))
;          ((= n 1) ; myturn - ensure that play-n-turns is called only
;           ;when it is my turn
;           (set! depth-1 (increase-depth (leaf (cons board 0))))
;;           (map (lambda (ele)
;;                  (if (= 10 (score (leaf-val ele)))
;;                      (set! win-list-1 (cons (leaf-val ele) win-list-1))
;;                      (void)))
;;                (gnode-subtrees depth-1))
;           (set! win-list-1 (keep-nodes-with-winning-leaves depth-1))
;           (set! myturn (not myturn))
;           (if (null? win-list-1)
;               (pnt-h (+ n 1))
;               (cdr (leaf-val (list-ref win-list-1 (random (length win-list-1)))))))))
;  (pnt-h 1))
;
;(define (minimax)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ABANDONED CODE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;


;(define (list-set->set-union ls)
;  (define ret (mutable-set))
;  (if (null? ls) (mutable-set)
;      (begin
;        (set-union! (car ls) (list-set->set-union (cdr ls)))
;        (car ls))))

;;; Takes about 50 s for depth 3;
;;; 0.24 sec for depth 2.
;;(define (play-n-turns-2 n board)
;;  (if (= n 0)
;;      board
;;      ;(displayln (map score bl))
;;      ;bs
;;      (let ()
;;        ;(define new-bs
;;          (map (lambda (gn)
;;                 (map (lambda (board)
;;                        (play-n-turns (- n 1) 
;;                      (gnode-subtrees gn))(map bl all-plays-2))
;;        ;(display-list new-bl)
;;        ;(define new-bl-score (map score new-bl))
;;        
;;        ;(displayln new-bl-score)
;;        (set! myturn (not myturn))
;;        (play-n-turns-2 (- n 1) new-bs))))
;
;(define (remove-equivalents l-board-lpp)
;  ;(display "l-board-lpp: ") (display-list l-board-lpp) (newline)
;  (cond ((null? l-board-lpp) l-board-lpp)
;        (else
;         (define all-mirrors (get-all-symmetric-vectors (caar l-board-lpp)))
;         (define (re-h l-b-lpp)
;           (cond ((null? l-b-lpp) '())
;                 ((member (caar l-b-lpp) all-mirrors)
;                  (re-h (cdr l-b-lpp)))
;                 (else (cons (car l-b-lpp) (re-h (cdr l-b-lpp))))))
;         (define removed-all-mirrors (re-h (cdr l-board-lpp)))
;         (cons (car l-board-lpp) (remove-equivalents removed-all-mirrors)))))
;
;
;(define (get-all-mirrors board)
;  (define orig-space (make-object space% 2 #f 0))
;  (send orig-space set!-space board)
;  (list
;   (send (mirror-space orig-space #t #f #f) get-space)
;   (send (mirror-space orig-space #f #t #f) get-space)
;   (send (mirror-space orig-space #f #f #t) get-space)
;   (send (mirror-space orig-space #f #t #t) get-space)
;   (send (mirror-space orig-space #t #f #t) get-space)
;   (send (mirror-space orig-space #t #t #f) get-space)
;   (send (mirror-space orig-space #t #t #t) get-space)))
;
;
;(define (get-all-symmetric-vectors board)
;  (list ; this may be a list or set
;   (mirror-3d-vector board #t #f #f)
;   (mirror-3d-vector board #f #t #f)
;   (mirror-3d-vector board #f #f #t)
;   (mirror-3d-vector board #f #t #t)
;   (mirror-3d-vector board #t #f #t)
;   (mirror-3d-vector board #t #t #f)
;   (mirror-3d-vector board #t #t #t)))
;;   (symmetric-3d-vector board '(x z y))
;;   (symmetric-3d-vector board '(y x z))
;;   (symmetric-3d-vector board '(z x y))
;;   (symmetric-3d-vector board '(y z x))
;;   (symmetric-3d-vector board '(x z y))))
;
;;; takes a board and returns a gtree
;(define (all-plays-2 board)
;  (define sym (get-all-symmetric-vectors board))
;  (define s (mutable-set))
;  (define (ap2h)
;    (for ((i 64))
;    (define z (quotient i 16))
;    (define y (quotient (remainder i 16) 4))
;    (define x (remainder (remainder i 16) 4))
;    (define board1 (board-copy board))
;    ;(displayln board1)
;    (if (= 0 (get-value board (list x y z)))
;        (begin
;          (set!-value board1 (list x y z) (sign))
;          (if (not (set-member? sym board1))
;              (begin
;                (set-union! sym (get-all-symmetric-vectors board1))
;                (set-add! s board1))
;                ;(display i) (displayln ": sym is now") (displayln sym) (newline))
;              (void)))
;        (void))))
;  (ap2h)
;  (gnode board (set->list s)))
;
;(define (expand expr)
;  ;;; uses expr from the dictionary created by Patashnik
;  (define zero (char->integer #\0))
;  (define exp-l (string->list expr))
;  (define (charList->integer l)
;    (define (charList->digitList l)
;      (cond ((null? l) '(#f))
;            (else
;             (define digit (- (char->integer (car l)) zero))
;             (if (< -1 digit 10)
;                 (cons digit (charList->digitList (cdr l)))
;                 '(#f)))))
;    (define (digitList->integer l)
;      (cond ((null? l) 0)
;            ((car l) (+ (car l) (* 10 (digitList->integer (cdr l)))))
;            (else (digitList->integer (cdr l)))))
;    (digitList->integer (reverse (charList->digitList l))))
;  (define (from-next-char l)
;    (cond ((null? l) '())
;          (else
;           (define digit (- (char->integer (car l)) zero))
;           (if (< -1 digit 10)
;              (from-next-char (cdr l))
;              l))))
;  (define (e-h l) ; expand helper
;    ;(displayln l)
;    (cond ((null? l) '())
;          ((equal? (car l) #\x) (cons -1 (e-h (cdr l))))
;          ((equal? (car l) #\o) (cons 1 (e-h (cdr l))))
;          (else
;           (append (make-list (charList->integer l) 0)
;                   (e-h (from-next-char l))))))
;  (define pre-board (e-h exp-l))
;  ;(displayln pre-board)
;  (define len (length pre-board))
;  ;(displayln len)
;  (define board-1d (append pre-board (make-list (- 64 len) 0)))
;  (list->board board-1d))
;
;(define (compress board) ; compresses for search within patashnik dictionary
;  (define zero (char->integer #\0))
;  (define l (board->list board))
;  (define (zeroList->digitList l)
;    (define (zeroList->integer l (n 0))
;      (cond ((null? l) #f) ; there is no integer if there's no character at the end
;            ((equal? (car l) 0) (zeroList->integer (cdr l) (+ n 1)))
;            (else n)))
;    (define (integer->digitList n)
;      (cond ((not n) '())
;            ((= 0 (quotient n 10)) (list (integer->char (+ zero (remainder n 10)))))
;            (else (cons (integer->char (+ zero (quotient n 10)))
;                        (integer->digitList (remainder n 10))))))
;    (integer->digitList (zeroList->integer l)))
;  (define (from-next-nonzero l)
;    (cond ((null? l) '())
;          ((equal? 0 (car l)) (from-next-nonzero (cdr l)))
;          (else l)))
;  (define (c-h l)
;    (cond ((null? l) '())
;          ((equal? (car l) -1) (cons #\x (c-h (cdr l))))
;          ((equal? (car l) 1) (cons #\o (c-h (cdr l))))
;          (else
;           (append (zeroList->digitList l)
;                   (c-h (from-next-nonzero l))))))
;  (list->string (c-h l)))
;
;(define (perfect-attack board)
;  (define expr (compress board))
;  ;(displayln expr)
;  (define len (string-length expr))
;  (define qubic-dict (open-input-file "qubic-dict.txt" #:mode 'text))
;  (define (pa-h)
;    (define line (read-line qubic-dict))
;    (cond ((equal? line eof) #f)
;          ((string-contains? line expr)
;           (string->number (cadr (string-split line " "))))
;          (else (pa-h))))
;  
;  (define pos (pa-h))
;  (close-input-port qubic-dict)
;  pos)
;
;
;  
