#lang racket

(require racket/set)
(include "win.rkt") ; comment out this line later
(include "symmetry.rkt")

(struct gnode (val subtrees))

(define myturn #t)
(define (sign)
  (if myturn 1 -1))

(define (all-plays board)
  ;; takes a borad and returns a list of (list board lpp)
  ;; denoting all possible moves
  ;; lpp is for last-played position, used in win? function.
  ;(for/list ((i 64))
  (define (ap-h i)
    (cond ((= i 64) '())
          (else
           (define z (quotient i 16))
           (define y (quotient (remainder i 16) 4))
           (define x (remainder (remainder i 16) 4))
           ;(define board1 (board-copy board))
           ;(displayln board1)
           (if (= 0 (get-value board (list x y z)))
               (cons (set!-value board (list x y z) (sign))
                     (ap-h (+ i 1)))
               ;(list board1 (list x y z)))
               ; board1
               (ap-h (+ i 1))))))
  (ap-h 0))

;; takes a board and returns a gtree
(define (all-plays-2 board)
  (define sym (get-all-symmetric-vectors board))
  (define s (mutable-set))
  (define (ap2h)
    (for ((i 64))
    (define z (quotient i 16))
    (define y (quotient (remainder i 16) 4))
    (define x (remainder (remainder i 16) 4))
    (define board1 (board-copy board))
    ;(displayln board1)
    (if (= 0 (get-value board (list x y z)))
        (begin
          (set!-value board1 (list x y z) (sign))
          (if (not (set-member? sym board1))
              (begin
                (set-union! sym (get-all-symmetric-vectors board1))
                (set-add! s board1))
                ;(display i) (displayln ": sym is now") (displayln sym) (newline))
              (void)))
        (void))))
  (ap2h)
  (gnode board (set->list s)))

(define (score board-lpp)
  (if (apply win? board-lpp)
      (if myturn 10 -10)
      0))

(define (remove-all ele l)
  (cond ((null? l) '())
        ((equal? ele (car l)) (remove-all ele (cdr l)))
        (else (cons (car l) (remove-all ele (cdr l))))))

;; Takes 8s for depth 3
;; 0.08s for depth 2
;; without the remove-equivalents
(define (play-n-turns n bl)
  (if (= n 0)
      ;(void)
      ;(displayln (map score bl))
      bl
      (let ()
        (define new-bl
          ;(remove-equivalents
           ;(remove-all (void)
          (append*
           (map (lambda (ele)
                  (all-plays ele))
                bl)))
        ;(display-list new-bl)
        ;(define new-bl-score (map score new-bl))
        
        ;(displayln new-bl-score)
        (set! myturn (not myturn))
        (play-n-turns (- n 1) new-bl))))

(define (list-set->set-union ls)
  (define ret (mutable-set))
  (if (null? ls) (mutable-set)
      (begin
        (set-union! (car ls) (list-set->set-union (cdr ls)))
        (car ls))))

;; Takes about 50 s for depth 3;
;; 0.24 sec for depth 2.
;(define (play-n-turns-2 n board)
;  (if (= n 0)
;      board
;      ;(displayln (map score bl))
;      ;bs
;      (let ()
;        ;(define new-bs
;          (map (lambda (gn)
;                 (map (lambda (board)
;                        (play-n-turns (- n 1) 
;                      (gnode-subtrees gn))(map bl all-plays-2))
;        ;(display-list new-bl)
;        ;(define new-bl-score (map score new-bl))
;        
;        ;(displayln new-bl-score)
;        (set! myturn (not myturn))
;        (play-n-turns-2 (- n 1) new-bs))))

(define (display-list vec-lpp)
  (define i 0)
  (map (lambda (ele)
         (display "i=") (display i)
         (display "; lpp=") (display(cadr ele))
         (displayln "; board=")
         (display-board (car ele))
         (newline)
         (set! i (+ i 1)))
       vec-lpp)
  (newline))

(define (display-board board)
  (for ((i 4)) (displayln (vector-ref board i))))

(define (remove-equivalents l-board-lpp)
  ;(display "l-board-lpp: ") (display-list l-board-lpp) (newline)
  (cond ((null? l-board-lpp) l-board-lpp)
        (else
         (define all-mirrors (get-all-symmetric-vectors (caar l-board-lpp)))
         (define (re-h l-b-lpp)
           (cond ((null? l-b-lpp) '())
                 ((member (caar l-b-lpp) all-mirrors)
                  (re-h (cdr l-b-lpp)))
                 (else (cons (car l-b-lpp) (re-h (cdr l-b-lpp))))))
         (define removed-all-mirrors (re-h (cdr l-board-lpp)))
         (cons (car l-board-lpp) (remove-equivalents removed-all-mirrors)))))

(define (get-all-mirrors board)
  (define orig-space (make-object space% 2 #f 0))
  (send orig-space set!-space board)
  (list
   (send (mirror-space orig-space #t #f #f) get-space)
   (send (mirror-space orig-space #f #t #f) get-space)
   (send (mirror-space orig-space #f #f #t) get-space)
   (send (mirror-space orig-space #f #t #t) get-space)
   (send (mirror-space orig-space #t #f #t) get-space)
   (send (mirror-space orig-space #t #t #f) get-space)
   (send (mirror-space orig-space #t #t #t) get-space)))

(define (get-all-symmetric-vectors board)
  (mutable-set ; this may be a list or set
   (mirror-3d-vector board #t #f #f)
   (mirror-3d-vector board #f #t #f)
   (mirror-3d-vector board #f #f #t)
   (mirror-3d-vector board #f #t #t)
   (mirror-3d-vector board #t #f #t)
   (mirror-3d-vector board #t #t #f)
   (mirror-3d-vector board #t #t #t)))
;   (symmetric-3d-vector board '(x z y))
;   (symmetric-3d-vector board '(y x z))
;   (symmetric-3d-vector board '(z x y))
;   (symmetric-3d-vector board '(y z x))
;   (symmetric-3d-vector board '(x z y))))





;;;;;;;;;;;; PERFECT PLAY ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (expand expr)
  ;;; uses expr from the dictionary created by Patashnik
  (define zero (char->integer #\0))
  (define exp-l (string->list expr))
  (define (charList->integer l)
    (define (charList->digitList l)
      (cond ((null? l) '(#f))
            (else
             (define digit (- (char->integer (car l)) zero))
             (if (< -1 digit 10)
                 (cons digit (charList->digitList (cdr l)))
                 '(#f)))))
    (define (digitList->integer l)
      (cond ((null? l) 0)
            ((car l) (+ (car l) (* 10 (digitList->integer (cdr l)))))
            (else (digitList->integer (cdr l)))))
    (digitList->integer (reverse (charList->digitList l))))
  (define (from-next-char l)
    (cond ((null? l) '())
          (else
           (define digit (- (char->integer (car l)) zero))
           (if (< -1 digit 10)
              (from-next-char (cdr l))
              l))))
  (define (e-h l) ; expand helper
    ;(displayln l)
    (cond ((null? l) '())
          ((equal? (car l) #\x) (cons -1 (e-h (cdr l))))
          ((equal? (car l) #\o) (cons 1 (e-h (cdr l))))
          (else
           (append (make-list (charList->integer l) 0)
                   (e-h (from-next-char l))))))
  (define pre-board (e-h exp-l))
  ;(displayln pre-board)
  (define len (length pre-board))
  ;(displayln len)
  (define board-1d (append pre-board (make-list (- 64 len) 0)))
  (list->board board-1d))

(define (compress board) ; compresses for search within patashnik dictionary
  (define zero (char->integer #\0))
  (define l (board->list board))
  (define (zeroList->digitList l)
    (define (zeroList->integer l (n 0))
      (cond ((null? l) #f) ; there is no integer if there's no character at the end
            ((equal? (car l) 0) (zeroList->integer (cdr l) (+ n 1)))
            (else n)))
    (define (integer->digitList n)
      (cond ((not n) '())
            ((= 0 (quotient n 10)) (list (integer->char (+ zero (remainder n 10)))))
            (else (cons (integer->char (+ zero (quotient n 10)))
                        (integer->digitList (remainder n 10))))))
    (integer->digitList (zeroList->integer l)))
  (define (from-next-nonzero l)
    (cond ((null? l) '())
          ((equal? 0 (car l)) (from-next-nonzero (cdr l)))
          (else l)))
  (define (c-h l)
    (cond ((null? l) '())
          ((equal? (car l) -1) (cons #\x (c-h (cdr l))))
          ((equal? (car l) 1) (cons #\o (c-h (cdr l))))
          (else
           (append (zeroList->digitList l)
                   (c-h (from-next-nonzero l))))))
  (list->string (c-h l)))

(define (perfect-attack board)
  (define expr (compress board))
  ;(displayln expr)
  (define len (string-length expr))
  (define qubic-dict (open-input-file "qubic-dict.txt" #:mode 'text))
  (define (pa-h)
    (define line (read-line qubic-dict))
    (cond ((equal? line eof) #f)
          ((string-contains? line expr)
           (string->number (cadr (string-split line " "))))
          (else (pa-h))))
  
  (define pos (pa-h))
  (close-input-port qubic-dict)
  pos)
    
  
