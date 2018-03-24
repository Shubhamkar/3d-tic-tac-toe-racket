#lang racket

;;; a demo of a function that check of a winning position is formed.

(define board (make-list 4 (make-list 4 (make-list 4 0))))
; unset state is 0; other states are -1 and 1.

(define (get-value pos)
  ;; pos - a list of x y z coordinates, in that order
  (list-ref (list-ref (list-ref board (car pos)) (cadr pos)) (caddr pos)))

(define (win? player)
  ; returns #t if a player has won.
  (define (check-eqv init update)
    ; checks if a winning position is attained by the positions given
    ; by the update method
    (define l-coord
      (until (lambda (x) (border-reached? (car x)))
             (lambda (x) (cons (update (car x)) x))
             (list init))))
  (define (helper)
    ; the main function of win?
    
    ; An example of update function - to be passed to the check-eqv function
    ; (define (update pos) (cons (+ (car pos) 1) (cdr pos)))

    ;; For a moment, it may feel like the way these functions operate
    ;; depend on how the board is implemented - as a vector or list
    ;; However, the act of retrieving the element is handled by the get-value function;
    ;; the coordinates can be represented in any convenient format
    ;; THIS MAY BE A BOTTLENECK without using arrays
    (define (incr-x pos) (cons (+ (car pos) 1) (cdr pos)))
    (define (incr-y pos) (cons (car pos) (cons (+ (cadr pos) 1) (cddr pos))))
    (define (incr-z pos) (cons (car pos) (cons (cadr pos) (cons (+ (caddr pos) 1) '()))))
    (define (incr-xy pos) (cons (+ 1 (car pos)) (cons (+ 1 (cadr pos)) (cddr pos))))
    (define (incr-zx pos) (cons (+ 1 (car pos)) (cons (cadr pos) (cons (+ (caddr pos) 1) '()))))
    (define (incr-yz pos) (cons (car pos) (cons (+ 1 (cadr pos)) (cons (+ (caddr pos) 1) '()))))
    (define (incr-xyz pos) (cons (+ 1 (car pos)) (cons (+ 1 (cadr pos)) (cons (+ (caddr pos) 1) '()))))
    ;; Sure, this can be compressed into a single function, and it would be easy
    ;; but is it more useful?
    ;; It seems it would be: x can be increased or decreased, alongside y.
    )
  (define (in-a-line? l)
    ; takes in a list of coordinates and checks if they form a complete line
    ; currently does not check if the position is unset.
    (apply = (map get-value l)))
  (define (border-reached? pos)
    ; Returns #t if pos is an invalid (in a greater sense) coordinate
    (or (< 2 (car pos)) (< 2 (cadr pos)) (< 2 (caddr pos)))))

;; ================== SOME HIGHER ORDER FUNCTIONS =========================
(define (until p f x)
  ;;; applies f to p until (p x) returns true
  (if (p x) x (until p f (f x))))


  