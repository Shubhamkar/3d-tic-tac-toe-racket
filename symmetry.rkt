;#lang racket

;(include "win.rkt")

(define space%
  (class object%
    (super-new)
    (init max-coord)
    (init include-axes?)
    (init initial-value)
    ;; with origin as the center, makes a 3d-vector, with
    ;; max coordiante in each direction as max-coord
    (define r (+ (* 2 max-coord) (if include-axes? 1 0)))
    (define actual-space (make-3d-vector r r r initial-value))
    (define axes-included include-axes?)
    
    (define/public (set!-value pos val)
      (define x 0)
      (define y 0)
      (define z 0)
      (cond ((list? pos)
             (set! x (car pos))
             (set! y (cadr pos))
             (set! z (caddr pos)))
            (else (error "pos should be a list of coordinates (x y z): " pos)))
      (3d-vector-set! actual-space
                      (+ (quotient r 2) x (if (< x 0) 0
                                              (if axes-included 0 -1)))
                      (+ (quotient r 2) y (if (< y 0) 0
                                              (if axes-included 0 -1)))
                      (+ (quotient r 2) z (if (< z 0) 0
                                              (if axes-included 0 -1)))
                      val))
    
    (define/public (get-value pos)
      (define x 0)
      (define y 0)
      (define z 0)
      (cond ((list? pos)
             (set! x (car pos))
             (set! y (cadr pos))
             (set! z (caddr pos)))
            (else (error "pos should be a list of coordinates (x y z): " pos)))
      (3d-vector-ref actual-space
                     (+ (quotient r 2) x (if (< x 0) 0
                                             (if axes-included 0 -1)))
                     (+ (quotient r 2) y (if (< y 0) 0
                                             (if axes-included 0 -1)))
                     (+ (quotient r 2) z (if (< z 0) 0
                                             (if axes-included 0 -1)))))
    (define/public (get-space)
      actual-space)
    (define/public (set!-space vec)
      (set! actual-space (3d-vector-copy vec)))))

(define (mirror-space space mirror-x mirror-y mirror-z)
  (define x-fac (if mirror-x -1 1))
  (define y-fac (if mirror-y -1 1))
  (define z-fac (if mirror-z -1 1))
  (define space1 (make-object space% 2 #f 0))
  (for ((i 5))
    (define x1 (- i 2))
    (for ((j 5))
      (define y1 (- j 2))
      (for ((k 5))
        (define z1 (- k 2))
        (if (or (= x1 0) (= y1 0) (= z1 0)) (void)
            (send space1 set!-value (list x1 y1 z1)
                  (send space get-value
                        (list (* x-fac x1) (* y-fac y1) (* z-fac z1))))))))
  space1)

;(define (mirror-3d-vector vec mirror-x mirror-y mirror-z)
;  (define x-fac (if mirror-x -1 1))
;  (define y-fac (if mirror-y -1 1))
;  (define z-fac (if mirror-z -1 1))
;  (define vec1 (make-3d-vector 4 4 4 0))
;  (for ((i '(-3 -1 1 3)))
;    (define x1 (/ (+ (* i x-fac) 3) 2))
;    (define x (/ (+ i 3) 2))
;    (for ((j'(-3 -1 1 3)))
;      (define y1 (/ (+ (* j y-fac) 3) 2))
;      (define y (/ (+ j 3) 2))
;      (for ((k '(-3 -1 1 3)))
;        (define z1 (/ (+ (* k z-fac) 3) 2))
;        (define z (/ (+ k 3) 2))
;        ;(if (or (= x1 0) (= y1 0) (= z1 0)) (void)
;            (3d-vector-set! vec1 x1 y1 z1
;                            (3d-vector-ref vec
;                                           x y z)))))
;  vec1)

(define (mirror-3d-vector vec mirror-x mirror-y mirror-z)
  (define x-fac (if mirror-x -1 1))
  (define y-fac (if mirror-y -1 1))
  (define z-fac (if mirror-z -1 1))
  (define vec1 0)
  (for ((i '(-3 -1 1 3)))
    (define x1 (/ (+ (* i x-fac) 3) 2))
    (define x (/ (+ i 3) 2))
    (for ((j'(-3 -1 1 3)))
      (define y1 (/ (+ (* j y-fac) 3) 2))
      (define y (/ (+ j 3) 2))
      (for ((k '(-3 -1 1 3)))
        (define z1 (/ (+ (* k z-fac) 3) 2))
        (define z (/ (+ k 3) 2))
        ;(if (or (= x1 0) (= y1 0) (= z1 0)) (void)
        ;(displayln vec1)
        ;(display "(x1 y1 z1): ") (displayln (list x1 y1 z1))
        ;(display "(x y z): ") (displayln (list x y z))
        ;(print-board vec1)
        (set! vec1 (set!-value vec1 (list x1 y1 z1)
                               (get-value vec
                                          (list x y z)))))))
  vec1)



(define (symmetric-3d-vector vec l)
  (define vec1 (make-3d-vector 4 4 4 0))
  (match l
    ('(y x z) 
     (for ((i '(-3 -1 1 3)))
       (define x (/ (+ i 3) 2))
       (for ((j'(-3 -1 1 3)))
         (define y (/ (+ j 3) 2))
         (for ((k '(-3 -1 1 3)))
           (define z (/ (+ k 3) 2))
           ;(if (or (= x1 0) (= y1 0) (= z1 0)) (void)
           (3d-vector-set! vec1 x y z
                           (3d-vector-ref vec
                                          y x z))))))
    ('(x z y) 
     (for ((i '(-3 -1 1 3)))
       (define x (/ (+ i 3) 2))
       (for ((j'(-3 -1 1 3)))
         (define y (/ (+ j 3) 2))
         (for ((k '(-3 -1 1 3)))
           (define z (/ (+ k 3) 2))
           ;(if (or (= x1 0) (= y1 0) (= z1 0)) (void)
           (3d-vector-set! vec1 x y z
                           (3d-vector-ref vec
                                          x z y))))))
    ('(z y x) 
     (for ((i '(-3 -1 1 3)))
       (define x (/ (+ i 3) 2))
       (for ((j'(-3 -1 1 3)))
         (define y (/ (+ j 3) 2))
         (for ((k '(-3 -1 1 3)))
           (define z (/ (+ k 3) 2))
           ;(if (or (= x1 0) (= y1 0) (= z1 0)) (void)
           (3d-vector-set! vec1 x y z
                           (3d-vector-ref vec
                                          z y x))))))
    ('(z x y) 
     (for ((i '(-3 -1 1 3)))
       (define x (/ (+ i 3) 2))
       (for ((j'(-3 -1 1 3)))
         (define y (/ (+ j 3) 2))
         (for ((k '(-3 -1 1 3)))
           (define z (/ (+ k 3) 2))
           ;(if (or (= x1 0) (= y1 0) (= z1 0)) (void)
           (3d-vector-set! vec1 x y z
                           (3d-vector-ref vec
                                          x y z))))))
    ('(y z x) 
     (for ((i '(-3 -1 1 3)))
       (define x (/ (+ i 3) 2))
       (for ((j'(-3 -1 1 3)))
         (define y (/ (+ j 3) 2))
         (for ((k '(-3 -1 1 3)))
           (define z (/ (+ k 3) 2))
           ;(if (or (= x1 0) (= y1 0) (= z1 0)) (void)
           (3d-vector-set! vec1 x y z
                           (3d-vector-ref vec
                                          y z x)))))))
  vec1)

;(define space1 (make-object space% 2 #f 0))
;(send space1 set!-value (list -2 -2 -2) 1)
;(define space2 (mirror-space space1 #f #t #t))