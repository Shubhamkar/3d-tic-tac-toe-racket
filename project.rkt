#lang racket/gui
(require racket/gui/base)	

; Make a frame by instantiating the frame% class
(define main-window (new frame% [label "3D Tic Tac Toe"]))

; Show the frame by calling its show method
(send main-window show #t)

(define msg-area (new message% [parent main-window]
                          [label "Welcome!"]))

(define restart-btn
  (new button% [parent main-window]
             [label "Restart"]
             ; Callback procedure for a button click:
             [callback (lambda (button event)
                         ;(send msg-area set-label "The game has been restarted."))]))
                         (send restart-confirm-window show #t))]))

;;------------------ Restart Confirmation Dialog Box --------------

(define restart-confirm-window
  (new frame% (label "Restart?")))

(define restart-confirm-msg
  (new message% (parent restart-confirm-window)
       (label "Are you sure you want to restart?")))

(define restart-yes
  (new button%
       [parent restart-confirm-window]
       [label "Yes"]
       ; Callback procedure for a button click:
       [callback (lambda (button event)
                   (send msg-area set-label "The game has been restarted.")
                   (send restart-confirm-window show #f))]))
;(send restart-confirm-window show #t))]))

(define restart-no
  (new button%
       [parent restart-confirm-window]
       [label "No"]
       ; Callback procedure for a button click:
       [callback (lambda (button event)
                   ;(send msg-area set-label "The game has been restarted.")
                   (send restart-confirm-window show #f))]))
