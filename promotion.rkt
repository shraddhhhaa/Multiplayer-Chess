#lang racket
(require racket/gui)
(require (rename-in 2htdp/image (make-pen 2htdp:make-pen) (make-color 2htdp:make-color)))
(provide (all-defined-out))

(define promotion-frame (new frame% [label "Promotion"]
                             [width 340]
                             [height 120]
                             [style '(no-resize-border)]))

(send promotion-frame show #t)

(define prom-canvas%
  (class canvas%
    (define/override (on-event event) (prom-click event))
    (super-new)))

(define prom-canvas (new prom-canvas% [parent promotion-frame]
                         [paint-callback (lambda (canvas dc) (promotion dc))]))

(define (promotion dc)
  (overlay (text "Congratulations, your pawn has been promoted!" 24 "olive") (empty-scene 340 120)))

(define (prom-click event)
  (cond [(eq? (send event get-event-type) 'left-down) (display (cons (send event get-x) (send event get-y))) (newline)]))