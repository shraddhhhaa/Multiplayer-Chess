#lang racket
(require "./Figures.rkt");-----------***************-----------------


(define my-frame (new frame% [label "Chess Project"]
                      [width 600]
                      [height 620]
                      [style '(no-resize-border)]))

(send my-frame show #t)

(define my-canvas%
  (class canvas%
    (define/override (on-event event)
      (cond [(eq? (send event get-event-type) 'left-down) (mouse-click (send event get-x) (send event get-y))]))
    (super-new)))

(define (mouse-click a b);--------------------/******************/---------------------
  (display (list a b)))

(define (painting-func dc)
  (define (helper obj)
    (send dc draw-bitmap (get-field image obj) (* board-size (/ 1 8) (get-field x obj)) (* board-size (/ 1 8) (get-field y obj))))
  
  (map helper (list b-rook1 b-knight1 b-bishop1 b-queen b-king b-bishop2 b-knight2 b-rook2 b-pawn1 b-pawn2 b-pawn3 b-pawn4 b-pawn5 b-pawn6 b-pawn7 b-pawn8
                    w-rook1 w-knight1 w-bishop1 w-queen w-king w-bishop2 w-knight2 w-rook2 w-pawn1 w-pawn2 w-pawn3 w-pawn4 w-pawn5 w-pawn6 w-pawn7 w-pawn8)));--------/*******/-----------

(define my-canvas (new my-canvas% [parent my-frame]
                       [paint-callback (lambda (canvas dc) (begin (send dc draw-bitmap (make-object bitmap% "./Boards/black.png") 50 50);----****----
                                                                  (painting-func dc)))]))

;(define my-dc (new bitmap-dc% ;[parent my-canvas]
 ;                  [bitmap (make-object bitmap% "./Figures/black_pawn.png")]))

