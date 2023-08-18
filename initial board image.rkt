#lang racket
(require 2htdp/image)
(require 2htdp/universe)
(require "../figures.rkt")

(define background1 (bitmap "f.jpg"))
(define white (bitmap "white.png"))
(define black (bitmap "black.png"))

;--------------------___________FOR LOOP DEFINED___________------------------
(define-syntax for
  (syntax-rules (:)
    [(for init : bool : termin : statements ... )
     (begin init
            (define (iter)
              (cond [bool (begin statements ...
                                 termin
                                 (iter))]))
            (iter))]))

;-----------------------------------------------------------------------------

(define (my-function image)
  (begin (for (define r 0) : (< r 8) : (set! r (+ r 1)) :
           (for (define c 0) : (< c 8) : (set! c (+ c 1)) :
             (set! image (place-image (if (even? (+ r c)) white black) (+ 190 (* r 60)) (+ 90 (* c 60)) image))))
         (save-image image "Board.png")
         image))

(big-bang background1
  [to-draw my-function])

