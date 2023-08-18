#lang racket
;(require "project.rkt")
(require "moves-helper.rkt")
(require "figures.rkt")
;(require "moveAndFind.rkt")
(provide (all-defined-out))


; TODO Make a function that checks for castling. INPUT:- board. OUTPUT:- #t or #f.
(define (castle curr_board specs)
  (let* [(pos_king (get-field pos specs))
         (b61 #|(get-field figure|# (2d-vector-ref curr_board 6 1));)
         (k71 #|(get-field figure|# (2d-vector-ref curr_board 7 1));)
         (b31 #|(get-field figure|# (2d-vector-ref curr_board 3 1));)
         (k21 #|(get-field figure|# (2d-vector-ref curr_board 2 1));)
         (r11 #|(get-field figure|# (2d-vector-ref curr_board 1 1));)
         (r81 #|(get-field figure|# (2d-vector-ref curr_board 8 1));)
         (q41 #|(get-field figure|# (2d-vector-ref curr_board 4 1))]
    (cond [(not (or (eq? pos_king (cons 5 1)) (eq? pos_king (cons 5 8))))
           #f]
          [(or (not (eq? no-figure b61)) (not (eq? no-figure k71))
                (eq? no-figure r11) (eq? no-figure r81)
                (not (eq? no-figure k21)) (not (eq? no-figure b31))
                (not (eq? no-figure q41))) #f]
          [(or (not (eq? (get-field object r11) 'rook))
               (not (eq? (get-field object r81) 'rook))) #f]
          [else (if (check curr_board 'white (cons 7 1))
                    (if (check curr_board 'white (cons 2 1)) #f
                        (cons 2 1))
                    (cons (cons 2 1) (list (cons 7 1))))])))
          





















;# TODO: Check whether a piece e=will get killed if it moved to a place? INPUT:- board place(s) OUTPUT #t or #f.
;;;However this will a part of the algorithm and will be taken care of on its own!!!!!!!!!!!!!!!!























