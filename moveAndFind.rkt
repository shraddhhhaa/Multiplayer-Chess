#lang racket

(provide (all-defined-out))
;(require "MainProgram.rkt")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;TODO Make function "move" that makes a given move of a piece on the board INPUT board, start, end.         ;start & end are (cons row col) :: name is (cons color name)
;(define (move curr_board name start end)
;  (let ([piece1 (2d-vector-ref curr_board (car start) (cdr start))]
;        [piece2 (2d-vector-ref curr_board (car end) (cdr end))])
;    
;                                               (2d-vector-set! curr_board (car start) (cdr start) 0)
;                                               (2d-vector-set! curr_board (car end) (cdr end) piece1)))]
;                        [else (begin
;                                (2d-vector-set! curr_board (car start) (cdr start) 0)
;                                (2d-vector-set! curr_board (car end) (cdr end) piece1))])]
;                 [else "Given piece is not at the given starting position"])]
;          [else "There is no piece at the given starting position"])))   ; or returns nothing.



;;;; Here moves function does not check whether the given move is a legal one or not. It is left on SAURAV to tacke care of that SITUATION...

;TODO A function that finds the given piece from the given board returns (cons row col).

;This function converge from both sides simultaneously. piece_name is (cons color name).

;(define (find-this given_board piece)          ;Here the find-this function takes
;  (define row 1)                                    ;4 positions into account for
;  (define count 7)                                  ;searching the given piece!! 
;  (define (helper)
;    (define count1 7)
;    (define col 0)
;    (define rowi (2d-vector-ref given_board row *))
;    (define rowj (2d-vector-ref given_board (+ count row) *))
;    
;    (define (helper1)
;      
;      (define ele (vector-ref rowi col))
;      (define ele1 (vector-ref rowi (+ count1 col)))
;      (define ele2 (vector-ref rowj col))
;      (define ele3 (vector-ref rowj (+ count1 col)))
;             
;      (cond [(= col 4) (begin (set! row (+ row 1))
;                              (set! count (- count 2))
;                              #f)]
;            [(and (piece? ele) (equal? (piece-name ele) (cdr piece_name))
;                  (equal? (piece-color ele) (car piece_name)))
;             (cons row (+ 1 col))]
;            [(and (piece? ele1) (equal? (piece-name ele1) (cdr piece_name))
;                  (equal? (piece-color ele1) (car piece_name)))
;             (cons row (+ 1 col count1))]
;            [(and (piece? ele2) (equal? (piece-name ele2) (cdr piece_name))
;                  (equal? (piece-color ele2) (car piece_name)))
;             (cons (+ count row) (+ 1 col))]
;            [(and (piece? ele3) (equal? (piece-name ele3) (cdr piece_name))
;                  (equal? (piece-color ele3) (car piece_name)))
;             (cons (+ count row) (+ 1 col count1))]
;            [else (begin (set! col (+ col 1))
;                         (set! count1 (- count1 2))
;                         (helper1))]))
;    [let* ([decider (helper1)])
;      (cond [(> row 4) #f]
;            [(not decider) (helper)]
;            [else decider])])
;  (helper))