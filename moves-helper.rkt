#lang racket


(require "figures.rkt")
(require "macro.rkt")
(provide (all-defined-out))

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

;------------2d-vector -----------------


(define (make-2d-vector r c)
  (build-vector r (lambda (x) (make-vector c (new square%)))))

(define (2d-vector-ref board r c)
  (vector-ref (vector-ref board (- r 1)) (- c 1)))

(define (2d-vector-set! board r c val)
  (vector-set! (vector-ref board (- r 1)) (- c 1) val))
;'''''''''''''''''''''''''''''''''''''''44'''''''''''''''''''''''''''''''''''''

(define square%
  (class object%
    (init-field [figure no-figure])
    (super-new)))

(define board (make-2d-vector 8 8))


(2d-vector-set! board 1 1 (new square% [figure b-rook1]))
(2d-vector-set! board 8 1 (new square% [figure b-rook2]))
(2d-vector-set! board 1 3 (new square% [figure w-pawn1]))
;(2d-vector-set! board 2 4 (new square% [figure b-pawn4]))
(2d-vector-set! board 5 2 (new square% [figure w-pawn5]))
(2d-vector-set! board 5 1 (new square% [figure b-king]))
;(2d-vector-set! board 7 8 (new square% [figure w-knight2]))
;(send b-rook1 move 0 1)

;(2d-vector-set! board 3 1 b-bishop1)
(2d-vector-set! board 5 8 (new square% [figure w-king]))
(2d-vector-set! board 6 8 (new square% [figure w-bishop2]))
(2d-vector-set! board 5 7 (new square% [figure w-pawn5]))
(2d-vector-set! board 4 7 (new square% [figure w-pawn4]))
(2d-vector-set! board 5 8 (new square% [figure w-queen]))
(2d-vector-set! board 4 8 (new square% [figure w-rook2]))
;(2d-vector-set! board 3 8 (new square% [figure w-bishop1]))
;(2d-vector-set! board 7 1 b-knight2)
;(send b-knight1 move 6 0)

(2d-vector-set! board 2 2 (new square% [figure b-pawn2]))
(2d-vector-set! board 7 7 (new square% [figure w-pawn7]))
(2d-vector-set! board 6 7 (new square% [figure w-pawn6]))
;(2d-vector-set! board 7 8 (new square% [figure w-knight2]))

;# TODO: checks whether the King is in check or not INPUT:- board. Output #t or #f.

(define (find ele lst)
  (define (helper l)
    (cond [(null? l) #f]
          [(equal? ele (car l)) #t]
          [else (helper (cdr l))]))
  (helper lst))

(define (check curr_board color pos)
  (cond [(eq? color 'black)
         (let [(move (all-moves curr_board 'white))]
           (find pos move))]
        [else (let [(move (all-moves curr_board 'black))]
                (find pos move))]))


;TODO Make functions of ROOK BISHOP KNIGHT QUEEN KING PAWN that returns all the possible moves (if possible) of the
;respective piece given the curren_board.  (DONE)

(define (moves curr_board specs)  ;specs if class figure-% having get-field oblect, get-field color, get-field pos.
  
  (let* ([name (get-field object specs)]
         [colour (get-field color specs)])

    (begin
    
      (define (rook)
        (define pos_rook (get-field pos specs))
  
        (define ans '())
        (let* ([row (cdr pos_rook)]          ;higher-order function used...
               [col (car pos_rook)]
               [up (- row 1)]
               [down (- 8 row)]
               [left (- col 1)]
               [right (- 8 col)])
          (begin
            (define (high i op operand end)
              (cond [(> i end) (set! ans ans)]
                          
                    [(equal? operand 'row)
                     (let [(some (get-field figure (2d-vector-ref curr_board col (op row i))))]
                       (if (not (eq? no-figure some)) (if (eq? (get-field color some) colour)
                                                          (set! ans ans)
                                                          (set! ans (cons (cons col (op row i)) ans)))
                                
                           (begin (set! ans (cons (cons col (op row i)) ans))
                                  (high (+ i 1) op operand end))))]
                          
                    [else (let [(some (get-field figure (2d-vector-ref curr_board (op col i) row)))]
                                  
                            (if (not (eq? no-figure some))
                                (if (eq? (get-field color some) colour)
                                    (set! ans ans)
                                    (set! ans (cons (cons (op col i) row) ans)))
                                      
                                (begin (set! ans (cons (cons (op col i) row) ans))
                                                  (high (+ i 1) op operand end))))]))
                
            (high 1 - 'row up)
            (high 1 + 'col right)
            (high 1 + 'row down)
            (high 1 - 'col left)
            ans)))

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (define (king)   ; castling not added
        (define pos_king (get-field pos specs))
        (define ans '())
        (let* ([row (cdr pos_king)]
               [col (car pos_king)]
               [sol (append
                     (rook)
                     (bishop))])
          (begin (map (lambda (x) (if (or (= (cdr x) (+ row 1)) (= (cdr x) (- row 1)))
                                      (set! ans (cons x ans))
                                      (set! ans ans))) sol)
                 (map (lambda (x) (if (or (= (car x) (+ col 1)) (= (car x) (- col 1)))
                                      (set! ans (cons x ans))
                                      (set! ans ans))) sol)
                 (remove-duplicates ans))))

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (define (bishop)
        (define pos_bishop (get-field pos specs))
        (define ans '())
        (let* ([row (cdr pos_bishop)]
               [col (car pos_bishop)]
               [up-left (min (- row 1) (- col 1))]
               [up-right (min (- row 1) (- 8 col))]
               [down-right (min (- 8 row) (- 8 col))]
               [down-left (min (- 8 row) (- col 1))])
          (begin
            (define (high i opr opc end)
              (cond [(> i end) (set! ans ans)]
                          
                    [else (let ([some
                                 (get-field figure (2d-vector-ref curr_board (opc col i) (opr row i)))])
                                  
                            (if (not (eq? no-figure some))
                                (if (eq? (get-field color some) colour)
                                    (set! ans ans)
                                    (set! ans (cons (cons (opc col i) (opr row i)) ans)))
                                
                                (begin (set! ans (cons (cons (opc col i) (opr row i)) ans))
                                       (high (+ i 1) opr opc end))))]))
                  
            (high 1 - - up-left)
            (high 1 - + up-right)
            (high 1 + + down-right)
            (high 1 + - down-left)
            ans)))

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (define (knight)  ; higher-order functions used.df
        (define pos_knight (get-field pos specs))
        (define ans '())
        (let* ([row (cdr pos_knight)]
               [col (car pos_knight)]
               [up (- row 1)]
               [down (- 8 row)]
               [left (- col 1)]
               [right (- 8 col)])
          (begin
            (define (high i opr opc endr endc)
              (define i1 ((lambda (x) (if (even? x) 1 2)) i))
              (cond [(> i 2) (set! ans ans)]
                        
                    [(or (> i endr) (> i1 endc)) (high (+ i 1) opr opc endr endc)]
                        
                    [else (let [(some
                                 (get-field figure (2d-vector-ref curr_board (opc col i1) (opr row i))))]
                            (if (not (eq? no-figure some))
                                (if (equal? (get-field color some) colour)
                                    (high (+ i 1) opr opc endr endc)
                                    (begin (set! ans (cons (cons (opc col i1) (opr row i)) ans))
                                           (high (+ i 1) opr opc endr endc)))
                                    
                                (begin (set! ans (cons (cons (opc col i1) (opr row i)) ans))
                                       (high (+ i 1) opr opc endr endc))))]))
                
            (high 1 - - up left)
            (high 1 - + up right)
            (high 1 + - down left)
            (high 1 + + down right)))
        ans)

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (define (pawn)
        (define pos_pawn (get-field pos specs))
        (define ans '())
        (let* ([row (cdr pos_pawn)]  ;  en-pass not added!!!!!!!!!!!
               [col (car pos_pawn)]
               [left (> (- col 1) 0)]
               [right (> (- 8 col) 0)]
               [colour (get-field color specs)])
        
    
          (begin
            (define (high i end op start) ; op is + for black and - for white start is 2 for black and 7 for white
              (cond [(or (> i end) (> i 2)) (set! ans ans)]
                    [else (let ([some (get-field figure (2d-vector-ref curr_board col (op row i)))])
                            
                            (if (not (eq? no-figure some))  (set! ans ans)
                                (if (= row start)
                                    (begin (set! ans (cons (cons col (op row i)) ans))
                                                      (high (+ i 1) end op (+ 1 start)))
                                    (set! ans (cons (cons col (op row i)) ans)))))]))

            (define (one op) ;op is + for black and - for white.
              (cond [(or (< (- col 1) 1) ( > (op row 1) 8 ) (< (op row 1) 1)) (set! ans ans)]
                    [left (let ([some (get-field figure (2d-vector-ref curr_board (- col 1) (op row 1)))])
                            (if (and (not (eq? no-figure some))
                                     (not (eq? colour (get-field color some))))
                                (set! ans (cons (cons (- col 1) (op row 1)) ans))
                                (set! ans ans)))]))

            (define (two op) ; op is + black - for white.
              (cond [(or (> (+ col 1) 8) ( > (op row 1) 8 ) (< (op row 1) 1)) (set! ans ans)]
                    [right (let ([some
                                  (get-field figure (2d-vector-ref curr_board (+ col 1) (op row 1)))])
                             (if (and (not (eq? no-figure some))
                                      (not (eq? colour (get-field color some))))
                                 (set! ans (cons (cons (+ col 1) (op row 1)) ans))
                                 (set! ans ans)))]))


            (define op1 (if (eq? colour 'black) + -))
            (define fwrd (if (eq? colour 'black) (- 8 row) (- row 1)))
            (define st (if (eq? colour 'black) 2 7))

            (high 1 fwrd op1 st)
            (one op1) (two op1)
            ans)))

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (define (queen)  ; class figure-class% having object
        (append (rook) (bishop)))
    
      (cond [(eq? name 'rook) (rook)]
            [(eq? name 'knight) (knight)]
            [(eq? name 'bishop) (bishop)]
            [(eq? name 'queen) (queen)]
            [(eq? name 'king) (king)]
            [(eq? name 'pawn) (pawn)]))))


(define (all-moves curr_board colour)
  (define all-pieces-b (list b-rook1 b-rook2 b-knight1 b-knight2 b-bishop1 b-bishop2 b-queen b-king
                             b-pawn1 b-pawn2 b-pawn3 b-pawn4 b-pawn5 b-pawn6 b-pawn7 b-pawn8))

  (define all-pieces-w (list w-rook1 w-rook2 w-knight1 w-knight2 w-bishop1 w-bishop2 w-queen w-king
                             w-pawn1 w-pawn2 w-pawn3 w-pawn4 w-pawn5 w-pawn6 w-pawn7 w-pawn8))
  
  (define ans '())
  (cond [(eq? colour 'black)
         (let [(living (remove* '(()) (map (lambda (x) (if (get-field exists x) x
                                                           '())) all-pieces-b)))]
           (set! ans (append* (map (lambda (x) (moves curr_board x)) living))))]
        [else (let [(living (remove* '(()) (map (lambda (x) (if (get-field exists x) x
                                                                '())) all-pieces-w)))]
                (set! ans (append* (map (lambda (x) (moves curr_board x)) living))))])
  (remove-duplicates ans))


(define (castling brd till-now turn)
  (define row1 (if (eq? turn 'black) 1 8))
  (let* [(D (find (cons 4 row1) till-now))
         (C (equal? (get-field figure (2d-vector-ref brd 3 row1)) no-figure))
         (B (equal? (get-field figure (2d-vector-ref brd 2 row1)) no-figure))
         (A (equal? (get-field object (get-field figure (2d-vector-ref brd 1 row1))) 'rook))
         (E (equal? (get-field pos (if (= 1 row1) b-king w-king)) (cons 5 row1)))
         (F (find (cons 6 row1) till-now))
         (G (equal? (get-field figure (2d-vector-ref brd 7 row1)) no-figure))
         (H (equal? (get-field object (get-field figure (2d-vector-ref brd 8 row1))) 'rook))]
    (begin 
      (cond [(and A B C D E ) (set! till-now (cons (cons 2 row1) till-now))])
      (cond [(and E F G H ) (set! till-now (cons (cons 7 row1) till-now))]))
    till-now))
