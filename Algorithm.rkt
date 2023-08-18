#lang racket
(require "figures.rkt")
(require "moves-helper.rkt")
(provide (all-defined-out))

(define val-table (make-hash))

(hash-set! val-table 'king 200)
(hash-set! val-table 'pawn 1)
(hash-set! val-table 'knight 3)
(hash-set! val-table 'bishop 3)
(hash-set! val-table 'rook 5)
(hash-set! val-table 'queen 9)


(define ref-table (make-hash))

(hash-set! ref-table 'king 6)
(hash-set! ref-table 'pawn 1)
(hash-set! ref-table 'knight 3)
(hash-set! ref-table 'bishop 4)
(hash-set! ref-table 'rook 2)
(hash-set! ref-table 'queen 5)

(define (count-obj obj color board)
  (define ans 0)
  (for (define my-list (if (eq? color 'black) black-pieces white-pieces)) : (not (null? my-list)) : (set! my-list (cdr my-list)) :
    (cond [(and (eq? (get-field object (car my-list)) obj) (get-field exists (car my-list))) (set! ans (+ ans 1))]))
  ans)


;------------2d-vector -----------------


#|(define (make-2d-vector r c init)
  (build-vector r (lambda (x) (make-vector c 0))))

(define (2d-vector-ref board r c)
  (if (equal? c *) (vector-ref board (- r 1))
      (vector-ref (vector-ref board (- r 1)) (- c 1))))

(define (2d-vector-set! board r c val)
  (vector-set! (vector-ref board (- r 1)) (- c 1) val))

(define (find ele lst)
  (define (helper l)
    (cond [(null? l) #f]
          [(equal? ele (car l)) #t]
          [else (helper (cdr l))]))
  (helper lst))

(define (2d-vector-find ele vec)
  (for (define i 1) : ( < i 9) : (set! i (+ i 1)):
    (if (find ele (vector->list (2d-vector-ref vec i *))) #t #f)))|#

;-----------------------------TO BE ADDED TO MAIN FUNCTION----------------------------------
(define in-check-w #f)
(define in-check-b #f)
(define checkmate #f)
(define stalemate #f)

(define black-pieces (list b-rook1 b-knight1 b-bishop1 b-king b-queen b-bishop2 b-knight2 b-rook2 b-pawn1 b-pawn2 b-pawn3 b-pawn4 b-pawn5 b-pawn6 b-pawn7 b-pawn8))
(define white-pieces (list w-rook1 w-knight1 w-bishop1 w-king w-queen w-bishop2 w-knight2 w-rook2 w-pawn1 w-pawn2 w-pawn3 w-pawn4 w-pawn5 w-pawn6 w-pawn7 w-pawn8))

#|(define (count-obj obj color board)
  (define ans 0)
  (for (define my-list (if (eq? color 'black) black-pieces white-pieces)) : (not (null? my-list)) : (set! my-list (cdr my-list)) :
    (cond [(and (eq? (get-field object (car my-list)) obj) (get-field exists (car my-list))) (set! ans (+ ans 1))]))
  ans)|#

(define (fake-move board piece pos1 pos2)
  (begin0 (if (not (eq? no-figure (get-field figure (2d-vector-ref board (car pos2) (cdr pos2))))) (get-field figure (2d-vector-ref board (car pos2) (cdr pos2))) #f)
          (cond [(eq? (get-field object piece) 'king) (set-field! pos piece pos2)])
          (set-field! figure (2d-vector-ref board (car pos1) (cdr pos1)) no-figure)
          (set-field! exists (get-field figure (2d-vector-ref board (car pos2) (cdr pos2))) #f)
          (set-field! figure (2d-vector-ref board (car pos2) (cdr pos2)) piece)))

  
;moves a piece to every possible position given by YASH's MOVE and checks CHECK condition, stores dead piece as #f or piece died in that move and then places it back on board
(define (moves-refine board piece current-player)
  (define ans '())
  (define dead-piece #f)
  (define pos (get-field pos piece))
  (begin (for (define lst (moves board piece)) : (not (null? lst)) : (set! lst (cdr lst)) :
           (set! dead-piece (fake-move board piece pos (car lst)))
           (cond [(not (check board current-player (get-field pos (if (eq? current-player 'black) b-king w-king))))
                  (set! ans (cons (car lst) ans))])
           (fake-move board piece (car lst) pos)
           (cond [dead-piece (begin (set-field! figure (2d-vector-ref board (caar lst) (cdar lst)) dead-piece)
                                    (set-field! exists dead-piece #t)
                                    (set! dead-piece #f))]))
         ans))

;Gives all moves (refined)

(define (refined-all-moves board current-player)
  (define ans '())
  (define pcs-lst (if (eq? current-player 'black) black-pieces white-pieces))
  (begin (for (define i 0) : (not (null? pcs-lst)) : (set! pcs-lst (cdr pcs-lst)) :
           (cond [(get-field exists (car pcs-lst)) (set! ans (append (moves-refine board (car pcs-lst) current-player) ans))]))
         ans))


(define (ai board color n)
  (define which-move #f)
  
  ;List containing (obj pos1 pos2 bool-prom) (killed-piece) (in-check-w) (in-check-b) (checkmate) (stalemate)
  (define ai-history '())

  (define (ai-move obj pos1 pos2)
    (let* ([dead-piece #f]
           [prom-piece #f]
           [fig-pos-2 (get-field figure (2d-vector-ref board (car pos2) (cdr pos2)))])
    
      (begin (set-field! figure (2d-vector-ref board (car pos1) (cdr pos1)) no-figure)
             (cond [(not (eq? fig-pos-2 no-figure))
                    (begin (set-field! exists fig-pos-2 #f)
                           (set! dead-piece fig-pos-2))])
             (set-field! figure (2d-vector-ref board (car pos2) (cdr pos2)) obj)
             (if (check board 'white (get-field pos w-king))
                 (set! in-check-w #t)
                 (set! in-check-w #f))
             (if (check board 'black (get-field pos b-king))
                 (set! in-check-b #t)
                 (set! in-check-b #f))
             (set-field! pos obj pos2)
             (cond [(and (eq? (get-field color obj) 'black) (eq? (cdr pos2) 8) (eq? 'pawn (get-field object obj)))
                    (begin (set! prom-piece obj)
                           (set-field! object obj 'queen))]
                   [(and (eq? (get-field color obj) 'white) (eq? (cdr pos2) 1) (eq? 'pawn (get-field object obj)))
                    (begin (set! prom-piece obj)
                           (set-field! object obj 'queen))])
             (set! ai-history (cons (list
                                     (list obj pos1 pos2 prom-piece) dead-piece in-check-w in-check-b checkmate stalemate)
                                    ai-history)))))

  (define (ai-undo)
    ;(begin (display ai-history) (newline))
    (cond [(not (null? ai-history))
           (let [(brd-hstry (car ai-history))]
             (begin (ai-move (caar brd-hstry) (caddar brd-hstry) (cadar brd-hstry))
                    (cond [(car (cdddar brd-hstry)) (set-field! object (caar brd-hstry) 'pawn)])
                    (cond [(cadr brd-hstry) (begin (set-field! exists (cadr brd-hstry) #t)
                                                   (set-field! figure (2d-vector-ref board (car (caddar brd-hstry)) (cdr (caddar brd-hstry))) (cadr brd-hstry)))])
                    (set! in-check-w (caddr brd-hstry))
                    (set! in-check-b (cadddr brd-hstry))
                    (set! checkmate (car (cddddr brd-hstry)))
                    (set! stalemate (cadr (cddddr brd-hstry)))
                    (set! ai-history (cddr ai-history))))]))
  
  (define (a-b-max a b color n)
    (define alpha a)
    (define score 0)
    (define ans 0)
    (cond [(= n 0) (evaluation board)]
          [else (begin (for (define my-list (if (eq? color 'black) black-pieces white-pieces)) : (not (null? my-list)) : (set! my-list (cdr my-list)) :
                         (cond [(get-field exists (car my-list))
                                (begin (define exit-from-loop #f)
                                       (for (define my-moves (moves-refine board (car my-list) color)) : (and (not exit-from-loop) (not (null? my-moves))) : (set! my-moves (cdr my-moves)) :
                                         (begin (ai-move (car my-list) (get-field pos (car my-list)) (car my-moves))
                                                (set! score (a-b-min alpha b (if (eq? color 'black) 'white 'black) (- n 1)))
                                                (cond [(or #f (>= score b)) (begin (set! exit-from-loop #t) (set! which-move (list (car my-list) (get-field pos (car my-list)) (car my-moves))))]
                                                      [(> score alpha) (set! alpha score)])
                                                (ai-undo)
                                                (if exit-from-loop (set! ans b) (set! ans alpha)))))]))
                       ans)]))

  (define (a-b-min a b color n)
    (define beta b)
    (define score 0)
    (define ans 0)
    (cond [(= n 0) (evaluation board)]
          [else (begin (for (define my-list (if (eq? color 'black) black-pieces white-pieces)) : (not (null? my-list)) : (set! my-list (cdr my-list)) :
                         (cond [(get-field exists (car my-list))
                                (begin (define exit-from-loop #f)
                                       (for (define my-moves (moves-refine board (car my-list) color)) : (and (not exit-from-loop) (not (null? my-moves))) : (set! my-moves (cdr my-moves)) :
                                         (begin (ai-move (car my-list) (get-field pos (car my-list)) (car my-moves))
                                                (set! score (a-b-max a beta (if (eq? color 'black) 'white 'black) (- n 1)))
                                                (cond [(<= score a) (set! exit-from-loop #t)])
                                                (cond [(< score beta) (set! beta score)])
                                                (ai-undo)
                                                (if exit-from-loop (set! ans a) (set! ans beta)))))]))
                       ans)]))

  (begin (a-b-max -2000 2000 'black n) which-move))

;--------------------___________FOR LOOP DEFINED___________------------------
(define-syntax for-loop
  (syntax-rules (:)
    [(for init : bool : termin : statements ... )
     (begin init
            (define (iter)
              (cond [bool (begin statements ...
                                 termin
                                 (iter))]))
            (iter))]))
;=======================================================================

#|(define myboard (make-2d-vector 8 8 0))


(define (S2Y)
  (define all-pieces-b (list b-rook1 b-rook2 b-knight1 b-knight2 b-bishop1 b-bishop2 b-queen b-king
                             b-pawn1 b-pawn2 b-pawn3 b-pawn4 b-pawn5 b-pawn6 b-pawn7 b-pawn8))
  
  (define all-pieces-w (list w-rook1 w-rook2 w-knight1 w-knight2 w-bishop1 w-bishop2 w-queen w-king
                             w-pawn1 w-pawn2 w-pawn3 w-pawn4 w-pawn5 w-pawn6 w-pawn7 w-pawn8))
  
  (define living-b (remove* '(()) (map (lambda (x) (if (get-field exists x) x
                                                       '())) all-pieces-b)))

  (define living-w (remove* '(()) (map (lambda (x) (if (get-field exists x) x
                                                       '())) all-pieces-w)))

  (for-loop (define i 0) : (not (null? living-b)) : (set! living-b (cdr living-b)) :
            (define pos (get-field pos (car living-b)))
            (2d-vector-set! myboard (cdr pos) (car pos) (hash-ref ref-table (get-field object (car living-b)))))

  (for-loop (set! i 0) : (not (null? living-w)) : (set! living-w (cdr living-w)) :
            (define pos (get-field pos (car living-w)))
            (2d-vector-set! myboard (cdr pos) (car pos)
                            (- 0 (hash-ref ref-table (get-field object (car living-w)))))))|#

;;;;;;;;;;;;;;;;;;;;;;;;############################################################################


(define (doubled brd color)
  (define v (if (eq? color 'black) 1 -1))
  (define end (if (eq? color 'black) 8 1))
  (define op (if (eq? color 'black) + -))
  (define count-obj 0)
  (for (define i 1) : (< i 9) : (set! i (+ i 1)) :
    (for (define j 1) : (< j 9) : (set! j (+ j 1)) :
      (cond [(and (not (= end j)) (and (eq? (get-field color (get-field figure (2d-vector-ref brd i j))) (if (> v 0) 'black 'white))
                                       (eq? (get-field object (get-field figure (2d-vector-ref brd i j))) 'pawn)))
             (cond [(and (eq? (get-field color (get-field figure (2d-vector-ref brd i (op j 1)))) (if (> v 0) 'black 'white))
                         (eq? (get-field object (get-field figure (2d-vector-ref brd i (op j 1)))) 'pawn)) (set! count-obj (+ count-obj 1))])])))
  count-obj)

(define (knight-ahead color)
  (define sign (if (eq? color 'black) > <))
  (let [(pos-kn1 (get-field pos (if (eq? color 'black) b-knight1 w-knight1)))
        (pos-kn2 (get-field pos (if (eq? color 'black) b-knight2 w-knight2)))]
    (if (sign (cdr pos-kn1) 4)
        (if (sign (cdr pos-kn2) 4) 4 2)
        0)))

(define (queen-ahead color)
  (define sign (if (eq? color 'black) > <))
  (let [(pos-q (get-field pos (if (eq? color 'black) b-queen w-queen)))]
    (if (sign (cdr pos-q) 4) 2
        0)))


  
(define (evaluation curr_board)
  (let* [(K (count-obj 'king 'black curr_board))
         (K1 (count-obj 'king 'white curr_board))
         (Q (+ (queen-ahead  'black) (count-obj 'queen 'black curr_board)))
         (Q1 (+ (queen-ahead  'white) (count-obj 'queen 'white curr_board)))
         (R (count-obj 'rook 'black curr_board))
         (R1 (count-obj 'rook 'white curr_board))
         (N (+ (knight-ahead 'black) (count-obj 'knight 'black curr_board)))
         (N1 (+ (knight-ahead 'black) (count-obj 'knight 'white curr_board)))
         (B (count-obj 'bishop 'black curr_board))
         (B1 (count-obj 'bishop 'white curr_board))
         (P (count-obj 'pawn 'balck curr_board))
         (P1 (count-obj 'pawn 'white curr_board))
         (M (length (refined-all-moves curr_board 'black))) ;turn is who the current player.
         (M1 (length (refined-all-moves curr_board 'white)))
         (D (doubled board 'black))
         (D1 (doubled board 'white))
         ;(S (count-obj blocked-pawn curr_board))
         ;(I (count-obj isolated-pawn curr_board))
         (Ch (if (check board (get-field pos b-king) 'black) 1 0))
         (Ch1 (if (check board (get-field pos b-king) 'white) 1 0))
         (Chm (if (null? (refined-all-moves curr_board 'black)) 2 0))
         (Chm1 (if (null? (refined-all-moves curr_board 'white)) 2 0));D1 S1 I1 are also defined
         (val (+ (* 100 (- K K1))
                 (* 9 (- Q Q1))
                 (* 5 (- R R1))
                 (* 3 (+ (- B B1) (- N N1)))
                 (* 1 (- P P1))
                 (* 0.5 (- D1 D))
                 (* 0.1 (- M M1))
                 (* 200 (- Ch Ch1))
                 (* 500 (- Chm Chm1))))]
    val))