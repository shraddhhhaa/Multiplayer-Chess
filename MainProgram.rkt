#lang racket
(require 2htdp/image)
(require 2htdp/universe)
(require "figures.rkt")
(require "moves-helper.rkt")
(require "Algorithm.rkt")

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

(define current-player 'white)

(define (change-player) (if (eq? current-player 'white) (set! current-player 'black) (set! current-player 'white)))

(define moves-list #f)
(define clicked-object #f)
(define paused #f)
(define bool-prom #f)
(define stalemate #f)
(define checkmate #f)
(define in-check-w #f)
(define in-check-b #f)
(define exit #f)
(define bool-ai #f)

(define (exit-func img) exit)

;-----------REMEMBER : In dead-list 'white contains dead pieces of black and 'black contain dead pieces of white---------------

(define dead-list (make-hash (list (cons 'white '()) (cons 'black '()))))

(define (promotion 1st-piece 2nd-piece)
  (begin (if (eq? 'black (get-field color 1st-piece))
             (cond [(eq? 2nd-piece 'queen) (begin (set-field! object 1st-piece 'queen)
                                                  (set-field! image 1st-piece (bitmap "./Figures/queen_black.png")))]
                   [(eq? 2nd-piece 'rook) (begin (set-field! object 1st-piece 'rook)
                                                 (set-field! image 1st-piece (bitmap "./Figures/rook_black.png")))]
                   [(eq? 2nd-piece 'bishop) (begin (set-field! object 1st-piece 'bishop)
                                                   (set-field! image 1st-piece (bitmap "./Figures/bishop_black.png")))]
                   [(eq? 2nd-piece 'knight) (begin (set-field! object 1st-piece 'knight)
                                                   (set-field! image 1st-piece (bitmap "./Figures/knight_black.png")))])

             (cond [(eq? 2nd-piece 'queen) (begin (set-field! object 1st-piece 'queen)
                                                  (set-field! image 1st-piece (bitmap "./Figures/queen_white.png")))]
                   [(eq? 2nd-piece 'rook) (begin (set-field! object 1st-piece 'rook)
                                                 (set-field! image 1st-piece (bitmap "./Figures/rook_white.png")))]
                   [(eq? 2nd-piece 'bishop) (begin (set-field! object 1st-piece 'bishop)
                                                   (set-field! image 1st-piece (bitmap "./Figures/bishop_white.png")))]
                   [(eq? 2nd-piece 'knight) (begin (set-field! object 1st-piece 'knight)
                                                   (set-field! image 1st-piece (bitmap "./Figures/knight_white.png")))]))
         (if (check board 'white (get-field pos w-king))
             (set! in-check-w #t)
             (set! in-check-w #f))
         (if (check board 'black (get-field pos b-king))
             (set! in-check-b #t)
             (set! in-check-b #f))))
                       
; What happens when you click on the canvas
(define (mouse-click image x y event)
  (begin (cond [(and 1st-frame (string=? event "button-down"))
                (cond [(and (> x 311) (< x 494) (> y 225) (< y 250))
                       (begin (set! bool-ai #t)
                              (set! 1st-frame #f))]
                      [(and (> x 250) (< x 551) (> y 281) (< y 315))
                       (begin (set! clock-box #t)
                              (set! clock #t)
                              (set! 1st-frame #f))]
                      [(and (> x 320) (< x 480) (> y 345) (< y 370))
                       (set! 1st-frame #f)])]

               [(and (or stalemate checkmate) (string=? event "button-down")) (set! exit #t)]
                
               [(and (string=? event "button-down") (> x 740) (< y 60)) (set! exit #t)]

               [(and (string=? event "button-down") (not bool-prom) (not paused) (> x 160) (> y 60) (< x 640) (< y 540))
                (let ([clicked-square (2d-vector-ref board (quotient (- x 100) 60) (quotient y 60))])
                  (cond [(get-field clicked clicked-square) (begin (cond [(and (eq? current-player 'black) (eq? (get-field pos-y clicked-square) 8) (eq? 'pawn (get-field object clicked-object)))
                                                                          (set! bool-prom clicked-object)]
                                                                         [(and (eq? current-player 'white) (eq? (get-field pos-y clicked-square) 1) (eq? 'pawn (get-field object clicked-object)))
                                                                          (set! bool-prom clicked-object)])
                                                                   ;;;;;;;;;Board history;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                                                   (set! board-history (cons (list
                                                                                              (list clicked-object (get-field pos clicked-object)
                                                                                                    (cons (get-field pos-x clicked-square)
                                                                                                          (get-field pos-y clicked-square))
                                                                                                    bool-prom)
                                                                                              (if (not (eq? (get-field figure clicked-square) no-figure)) (get-field figure clicked-square) #f)
                                                                                              in-check-w in-check-b checkmate stalemate)
                                                                                             board-history))
                                                                   ;;;;;;;;;Board history;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                                                   (if (and (eq? clicked-object 'king)
                                                                            (car (castling board (moves-refine board clicked-object current-player) current-player))
                                                                            (eq? (get-field pos clicked-square)
                                                                                 (cadr  (castling board (moves-refine board clicked-object current-player) current-player)))
                                                                            )
                                                                       (begin (move clicked-object (get-field pos clicked-object)
                                                                         (cons (get-field pos-x clicked-square)
                                                                               (get-field pos-y clicked-square)))
                                                                              (move (cond [(and (eq? current-player 'black) (< (car (get-field pos clicked-object)) 5)) b-rook1]
                                                                                          [(and (eq? current-player 'black) (>= (car (get-field pos clicked-object)) 5)) b-rook2]
                                                                                          [(and (eq? current-player 'white) (< (car (get-field pos clicked-object)) 5)) w-rook1]
                                                                                          [(and (eq? current-player 'white) (>= (car (get-field pos clicked-object)) 5)) w-rook2])
                                                                                    (get-field pos (cond [(and (eq? current-player 'black) (< (car (get-field pos clicked-object)) 5)) b-rook1]
                                                                                          [(and (eq? current-player 'black) (>= (car (get-field pos clicked-object)) 5)) b-rook2]
                                                                                          [(and (eq? current-player 'white) (< (car (get-field pos clicked-object)) 5)) w-rook1]
                                                                                          [(and (eq? current-player 'white) (>= (car (get-field pos clicked-object)) 5)) w-rook2]))
                                                                                    (caddr (castling board (moves-refine board clicked-object current-player) current-player))))
                                                                       (move clicked-object (get-field pos clicked-object)
                                                                         (cons (get-field pos-x clicked-square)
                                                                               (get-field pos-y clicked-square))))
                                                                   (restore-sq moves-list)
                                                                   (set! clicked-object #f)
                                                                   (set! moves-list #f)
                                                                   (if (check board 'white (get-field pos w-king))
                                                                       (set! in-check-w #t)
                                                                       (set! in-check-w #f))
                                                                   (if (check board 'black (get-field pos b-king))
                                                                       (set! in-check-b #t)
                                                                       (set! in-check-b #f))
                                                                   (if bool-ai (begin (let ([ai-ans (ai board 'black 1)])
                                                                                        (move (car ai-ans) (cadr ai-ans) (caddr ai-ans))))
                                                                       (change-player))
                                                                   (cond [(and (null? (refined-all-moves board current-player))
                                                                               (not (check board current-player (get-field pos (if (eq? current-player 'black) b-king w-king)))))
                                                                          (set! stalemate #t)])
                                                                   (cond [(and (null? (refined-all-moves board current-player))
                                                                               (check board current-player (get-field pos (if (eq? current-player 'black) b-king w-king))))
                                                                          (set! checkmate #t)]))]
                        [(and (not (eq? (get-field figure clicked-square) no-figure)) (eq? (get-field color (get-field figure clicked-square)) current-player))
                         (begin (cond [clicked-object (restore-sq moves-list)])
                                (set! clicked-object (get-field figure clicked-square))
                                (set! moves-list (moves-refine board clicked-object current-player))
                                (map (lambda (x) (let [(sq (2d-vector-ref board (car x) (cdr x)))]
                                                   (set-field! clicked sq #t))) moves-list))]))]
               
               [(and (string=? event "button-down") bool-prom)
                (cond [(and (> y 270) (< y 330))
                       (cond [(and (> x 280) (< x 340)) (begin (promotion bool-prom 'rook) (set! bool-prom #f))]
                             [(and (> x 340) (< x 400)) (begin (promotion bool-prom 'bishop) (set! bool-prom #f))]
                             [(and (> x 400) (< x 460)) (begin (promotion bool-prom 'knight) (set! bool-prom #f))]
                             [(and (> x 460) (< x 520)) (begin (promotion bool-prom 'queen) (set! bool-prom #f))])])]
               
               [(string=? event "button-down")
                (if paused (set! paused #f) (cond [(< (+ (* (- x 30) (- x 30)) (* (- y 30) (- y 30))) 900) (set! paused #t)]))])
         image))


(define (restore-sq lst)
  (map (lambda (x) (set-field! clicked (2d-vector-ref board (car x) (cdr x)) #f)) lst))

(define (move obj pos1 pos2)
  (begin (set-field! figure (2d-vector-ref board (car pos1) (cdr pos1)) no-figure)
         (cond [(not (eq? (get-field figure (2d-vector-ref board (car pos2) (cdr pos2))) no-figure))
                (begin (set-field! exists (get-field figure (2d-vector-ref board (car pos2) (cdr pos2))) #f)
                       (hash-set! dead-list current-player (append (hash-ref dead-list current-player) (list (get-field figure (2d-vector-ref board (car pos2) (cdr pos2)))))))])
         (set-field! figure (2d-vector-ref board (car pos2) (cdr pos2)) obj)
         (set-field! pos obj pos2)))


(define board-square%
  (class object%
    (init-field pos-x) ;position on chess board
    (init-field pos-y) ;position on chess board
    (init-field [clicked #f])
    (init-field [figure no-figure])
    (super-new)))


;------------2d-vector functions-----------------

(define (make-2d-vector r c)
  (build-vector r (lambda (x) (make-vector c #f))))

(define (2d-vector-ref board r c)
  (vector-ref (vector-ref board (- r 1)) (- c 1)))

(define (2d-vector-set! board r c val)
  (vector-set! (vector-ref board (- r 1)) (- c 1) val))

;------------2d-vector functions------------------


(define board (make-2d-vector 8 8))

(define (initialise-board)
  (for (define r 1) : (< r 9) : (set! r (+ r 1)) :
    (for (define c 1) : (< c 9) : (set! c (+ c 1)) :
      (begin (2d-vector-set! board r c (new board-square% [pos-x r]
                                            [pos-y c]))))))
        
(initialise-board)

(define (init-squares)
  (set-field! figure (2d-vector-ref board 1 1) b-rook1)
  (set-field! figure (2d-vector-ref board 2 1) b-knight1)
  (set-field! figure (2d-vector-ref board 3 1) b-bishop1)
  (set-field! figure (2d-vector-ref board 4 1) b-queen)
  (set-field! figure (2d-vector-ref board 5 1) b-king)
  (set-field! figure (2d-vector-ref board 6 1) b-bishop2)
  (set-field! figure (2d-vector-ref board 7 1) b-knight2)
  (set-field! figure (2d-vector-ref board 8 1) b-rook2)
  
  (set-field! figure (2d-vector-ref board 1 2) b-pawn1)
  (set-field! figure (2d-vector-ref board 2 2) b-pawn2)
  (set-field! figure (2d-vector-ref board 3 2) b-pawn3)
  (set-field! figure (2d-vector-ref board 4 2) b-pawn4)
  (set-field! figure (2d-vector-ref board 5 2) b-pawn5)
  (set-field! figure (2d-vector-ref board 6 2) b-pawn6)
  (set-field! figure (2d-vector-ref board 7 2) b-pawn7)
  (set-field! figure (2d-vector-ref board 8 2) b-pawn8)

  (set-field! figure (2d-vector-ref board 1 8) w-rook1)
  (set-field! figure (2d-vector-ref board 2 8) w-knight1)
  (set-field! figure (2d-vector-ref board 3 8) w-bishop1)
  (set-field! figure (2d-vector-ref board 4 8) w-queen)
  (set-field! figure (2d-vector-ref board 5 8) w-king)
  (set-field! figure (2d-vector-ref board 6 8) w-bishop2)
  (set-field! figure (2d-vector-ref board 7 8) w-knight2)
  (set-field! figure (2d-vector-ref board 8 8) w-rook2)
  
  (set-field! figure (2d-vector-ref board 1 7) w-pawn1)
  (set-field! figure (2d-vector-ref board 2 7) w-pawn2)
  (set-field! figure (2d-vector-ref board 3 7) w-pawn3)
  (set-field! figure (2d-vector-ref board 4 7) w-pawn4)
  (set-field! figure (2d-vector-ref board 5 7) w-pawn5)
  (set-field! figure (2d-vector-ref board 6 7) w-pawn6)
  (set-field! figure (2d-vector-ref board 7 7) w-pawn7)
  (set-field! figure (2d-vector-ref board 8 7) w-pawn8))
  
(init-squares)


; Paints the chess pieces on board

(define background (bitmap "./Boards/f.jpg"))

(define 1st-frame #t)

(define (print-figures-on-board image)
  (define (which-image sq)
    (cond [(and (get-field clicked sq) (not (eq? no-figure (get-field figure sq)))) (place-image (get-field image (get-field figure sq)) 30 30 danger)]
          [(eq? (get-field figure sq) clicked-object) (place-image (get-field image (get-field figure sq)) 30 30 selected)]
          [(get-field clicked sq) possible_move]
          [(and (eq? (get-field object (get-field figure sq)) 'king) (eq? (get-field color (get-field figure sq)) 'black) in-check-b)
           (place-image (get-field image (get-field figure sq)) 30 30 check-img)]
          [(and (eq? (get-field object (get-field figure sq)) 'king) (eq? (get-field color (get-field figure sq)) 'white) in-check-w)
           (place-image (get-field image (get-field figure sq)) 30 30 check-img)]
          [(even? (+ (get-field pos-x sq) (get-field pos-y sq))) (place-image (get-field image (get-field figure sq)) 30 30 white)]
          [else (place-image (get-field image (get-field figure sq)) 30 30 black)]))
  
  (cond [1st-frame
         (begin
           (set! image 1st-frame-img)
           (set! image (place-image (text "Welcome to IITB-CSE Chess" 40 "lightskyblue") 400 100 image))
           (set! image (place-image (text "Single Player" 30 "yellow") 400 240 image))
           (set! image (place-image (text "Multiplayer with Timer" 30 "red") 400 300 image))
           (set! image (place-image (text "Multiplayer" 30 "darkcyan") 400 360 image))
           image)]
        
        [clock-box
         (begin (set! image (place-image (text "Enter the timer per move you want" 30 "lightskyblue") 400 30 clock-picture))
                (set! image (place-image (text display-time 30 "lightskyblue") 400 570 image))
                image)]
      
        [else (begin (for (define r 1) : (< r 9) : (set! r (+ r 1)) :
                       (for (define c 1) : (< c 9) : (set! c (+ c 1)) :
                         (set! image (place-image (which-image (2d-vector-ref board r c)) (+ 130 (* r 60)) (+ 30 (* c 60)) image))))
         
                     (for (begin (define i 1) (define j 0) (define lst (reverse (hash-ref dead-list 'black)))) : (not (null? lst)) : (set! lst (cdr lst)) :
                       (set! image (place-image (get-field image (car lst)) (+ 670 j) (+ 30 (* i 60)) image))
                       (set! i (+ i 1))
                       (cond [(= i 9) (begin (set! j 60) (set! i 1))]))
                     (for (begin (set! i 1) (set! j 0) (set! lst (reverse (hash-ref dead-list 'white)))) : (not (null? lst)) : (set! lst (cdr lst)) :
                       (set! image (place-image (get-field image (car lst)) (- 130 j) (+ 30 (* i 60)) image))
                       (set! i (+ i 1))
                       (cond [(= i 9) (begin (set! j 60) (set! i 1))]))

                     (cond [bool-prom (begin (set! image (place-image game-paused 400 300 image))
                                             (if (eq? current-player 'white)
                                                 (begin (set! image (place-image (bitmap "./Figures/rook_black.png") 310 300 image))
                                                        (set! image (place-image (bitmap "./Figures/bishop_black.png") 370 300 image))
                                                        (set! image (place-image (bitmap "./Figures/knight_black.png") 430 300 image))
                                                        (set! image (place-image (bitmap "./Figures/queen_black.png") 490 300 image)))
                                                 (begin (set! image (place-image (bitmap "./Figures/rook_white.png") 310 300 image))
                                                        (set! image (place-image (bitmap "./Figures/bishop_white.png") 370 300 image))
                                                        (set! image (place-image (bitmap "./Figures/knight_white.png") 430 300 image))
                                                        (set! image (place-image (bitmap "./Figures/queen_white.png") 490 300 image)))))])

                     (cond [clock (set! image (place-image (text (num-to-string clock-w) 30 "ivory") 160 570
                                                           (place-image (text (num-to-string clock-b) 30 "ivory") 160 30 image)))])
         
                     (cond [stalemate (set! image (place-image stalemate-text 400 30 (place-image stalemate-img 400 300 image)))])
                     (cond [checkmate (begin (set! image (place-image checkmate-img 400 300 image))
                                             (if (eq? current-player 'black)
                                                 (set! image (place-image white-wins 400 30 image))
                                                 (set! image (place-image black-wins 400 30 image))))])

                     (cond [(and (or in-check-w in-check-b) (not (or stalemate checkmate)))
                            (set! image (place-image check-text 400 30 image))])
                                      
                     (if paused (begin (set! image (place-image game-paused 400 300 image))
                                       (set! image (place-image paused-text 400 300 image)))
                         (set! image (place-image pause 30 30 image)))

                     (set! image (place-image exit-img 770 30 image))
                     image)]))

;-------------------------------------------Clock Implementation------------------------------------------------------------------------------------------------------
(define clock-box #f)
(define clock #f)
(define time-val 0)
(define str-val "")
(define display-time "")
(define clock-w 0)
(define clock-b 0)
(define less-than-6 #t)

(define (num-to-string num)
  (string-append (number->string (quotient num 60)) ":" (number->string (remainder num 60))))

(define (enter-timer image key)
  (begin (cond [clock-box (cond [(and (< (string-length str-val) 2)
                                      (or (key=? "0" key) (key=? "1" key) (key=? "2" key) (key=? "3" key) (key=? "4" key) (key=? "5" key) (key=? "6" key) (key=? "7" key) (key=? "8" key) (key=? "9" key)))
                                 (cond [less-than-6 (cond [(< (string->number key) 6)
                                                           (begin (set! display-time (string-append display-time key))
                                                                  (set! str-val (string-append str-val key))
                                                                  (set! less-than-6 #f))])]
                                       [else (begin (set! display-time (string-append display-time key))
                                                    (set! str-val (string-append str-val key)))])]
                                [(key=? ":" key) (begin (set! time-val (* (string->number str-val) 60))
                                                        (set! display-time (string-append display-time key))
                                                        (set! str-val "")
                                                        (set! less-than-6 #t))]
                                [(key=? "\r" key) (begin (set! time-val (if (string->number str-val) (+ (string->number str-val) time-val) 59))
                                                         (set! clock-w time-val)
                                                         (set! clock-b time-val)
                                                         (set! clock-box #f))])]
               [(and (key=? key "z") (not paused)) (undo)])
         image))

(define (clock-func image)
  (begin (cond [(and (not paused) (not checkmate) (not stalemate) clock (not clock-box))
                (if (eq? current-player 'white)
                    (begin (set! clock-w (- clock-w 1))
                           (cond [(<= clock-w 0) (set! checkmate #t)]))
                    (begin (set! clock-b (- clock-b 1))
                           (cond [(<= clock-b 0) (set! checkmate #t)])))])
         image))
;-------------------------------------------Clock Implementation---------------------------------------------------------------------------------------------


;-------------------------------Undo Function---------------------------------

(define board-history
  ;List containing (obj pos1 pos2 bool-prom) (killed-piece) (in-check-w) (in-check-b) (checkmate) (stalemate)
  '())

(define (undo)
  (cond [(not (null? board-history))
         (let [(brd-hstry (car board-history))]
           (begin (move (caar brd-hstry) (caddar brd-hstry) (cadar brd-hstry))
                  (cond [(car (cdddar brd-hstry)) (begin (set-field! object (caar brd-hstry) 'pawn)
                                                         (set-field! image (caar brd-hstry)
                                                                     (if (eq? current-player 'black) (bitmap "./Figures/pawn_white.png") (bitmap "./Figures/pawn_black.png"))))])
                  (change-player)
                  (cond [(cadr brd-hstry) (begin (set-field! exists (cadr brd-hstry) #t)
                                                 (set-field! figure (2d-vector-ref board (car (caddar brd-hstry)) (cdr (caddar brd-hstry))) (cadr brd-hstry))
                                                 (hash-set! dead-list current-player (cdr (hash-ref dead-list current-player))))])
                  (set! in-check-w (caddr brd-hstry))
                  (set! in-check-b (cadddr brd-hstry))
                  (set! checkmate (car (cddddr brd-hstry)))
                  (set! stalemate (cadr (cddddr brd-hstry)))
                  (set! bool-prom #f)
                  (set! board-history (cdr board-history))))]))
;-------------------------------Undo Function---------------------------------

;-----------------------------------Start from user defined position------------------------------------
#|(define (user-defined-board)
  (begin (hash-set! dead-list 'black white-pieces)
         (hash-set! dead-list 'white black-pieces)))
(user-defined-board)|#
      
(big-bang background
  (name "IITB Chess Tournament")
  (on-mouse mouse-click)
  (to-draw print-figures-on-board)
  (on-tick clock-func 1)
  (on-key enter-timer)
  (stop-when exit-func)
  (close-on-stop #t))