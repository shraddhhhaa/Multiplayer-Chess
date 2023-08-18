#lang racket

(provide (all-defined-out))

(define-syntax while
  (syntax-rules (:)
    [(while bool :  statements ...)
     (begin (define (iter)
       (cond [bool (begin statements ...
                          (iter))]))
            (iter))]))
;
;(define i 0)
;(while (< i 5) : (display i)
;         (set! i (+ 1 i)))

(define-syntax for-loop
  (syntax-rules (:)
    [(for init : bool : termin : statements ... )
     (begin init
            (define (iter)
              (cond [bool (begin statements ...
                                 termin
                                 (iter))]))
            (iter))]))

;(for (set! i 1) : (< i 5) : (set! i (+ 1 i)) : (newline) (display i))

(define-syntax do-while
  (syntax-rules (:)
    [(do-while statements ... : bool )
     (begin statements ...
            (define (iter)
              (cond [bool (begin statements ...
                                 (iter))]))
            (iter))]))


;(do-while (newline) (display i) (set! i (+ i 1)) : (< i 10))


