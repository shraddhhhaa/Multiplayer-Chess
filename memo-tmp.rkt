#lang racket

(define (slow-fact n)
  (if (= n 0) 1
      (* n (slow-fact (- n 1)))))

(define fact-vec (make-vector 100 #f))
(define (memoized-fact n)
  
  (define (init) (vector-set! fact-vec 0 1))
  
  (define (fact n)
    (cond[(number? (vector-ref fact-vec n))
          (vector-ref fact-vec n)]
         [else (begin
                 (vector-set! fact-vec n (* n (fact (- n 1))))
                 (vector-ref fact-vec n))]))
  (init)
  fact)
  




(define (fib n)
   (cond[(= n 0) 0]
        [(= n 1) 1]
        [else 
         (+ (fib (- n 1)) (fib (- n 2)))]))

;Memoization:

(define (memoized-fib range)
  (define fib-vec (make-vector (+ range 1) #f))
  (define (init) (begin 
                   (vector-set! fib-vec 0 0)
                   (vector-set! fib-vec 1 1)))
  (define (fib n)
    (display fib-vec)
    (newline)
    (cond [(number? (vector-ref fib-vec n)) (vector-ref fib-vec  n)]
          [else (begin
                  (vector-set! fib-vec n (+ (fib (- n 1)) (fib (- n 2))))
                  (vector-ref fib-vec n))]))
  (init)
  (fib range))
    


; A memoization of fib in which the vector grows in size
; if required.

(define (memofib n)
  (define fib-vec (make-vector n #\#))
  (define (init) (vector-set! fib-vec 0 0)
                 (vector-set! fib-vec 1 1))
  (define (fib n)
    (begin
      (if (>= n (vector-length fib-vec))
          (set! fib-vec 
                (vector-append fib-vec 
                               (make-vector (+ 1 (- n  (vector-length fib-vec))) #\#)))
           '()) ; THis value does not matter
      (cond ((number? (vector-ref fib-vec n))
             (vector-ref fib-vec n))
            (else (begin
                    (vector-set! fib-vec n (+ (fib (- n 1)) 
                                              (fib (- n 2))))
                    (vector-ref  fib-vec n))))))
  (init)
  fib)






(define (bad-make-2d-vector r c)
  (make-vector r (make-vector c #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-2d-vector r c initial)
  (build-vector r (lambda (x) (make-vector c initial))))

(define (2d-vector-ref vec r c)
  (vector-ref (vector-ref vec r) c))

(define (2d-vector-set! vec r c val)
  (let ((v (vector-ref vec r)))
    (begin
      (vector-set! v c val))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (lcs l1 l2)
  (cond ((or (null? l1) (null? l2)) `())
        ((eq? (car l1) (car l2)) 
         (cons (car l1) (lcs (cdr l1) (cdr l2))))
        (else (let ((res1 (lcs (cdr l1) l2))
                    (res2 (lcs l1 (cdr l2))))
                (if (>= (length res1) (length res2)) res1 res2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define l1 '(A B C B D A B))
(define l2 '(B D C A B A))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (memo-lcs l1 l2)
  (define table (make-2d-vector (length l1) (length l2) #f))
  (define (memo-lcs-h l1 l2)
    (displayln table)
    (let ([n1 (- (length l1) 1)]
          [n2 (- (length l2) 1)])
      (cond [(or (null? l1) (null? l2)) '()]
            [(list? (2d-vector-ref table n1 n2))
             (2d-vector-ref table n1 n2)]
            [else
             (let ([val
                    (cond [(eq? (car l1) (car l2)) 
                           (cons (car l1) (memo-lcs-h
                                           (cdr l1) (cdr l2)))]
                          [else (let ((res1 (memo-lcs-h (cdr l1) l2))
                                      (res2 (memo-lcs-h l1 (cdr l2))))
                                  (if (>= (length res1) (length res2))
                                      res1 res2))])])
               (begin
                 (2d-vector-set! table n1 n2 val)
                 val))])))
    
  (memo-lcs-h l1 l2))


