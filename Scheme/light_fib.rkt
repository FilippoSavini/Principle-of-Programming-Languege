#lang racket
;;light version of fib

(define values (list '()))

(define (add-value idx value)
  (set! values (cons (list idx value) values)))

(define (get-value idx)
  (define (get-tail idx L)
    (let ((x (car L))
          (xs (cdr L)))
      (cond ((null? x) x)
            ((= (car x) idx) (car(cdr x)))
            (else (get-tail idx xs)))))
  (get-tail idx values))
          

(define (fib n)
  (let ((x (get-value n)))
    (cond ((not(null? x)) x)
           ((= n 0) 0)
           ((= n 1) 1)
           (else (let ((x (+ (fib (- n 1)) (fib (- n 2)))))
                   (add-value n x)
                   x)))))

(fib 10)
(newline)

(define fib-series '())

(define (add-elem value)
  (set! fib-series (append fib-series (list value))))

(define (get-elem idx)
  (if (< idx (length fib-series))
      (list-ref fib-series idx)
      '()))

(define (fibonacci n)
  (let ((x (get-elem n)))
    (display fib-series)
    (newline)
    (cond ((not(null? x)) x)
          ((= n 0) 0)
          ((= n 1) 1)
          (else (let ((k (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))
                  (add-elem k)
                  (display k)
                  k)))))
(fibonacci 10)
(newline)
(display fib-series)

    
  