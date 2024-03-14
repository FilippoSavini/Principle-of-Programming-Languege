#lang racket

;We want to implement a version of call/cc, called store-cc, where the continuation is called only once and
;it is implicit, i.e. we do not need to pass a variable to the construct to store it. Instead, to run the
;continuation, we can use the associated construct run-cc (which may take parameters). The composition
;of store-cc must be managed using in the standard last-in-first-out approach.
;E.g. if we run:

;we will get:
;here
;1
;2
;and if we call (run-cc)
;we get:
;2
;3
;and the continuation is discarded.

(define store '())

(define-syntax store-cc
  (syntax-rules ()
    ((_ body ...)
     (call/cc
      (lambda (k)
        (set! store (cons k store))
     body ...)))))

(define (run-cc . v)
  (apply (car store) v)
  (set! store (cdr store)))

  
(define (test)
 (define x 0)
 (store-cc
  (displayln "here")
  (set! x (+ 1 x)))
 (displayln x)
 (set! x (+ 1 x))
 x)

(test)
(run-cc)
(run-cc 2)
(test)
