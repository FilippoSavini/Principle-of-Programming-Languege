#lang racket

;;Consider the following code:
(define (r x y . s)
 (set! s (if (cons? s) (car s) 1))
 (lambda ()
  (if (< x y)
     (let ((z x))
       (set! x (+ s x))
      z)
     y)))
;;1. What can we use r for? Describe how it works and give some useful examples of its usage.
;it's a closure

(define k (r 1 8 . [3]))
(k)
(k)
(k)
(k)
(k)


;;2. It makes sense to create a version of r without the y parameter? If the answer is yes, implement
;;such version; if no, explain why.

(define (r1 x . s)
  (set! s (if (cons? s) (car s) 1))
  (lambda ()
    (let ((z x))
      (set! x (+ s x))
      z)))

(define test (r1 1 . []))
(test)
(test)
(test)
(test)
(test)
(test)

