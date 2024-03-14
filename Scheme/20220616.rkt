#lang racket

;;Define a list-to-compose pure function, which takes a list containing functions of one argument and
;;returns their composition.
;;E.g. (list-to-compose (list f g h)) is the function f(g(h(x)).

(define (list-to-compose lst)
  (lambda (x)
    (foldr (lambda (y acc)
             (y acc)) x lst)))
 

(define f (lambda (x) (+ 2 x)))
(define g (lambda (x) (* x x)))
(define h (lambda (x) (+ 1 x)))


(display ((list-to-compose (list f g h) ) 1))



        