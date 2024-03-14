#lang racket

;; Exercises on macros

;; 1) Define macro "for" which works like the standard procedure for-each but with conventional syntax
;; E.g. (for x in '(1 2 3) (display x) (newline))

(define-syntax for
  (synta-rules (in)
               ((_ var in lis fun ...)
                (for-each (lambda (var)
                            fun ...) lis))))

;; 2) Define a procedure for computing the cartesian product of two list
;; e.g.
;; (cartesian-product '(a b) '(1 2 3))
;; return -> ((a 1) (a 2) (a 3) (b 1) (b 2) (b 3))

(define (listify x)
  (if (list? x) x (list x)))

(define (cartesian-product x y)
  (let ((val '()))
    (for-each
     (lambda (a)
       (for-each
        (lambda (b)
          (set! val
                (append val(list
                            (append (listify a)
                                    (listify b))))))
        y))
     x)
    val))

;; 3) Define a macro for "list comprehesions", with syntax
;; (list/co <outexpr> <condexpr> for <list1> ...)
;; where <outexpr> and <condespr> are *optional* procedures with one argument.
;; These procedures expect a list containing one argument taken from each list in <list1>...
;; <outexpr> computes the output value to be put in the comprehension, while
;; <condexpr> is a predicate used to filter out unwanted values.

;; Examples:
;;(list/co (lambda (x)
;;           (cons (car x) (cadr x)))
;;         for '(1 2 3) '(a b c))
;;returns
;;((1 . a) (1 . b) (1 . c) (2 . a) (2 . b) (2 . c) (3 . a) (3 . b) (3 . c))

(define-syntax list/co
  (syntax-rules (for)
    ((_ expr for l1)
     (map expr l1))
    ((_ expr test for l1)
     (map expr (filter test l1)))
    ((_ expr for l1 l2 ...)
     (map expr
          (fold-left cartesian-product '(()) (list l1 l2 ...))))
    ((_ expr test for l1 l2 ...)
     (map expr
          (filter test
                  (fold-left cartesian-product '(()) (list l1 l2 ...)))))
    ))

;; 4) Define a better list/co macro, by exploiting for the translation the List Monad seen in Heskell.

;;for example:
;;(monadic/co (cons x y) from x '(1 2 3 4) y '(-1 2 3)) returns
;;((1 . -1) (1 . 2) (1 . 3) (2 . -1) (2 . 2) (2 . 3) (3 . -1) (3 . 2) (3 . 3) (4 . -1) (4 . 2) (4 . 3))
;;(monadic/co (cons x y) when (> x y) from x '(1 2 3 4) y '(-1 2 3)) returns
;;((1 . -1) (2 . -1) (3 . -1) (3 .2) (4 . -1) (4 . 2) (4 . 3))